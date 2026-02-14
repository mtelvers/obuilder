open Lwt.Infix
open Sexplib.Conv

include S.Sandbox_default

let ( / ) = Filename.concat

type t = {
  ctr_path : string;
  hcn_namespace_path : string;
}

type config = {
  ctr_path : string;
  hcn_namespace_path : string;
} [@@deriving sexp]

let next_id = ref (int_of_float (Unix.gettimeofday () *. 100.) mod 1_000_000)


let read_layerinfo results_dir =
  let path = results_dir / "layerinfo.json" in
  Log.info (fun f -> f "hcs_sandbox: looking for layerinfo at %s" path);
  if Sys.file_exists path then begin
    Log.info (fun f -> f "hcs_sandbox: found layerinfo.json");
    let contents =
      let ic = open_in_bin path in
      Fun.protect ~finally:(fun () -> close_in ic) @@ fun () ->
      let len = in_channel_length ic in
      Log.info (fun f -> f "hcs_sandbox: layerinfo.json length=%d, pos=%d" len (pos_in ic));
      seek_in ic 0;  (* Ensure we're at the beginning *)
      let buf = Bytes.create len in
      let rec read_all pos =
        if pos >= len then ()
        else begin
          let n = input ic buf pos (len - pos) in
          if n = 0 then failwith "Unexpected EOF while reading layerinfo.json"
          else read_all (pos + n)
        end
      in
      read_all 0;
      Bytes.to_string buf
    in
    Log.info (fun f -> f "hcs_sandbox: read %d bytes, parsing layerinfo.json" (String.length contents));
    let json = Yojson.Safe.from_string contents in
    let open Yojson.Safe.Util in
    let source = json |> member "source" |> to_string in
    let parent_layer_paths = json |> member "parent_layer_paths" |> to_list |> List.map to_string in
    Log.info (fun f -> f "hcs_sandbox: source=%s, parents=%d" source (List.length parent_layer_paths));
    Some (source, parent_layer_paths)
  end else begin
    Log.info (fun f -> f "hcs_sandbox: layerinfo.json not found at %s" path);
    None
  end

module Json_config = struct
  let strings xs = `List (List.map (fun x -> `String x) xs)

  let make {Config.cwd; argv; hostname; user; env; mounts; network; mount_secrets = _; entrypoint = _}
      ~layer_folders ~network_namespace : Yojson.Safe.t =
    let username =
      match user with
      | `Windows { Obuilder_spec.name } -> name
      | `Unix _ -> "ContainerAdministrator"
    in
    let windows_section =
      let base = [
        "layerFolders", `List (List.map (fun p -> `String p) layer_folders);
        "ignoreFlushesDuringBoot", `Bool true;
      ] in
      match network, network_namespace with
      | ["host"], Some ns ->
        base @ [
          "network", `Assoc [
            "allowUnqualifiedDNSQuery", `Bool true;
            "networkNamespace", `String ns;
          ];
        ]
      | _ -> base
    in
    let oci_mounts = List.map (fun { Config.Mount.src; dst; readonly; ty = _ } ->
      `Assoc [
        "destination", `String dst;
        "type", `String "bind";
        "source", `String src;
        "options", `List (
          (if readonly then [`String "ro"] else [`String "rw"]) @
          [`String "rbind"; `String "rprivate"]
        );
      ]
    ) mounts in
    `Assoc ([
      "ociVersion", `String "1.1.0";
      "process", `Assoc [
        "terminal", `Bool false;
        "user", `Assoc [
          "username", `String username;
        ];
        "args", strings argv;
        "env", strings (List.map (fun (k, v) -> Printf.sprintf "%s=%s" k v) env);
        "cwd", `String cwd;
      ];
      "root", `Assoc [
        "path", `String "";
        "readonly", `Bool false;
      ];
      "hostname", `String hostname;
      "windows", `Assoc windows_section;
    ] @
      (if oci_mounts <> [] then ["mounts", `List oci_mounts] else [])
    )
end

let run ~cancelled ?stdin:stdin ~log (t : t) config results_dir =
  let pp f = Os.pp_cmd f ("", config.Config.argv) in
  let container_id = Printf.sprintf "obuilder-run-%d" !next_id in
  incr next_id;
  (* Read the layer info from layerinfo.json *)
  let layer_folders = match read_layerinfo results_dir with
    | Some (source, parent_layer_paths) ->
      (* layerFolders = parent layers @ [writable scratch layer] *)
      parent_layer_paths @ [source]
    | None -> Fmt.failwith "No layerinfo.json found in %s" results_dir
  in
  (* Create HCN namespace for networking if requested *)
  let use_network = config.Config.network = ["host"] in
  (if use_network && Sys.win32 then begin
    Log.info (fun f -> f "hcs_sandbox: creating HCN namespace for networking");
    (Os.win32_pread [t.hcn_namespace_path; "create"] >>= function
     | Ok output -> Lwt.return output
     | Error (`Msg m) -> Fmt.failwith "Failed to create HCN namespace: %s" m) >>= fun output ->
    let ns = String.trim output in
    Log.info (fun f -> f "hcs_sandbox: created HCN namespace %s" ns);
    Lwt.return (Some ns)
  end else Lwt.return_none) >>= fun network_namespace ->
  Lwt.finalize (fun () ->
  Lwt_io.with_temp_dir ~perm:0o700 ~prefix:"obuilder-hcs-" @@ fun tmp ->
  (* Generate OCI config.json *)
  let json_config = Json_config.make config ~layer_folders ~network_namespace in
  let json_str = Yojson.Safe.pretty_to_string json_config ^ "\n" in
  Log.info (fun f -> f "hcs_sandbox: OCI config.json:\n%s" json_str);
  Os.write_file ~path:(tmp / "config.json") json_str >>= fun () ->
  (* Write secrets *)
  Lwt_list.iteri_s
    (fun id Config.Secret.{value; _} ->
      let secret_dir = tmp / "secrets" / string_of_int id in
      Os.ensure_dir (tmp / "secrets");
      Os.ensure_dir secret_dir;
      Os.write_file ~path:(secret_dir / "secret") value
    ) config.mount_secrets
  >>= fun () ->
  (* Build the ctr run command *)
  let cmd = [t.ctr_path; "run"; "--rm"] @
            (if Option.is_some network_namespace then ["--cni"] else []) @
            ["--config"; tmp / "config.json";
             container_id] in
  if Sys.win32 then begin
    (* On Windows, use temp file for output to avoid Lwt I/O issues *)
    let output_file = tmp / "output.log" in
    Log.info (fun f -> f "hcs_sandbox: creating output file %s" output_file);
    let out_fd = Unix.openfile output_file [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o600 in
    Lwt.catch (fun () ->
      let argv = Array.of_list cmd in
      let prog = List.hd cmd in
      let stdin_fd = match stdin with
        | Some fd ->
          fd.Os.needs_close <- false;  (* Sandbox takes ownership *)
          fd.Os.raw
        | None -> Unix.openfile "NUL" [Unix.O_RDONLY] 0
      in
      Log.info (fun f -> f "hcs_sandbox: creating process %s" prog);
      let pid = Unix.create_process prog argv stdin_fd out_fd out_fd in
      Unix.close stdin_fd;
      Unix.close out_fd;
      Log.info (fun f -> f "hcs_sandbox: started container process %d" pid);
      Lwt.on_termination cancelled (fun () ->
        let aux () =
          let pp f = Fmt.pf f "ctr task kill %S" container_id in
          Os.exec_result [t.ctr_path; "task"; "kill"; "-s"; "SIGKILL"; container_id] ~pp >>= fun _ ->
          Lwt.return_unit
        in
        Lwt.async aux
      );
      Log.info (fun f -> f "hcs_sandbox: waiting for process %d" pid);
      Os.win32_poll_waitpid ~sleep_interval:1.0 pid >>= fun status ->
      Log.info (fun f -> f "hcs_sandbox: process exited, status=%s"
        (match status with
         | Unix.WEXITED n -> Printf.sprintf "exited(%d)" n
         | Unix.WSIGNALED n -> Printf.sprintf "signaled(%d)" n
         | Unix.WSTOPPED n -> Printf.sprintf "stopped(%d)" n));
      (* Copy output to log *)
      Log.info (fun f -> f "hcs_sandbox: reading output from %s" output_file);
      let output =
        if Sys.file_exists output_file then begin
          try
            (* Use binary mode and explicit seek to handle Windows file buffering *)
            let ic = open_in_bin output_file in
            Fun.protect ~finally:(fun () -> close_in ic) @@ fun () ->
            seek_in ic 0;  (* Ensure we're at the beginning *)
            let len = in_channel_length ic in
            Log.info (fun f -> f "hcs_sandbox: output file length=%d" len);
            if len = 0 then ""
            else really_input_string ic len
          with
          | End_of_file ->
            Log.info (fun f -> f "hcs_sandbox: got EOF reading output file (maybe empty)");
            ""
          | exn ->
            Log.warn (fun f -> f "hcs_sandbox: failed to read output: %s" (Printexc.to_string exn));
            ""
        end else begin
          Log.info (fun f -> f "hcs_sandbox: output file does not exist");
          ""
        end
      in
      Log.info (fun f -> f "hcs_sandbox: read %d bytes of output" (String.length output));
      Log.info (fun f -> f "hcs_sandbox: writing to build log");
      Build_log.printf log "%s" output >>= fun () ->
      Log.info (fun f -> f "hcs_sandbox: unlinking output file");
      (try Unix.unlink output_file with _ -> ());
      Log.info (fun f -> f "hcs_sandbox: returning result");
      match status with
      | Unix.WEXITED 0 ->
        if Lwt.is_sleeping cancelled then Lwt.return (Ok () :> (unit, [`Msg of string | `Cancelled]) result)
        else Lwt_result.fail `Cancelled
      | Unix.WEXITED n ->
        Lwt.return (Fmt.error_msg "%t failed with exit status %d" pp n :> (unit, [`Msg of string | `Cancelled]) result)
      | Unix.WSIGNALED n ->
        Lwt.return (Fmt.error_msg "%t killed by signal %d" pp n :> (unit, [`Msg of string | `Cancelled]) result)
      | Unix.WSTOPPED n ->
        Lwt.return (Fmt.error_msg "%t stopped by signal %d" pp n :> (unit, [`Msg of string | `Cancelled]) result)
    ) (fun exn ->
      Log.warn (fun f -> f "hcs_sandbox: exception in Windows path: %s" (Printexc.to_string exn));
      (try Unix.close out_fd with _ -> ());
      (try Unix.unlink output_file with _ -> ());
      Lwt.fail exn
    )
  end else begin
    (* Unix path - use pipes *)
    Os.with_pipe_from_child @@ fun ~r:out_r ~w:out_w ->
    let stdout = `FD_move_safely out_w in
    let stderr = stdout in
    let copy_log = Build_log.copy ~src:out_r ~dst:log in
    let proc =
      let stdin = Option.map (fun x -> `FD_move_safely x) stdin in
      Os.exec_result ?stdin ~stdout ~stderr ~pp cmd
    in
    Lwt.on_termination cancelled (fun () ->
        let aux () =
          if Lwt.is_sleeping proc then (
            let pp f = Fmt.pf f "ctr task kill %S" container_id in
            Os.exec_result [t.ctr_path; "task"; "kill"; "-s"; "SIGKILL"; container_id] ~pp >>= fun _ ->
            Lwt.return_unit
          ) else Lwt.return_unit
        in
        Lwt.async aux
      );
    proc >>= fun r ->
    copy_log >>= fun () ->
    if Lwt.is_sleeping cancelled then Lwt.return (r :> (unit, [`Msg of string | `Cancelled]) result)
    else Lwt_result.fail `Cancelled
  end
  ) (fun () ->
    (* Clean up HCN namespace if we created one *)
    match network_namespace with
    | Some ns ->
      Log.info (fun f -> f "hcs_sandbox: deleting HCN namespace %s" ns);
      Os.win32_pread [t.hcn_namespace_path; "delete"; ns] >>= fun result ->
      (match result with
       | Ok _ -> ()
       | Error (`Msg m) -> Log.warn (fun f -> f "hcs_sandbox: failed to delete HCN namespace %s: %s" ns m));
      Lwt.return_unit
    | None -> Lwt.return_unit
  )

let create ~state_dir:_ (c : config) : t Lwt.t =
  Lwt.return ({ ctr_path = c.ctr_path; hcn_namespace_path = c.hcn_namespace_path } : t)

let shell _ = [{|C:\cygwin64\bin\bash.exe|}; "-lc"]

let tar _ = ["tar"; "-xf"; "-"]

open Cmdliner

let docs = "HCS SANDBOX"

let ctr_path =
  Arg.value @@
  Arg.opt Arg.string "ctr" @@
  Arg.info ~docs
    ~doc:"Path to the ctr (containerd) CLI."
    ~docv:"CTR_PATH"
    ["hcs-ctr-path"]

let hcn_namespace_path =
  Arg.value @@
  Arg.opt Arg.string "hcn-namespace" @@
  Arg.info ~docs
    ~doc:"Path to the hcn-namespace tool for Windows container networking."
    ~docv:"HCN_NAMESPACE_PATH"
    ["hcs-hcn-namespace-path"]

let cmdliner : config Term.t =
  let make ctr_path hcn_namespace_path =
    { ctr_path; hcn_namespace_path }
  in
  Term.(const make $ ctr_path $ hcn_namespace_path)
