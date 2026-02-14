open Lwt.Infix

let strf = Printf.sprintf

type cache = {
  lock : Lwt_mutex.t;
  mutable gen : int;
}

type t = {
  root : string;
  caches : (string, cache) Hashtbl.t;
  mutable next : int;
}

let ( / ) = Filename.concat

module Path = struct
  let result t id        = t.root / "result" / id
  let result_tmp t id    = t.root / "result-tmp" / id
  let state t            = t.root / "state"
  let cache t name       = t.root / "cache" / Escape.cache name
  let cache_tmp t i name = t.root / "cache-tmp" / strf "%d-%s" i (Escape.cache name)
end

module Ctr = struct
  let ctr_with_output args =
    if Sys.win32 then
      Os.win32_pread ("ctr" :: args)
    else begin
      let pp f = Os.pp_cmd f ("", "ctr" :: args) in
      Os.pread_all ~pp ("ctr" :: args) >>= fun (exit_code, stdout, stderr) ->
      if exit_code = 0 then
        Lwt_result.return stdout
      else begin
        Log.warn (fun f -> f "ctr %s failed (exit %d): stdout=%s stderr=%s"
          (String.concat " " args) exit_code stdout stderr);
        Lwt.return (Fmt.error_msg "ctr %s failed with exit status %d: %s"
          (String.concat " " args) exit_code stderr)
      end
    end

  let ctr args =
    ctr_with_output args >|= Result.map (fun _ -> ())

  let ctr_pread args =
    ctr_with_output args

  (* Prepare a writable snapshot from an optional parent.
     Uses --mounts to get JSON mount info in the output. *)
  let snapshot_prepare ~key ?parent () =
    let parent_args = match parent with
      | Some p -> [p]
      | None -> []
    in
    ctr_pread (["snapshot"; "prepare"; "--mounts"; key] @ parent_args)

  let snapshot_commit ~key ~committed_key () =
    ctr (["snapshot"; "commit"; committed_key; key])

  let snapshot_rm ~key () =
    ctr (["snapshot"; "rm"; key])

  let image_pull image =
    ctr (["image"; "pull"; image])
end

let snapshot_key id = "obuilder-" ^ id

let layerinfo_path dir = dir / "layerinfo.json"

let write_layerinfo ~dir ~snapshot_key ~source ~parent_layer_paths =
  let json = `Assoc [
    "snapshot_key", `String snapshot_key;
    "source", `String source;
    "parent_layer_paths", `List (List.map (fun p -> `String p) parent_layer_paths);
  ] in
  Os.write_file ~path:(layerinfo_path dir) (Yojson.Safe.pretty_to_string json ^ "\n")

let read_layerinfo dir =
  let path = layerinfo_path dir in
  Log.info (fun f -> f "read_layerinfo: opening %s" path);
  let contents =
    let ic = open_in_bin path in
    Fun.protect ~finally:(fun () -> close_in ic) @@ fun () ->
    let len = in_channel_length ic in
    Log.info (fun f -> f "read_layerinfo: file length = %d" len);
    let s = really_input_string ic len in
    Log.info (fun f -> f "read_layerinfo: read %d bytes" (String.length s));
    s
  in
  Log.info (fun f -> f "read_layerinfo: parsing JSON");
  let json = Yojson.Safe.from_string contents in
  let open Yojson.Safe.Util in
  let sk = json |> member "snapshot_key" |> to_string in
  let source = json |> member "source" |> to_string in
  let parent_layer_paths = json |> member "parent_layer_paths" |> to_list |> List.map to_string in
  Log.info (fun f -> f "read_layerinfo: done, key=%s" sk);
  (sk, source, parent_layer_paths)

(* Parse the JSON output of `ctr snapshot prepare --mounts <key> [<parent>]`.
   Format:
   [{"Type":"windows-layer","Source":"C:\\...\\snapshots\\N","Target":"",
     "Options":["rw","parentLayerPaths=[\"C:\\\\...\\\\snapshots\\\\M\"]"]}]
   Returns (source_path, parent_layer_paths). *)
let parse_mount_json output =
  try
    let json = Yojson.Safe.from_string (String.trim output) in
    let open Yojson.Safe.Util in
    match to_list json with
    | [] -> ("", [])
    | mount :: _ ->
      let source = mount |> member "Source" |> to_string in
      let options = mount |> member "Options" |> to_list |> List.map to_string in
      let parents =
        List.find_map (fun opt ->
          match Astring.String.cut ~sep:"parentLayerPaths=" opt with
          | Some (_, json_str) ->
            (try
               let arr = Yojson.Safe.from_string json_str in
               Some (to_list arr |> List.map to_string)
             with _ -> None)
          | None -> None
        ) options
        |> Option.value ~default:[]
      in
      (source, parents)
  with _ -> ("", [])

let delete t id =
  let path = Path.result t id in
  match Os.check_dir path with
  | `Missing -> Lwt.return_unit
  | `Present ->
    (* Read the actual snapshot key from layerinfo.json.
       The key may differ from the default "obuilder-<id>" for base images
       which use "obuilder-base-<hash>". *)
    let rootfs = path / "rootfs" in
    let key =
      if Sys.file_exists (layerinfo_path rootfs) then
        let (sk, _, _) = read_layerinfo rootfs in sk
      else if Sys.file_exists (layerinfo_path path) then
        let (sk, _, _) = read_layerinfo path in sk
      else
        snapshot_key id  (* fallback to constructed key *)
    in
    Log.info (fun f -> f "Deleting snapshot %s for result %s" key id);
    (Ctr.snapshot_rm ~key () >>= function
     | Ok () -> Lwt.return_unit
     | Error (`Msg m) ->
       Log.warn (fun f -> f "Failed to remove snapshot %s: %s" key m);
       Lwt.return_unit) >>= fun () ->
    (* Also try to remove the committed snapshot *)
    let committed_key = key ^ "-committed" in
    (Ctr.snapshot_rm ~key:committed_key () >>= function
     | Ok () -> Lwt.return_unit
     | Error (`Msg _) -> Lwt.return_unit) >>= fun () ->
    Os.rm ~directory:path

let purge path =
  Sys.readdir path |> Array.to_list |> Lwt_list.iter_s (fun item ->
      let item = path / item in
      Log.warn (fun f -> f "Removing left-over temporary item %S" item);
      Os.rm ~directory:item
    )

let root t = t.root

let df t = Lwt.return (Os.free_space_percent t.root)

let create ~root =
  Os.ensure_dir root;
  Os.ensure_dir (root / "result");
  Os.ensure_dir (root / "result-tmp");
  Os.ensure_dir (root / "state");
  Os.ensure_dir (root / "cache");
  Os.ensure_dir (root / "cache-tmp");
  purge (root / "result-tmp") >>= fun () ->
  purge (root / "cache-tmp") >>= fun () ->
  Lwt.return { root; caches = Hashtbl.create 10; next = 0 }

let build t ?base ~id fn =
  let result = Path.result t id in
  let result_tmp = Path.result_tmp t id in
  assert (not (Sys.file_exists result));
  let key = snapshot_key id in
  begin match base with
    | None ->
      (* No parent — this is a base image import.
         The fetcher (fn) will handle snapshot preparation and write layerinfo.json.
         We just need to create the result_tmp directory. *)
      Os.ensure_dir result_tmp;
      Lwt.return_unit
    | Some base_id ->
      (* Build step with a parent — prepare a snapshot from the parent's committed snapshot.
         Read the actual snapshot key from the parent's layerinfo.json. *)
      let parent_dir = Path.result t base_id in
      let parent_rootfs = parent_dir / "rootfs" in
      let parent_key =
        if Sys.file_exists (layerinfo_path parent_rootfs) then
          let (sk, _, _) = read_layerinfo parent_rootfs in sk
        else if Sys.file_exists (layerinfo_path parent_dir) then
          let (sk, _, _) = read_layerinfo parent_dir in sk
        else
          snapshot_key base_id  (* fallback to constructed key *)
      in
      let parent = parent_key ^ "-committed" in
      (* Clean up any existing snapshot with this key (for idempotency) *)
      (Ctr.snapshot_rm ~key () >>= function
       | Ok () -> Log.info (fun f -> f "Removed existing snapshot %s" key); Lwt.return_unit
       | Error _ -> Lwt.return_unit) >>= fun () ->
      Log.info (fun f -> f "Preparing snapshot from parent %s" parent);
      Ctr.snapshot_prepare ~key ~parent () >>= function
      | Ok mounts_json ->
        let source, parent_layer_paths = parse_mount_json mounts_json in
        Os.ensure_dir result_tmp;
        write_layerinfo ~dir:result_tmp ~snapshot_key:key ~source ~parent_layer_paths
      | Error (`Msg m) ->
        Fmt.failwith "Failed to prepare snapshot %s: %s" key m
  end
  >>= fun () ->
  Lwt.try_bind
    (fun () -> fn result_tmp)
    (fun r ->
       begin match r with
         | Ok () ->
           (* Read the snapshot key from layerinfo.json — may have been written
              by the fetcher (in rootfs/) or by us (in result_tmp/). *)
           Log.info (fun f -> f "Build succeeded, looking for layerinfo");
           let rootfs = result_tmp / "rootfs" in
           let snap_key =
             if Sys.file_exists (layerinfo_path rootfs) then begin
               Log.info (fun f -> f "Reading layerinfo from %s" (layerinfo_path rootfs));
               let (sk, _, _) = read_layerinfo rootfs in sk
             end else if Sys.file_exists (layerinfo_path result_tmp) then begin
               Log.info (fun f -> f "Reading layerinfo from %s" (layerinfo_path result_tmp));
               let (sk, _, _) = read_layerinfo result_tmp in sk
             end else begin
               Log.info (fun f -> f "No layerinfo found, using key %s" key);
               key
             end
           in
           Log.info (fun f -> f "Snapshot key is %s" snap_key);
           let committed_key = snap_key ^ "-committed" in
           Log.info (fun f -> f "Committing snapshot %s -> %s" snap_key committed_key);
           (Ctr.snapshot_commit ~key:snap_key ~committed_key () >>= function
            | Ok () -> Lwt.return_unit
            | Error (`Msg m) ->
              Log.warn (fun f -> f "Failed to commit snapshot %s: %s" snap_key m);
              Lwt.return_unit) >>= fun () ->
           (* On Windows, use 'move' command instead of Sys.rename
              since the latter seems to have issues with directory handles. *)
           if Sys.win32 then begin
             Gc.full_major ();
             Gc.compact ();
             let pp f = Fmt.pf f "[ MV ]" in
             Os.exec_result ~pp ["move"; result_tmp; result]
             >>= function
             | Ok () -> Lwt.return_unit
             | Error (`Msg m) ->
               (* Fallback: try robocopy with /MOVE.
                  Note: robocopy exit codes < 8 indicate success. *)
               Log.warn (fun f -> f "move command failed (%s), trying robocopy" m);
               let is_robocopy_success n = n < 8 in
               Os.exec_result ~pp ~is_success:is_robocopy_success
                 ["robocopy"; result_tmp; result; "/E"; "/MOVE"; "/NFL"; "/NDL"; "/NJH"; "/NJS"]
               >>= function
               | Ok () ->
                 (* robocopy doesn't remove the source directory, do it manually *)
                 Os.rm ~directory:result_tmp
               | Error (`Msg m2) ->
                 Log.warn (fun f -> f "robocopy also failed: %s" m2);
                 Lwt.fail_with (Printf.sprintf "Failed to move %s to %s: %s" result_tmp result m)
           end else
             Os.mv ~src:result_tmp result
         | Error _ ->
           (* Clean up snapshot if we created one *)
           (if base <> None then
              Ctr.snapshot_rm ~key () >>= function
              | Ok () -> Lwt.return_unit
              | Error (`Msg m) ->
                Log.warn (fun f -> f "Failed to remove snapshot %s: %s" key m);
                Lwt.return_unit
            else Lwt.return_unit) >>= fun () ->
           Os.rm ~directory:result_tmp
       end >>= fun () ->
       Lwt.return r
    )
    (fun ex ->
      Log.warn (fun f -> f "Uncaught exception from %S build function: %a" id Fmt.exn ex);
      (if base <> None then
         Ctr.snapshot_rm ~key () >>= function
         | Ok () -> Lwt.return_unit
         | Error (`Msg m) ->
           Log.warn (fun f -> f "Failed to remove snapshot %s: %s" key m);
           Lwt.return_unit
       else Lwt.return_unit) >>= fun () ->
      Os.rm ~directory:result_tmp >>= fun () ->
      Lwt.reraise ex
    )

let result t id =
  let dir = Path.result t id in
  match Os.check_dir dir with
  | `Present -> Lwt.return_some dir
  | `Missing -> Lwt.return_none

let log_file t id =
  result t id >|= function
  | Some dir -> dir / "log"
  | None -> (Path.result_tmp t id) / "log"

let state_dir = Path.state

let get_cache t name =
  match Hashtbl.find_opt t.caches name with
  | Some c -> c
  | None ->
    let c = { lock = Lwt_mutex.create (); gen = 0 } in
    Hashtbl.add t.caches name c;
    c

let cache ~user:_ t name =
  let cache = get_cache t name in
  Lwt_mutex.with_lock cache.lock @@ fun () ->
  let tmp = Path.cache_tmp t t.next name in
  t.next <- t.next + 1;
  let master = Path.cache t name in
  begin match Os.check_dir master with
    | `Missing ->
      Os.ensure_dir master;
      Lwt.return_unit
    | `Present -> Lwt.return_unit
  end >>= fun () ->
  let gen = cache.gen in
  Os.ensure_dir tmp;
  Os.cp ~src:master tmp >>= fun () ->
  let release () =
    Lwt_mutex.with_lock cache.lock @@ fun () ->
    begin
      if cache.gen = gen then (
        cache.gen <- cache.gen + 1;
        Os.rm ~directory:master >>= fun () ->
        Os.mv ~src:tmp master
      ) else
        Os.rm ~directory:tmp
    end
  in
  Lwt.return (tmp, release)

let delete_cache t name =
  let cache = get_cache t name in
  Lwt_mutex.with_lock cache.lock @@ fun () ->
  cache.gen <- cache.gen + 1;
  let snapshot = Path.cache t name in
  if Sys.file_exists snapshot then (
    Os.rm ~directory:snapshot >>= fun () ->
    Lwt_result.return ()
  ) else Lwt_result.return ()

let complete_deletes _ =
  Lwt.return_unit
