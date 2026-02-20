module Os = Obuilder.Os

let ( / ) = Filename.concat

let strf = Printf.sprintf

let unix_path path =
  if Sys.win32 then
    let buf = Buffer.create 256 in
    let ic = Unix.open_process_in (Printf.sprintf "cygpath -u %S" path) in
    let line = input_line ic in
    ignore (Unix.close_process_in ic);
    String.trim line
  else
    path

let next_container_id = ref 0

let base_tar =
  let mydir = Sys.getcwd () in
  let base_tar = mydir / "base.tar" in
  let ic = open_in_bin base_tar in
  let len = in_channel_length ic in
  let data = really_input_string ic len in
  close_in ic;
  Bytes.of_string data

let with_fd x f =
  match x with
  | `FD_move_safely fd ->
    let copy = Unix.dup ~cloexec:true fd.Os.raw in
    Os.close fd;
    Fun.protect ~finally:(fun () -> Unix.close copy) @@ fun () ->
    f copy
  | _ -> failwith "Unsupported mock FD redirection"

let write_all fd buf ofs len =
  let rec aux ofs remaining =
    if remaining = 0 then ()
    else
      let n = Unix.write fd buf ofs remaining in
      aux (ofs + n) (remaining - n)
  in
  aux ofs len

let docker_create ?stdout base =
  with_fd (Option.get stdout) @@ fun stdout ->
  let id = strf "%s-%d\n" base !next_container_id in
  incr next_container_id;
  let rec aux i =
    let len = String.length id - i in
    if len = 0 then Ok 0
    else (
      let sent = Unix.single_write_substring stdout id i len in
      aux (i + sent)
    )
  in
  aux 0

let docker_export ?stdout _id =
  with_fd (Option.get stdout) @@ fun stdout ->
  write_all stdout base_tar 0 (Bytes.length base_tar);
  Ok 0

let docker_inspect ?stdout _id =
  with_fd (Option.get stdout) @@ fun stdout ->
  let msg = Bytes.of_string "PATH=/usr/bin:/usr/local/bin" in
  write_all stdout msg 0 (Bytes.length msg);
  Ok 0

let exec_docker ?stdout = function
  | ["create"; "--"; base] -> docker_create ?stdout base
  | ["export"; "--"; id] -> docker_export ?stdout id
  | ["image"; "inspect"; "--format"; {|{{range .Config.Env}}{{print . "\x00"}}{{end}}|}; "--"; base] -> docker_inspect ?stdout base
  | ["rm"; "--force"; "--"; id] -> Fmt.pr "docker rm --force %S@." id; Ok 0
  | x -> Fmt.failwith "Unknown mock docker command %a" Fmt.(Dump.list string) x

let mkdir = function
  | ["-m"; "755"; "--"; path] -> Unix.mkdir path 0o755; Ok 0
  | x -> Fmt.failwith "Unexpected mkdir %a" Fmt.(Dump.list string) x

let closing redir fn =
  Fun.protect fn ~finally:(fun () ->
    match redir with
    | Some (`FD_move_safely fd) -> Os.ensure_closed_unix fd
    | _ -> ()
  )

let exec ?timeout ?cwd ?stdin ?stdout ?stderr ~pp cmd =
  ignore timeout;
  closing stdin @@ fun () ->
  closing stdout @@ fun () ->
  closing stderr @@ fun () ->
  match cmd with
  | ("", argv) ->
    Fmt.pr "exec: %a@." Fmt.(Dump.array string) argv;
    begin match Array.to_list argv with
      | "docker" :: args -> exec_docker ?stdout args
      | "sudo" :: "--" :: ("tar" :: _ as tar) when not Os.running_as_root ->
        Os.default_exec ?cwd ?stdin ?stdout ~pp ("", Array.of_list tar)
      | "tar" :: "-C" :: path :: opts when Os.running_as_root ->
        let path = unix_path path in
        let tar = (if Sys.win32 then "C:\\cygwin64\\bin\\tar.exe" else "tar") :: "-C" :: path :: opts in
        Os.default_exec ?cwd ?stdin ?stdout ~pp ("", Array.of_list tar)
      | "mkdir" :: args when Os.running_as_root -> mkdir args
      | "sudo" :: "--" :: "mkdir" :: args when not Os.running_as_root -> mkdir args
      | x -> Fmt.failwith "Unknown mock command %a" Fmt.(Dump.list string) x
    end
  | (x, _) -> Fmt.failwith "Unexpected absolute path: %S" x
