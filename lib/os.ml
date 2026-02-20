type unix_fd = {
  raw : Unix.file_descr;
  mutable needs_close : bool;
}

let stdout = {
  raw = Unix.stdout;
  needs_close = false;
}

let stderr = {
  raw = Unix.stderr;
  needs_close = false;
}

let close fd =
  assert (fd.needs_close);
  Unix.close fd.raw;
  fd.needs_close <- false

let ensure_closed_unix fd =
  if fd.needs_close then
    close fd

let pp_cmd f (cmd, argv) =
  let argv = if cmd = "" then argv else cmd :: argv in
  Fmt.hbox Fmt.(list ~sep:sp (quote string)) f argv

let redirection = function
  | `FD_move_safely x -> `FD_copy x.raw
  | `Dev_null -> `Dev_null

let close_redirection (x : [`FD_move_safely of unix_fd | `Dev_null]) =
  match x with
  | `FD_move_safely x -> ensure_closed_unix x
  | `Dev_null -> ()

(* Process execution using Unix directly *)
let default_exec ?proc_mgr ?cwd ?stdin ?stdout ?stderr ~pp argv =
  ignore proc_mgr;
  let cmd, args = argv in
  let args = Array.to_list args in
  let executable = if cmd = "" then List.hd args else cmd in
  let args = if cmd = "" then args else cmd :: args in
  try
    let stdin_fd = match stdin with
      | Some (`FD_copy fd) -> fd
      | Some `Dev_null | None -> Unix.openfile "/dev/null" [Unix.O_RDONLY] 0
    in
    let stdout_fd = match stdout with
      | Some (`FD_copy fd) -> fd
      | Some `Dev_null | None -> Unix.openfile "/dev/null" [Unix.O_WRONLY] 0
    in
    let stderr_fd = match stderr with
      | Some (`FD_copy fd) -> fd
      | Some `Dev_null | None -> Unix.openfile "/dev/null" [Unix.O_WRONLY] 0
    in
    let close_if_devnull fd orig =
      match orig with
      | Some (`FD_copy _) -> ()
      | _ -> Unix.close fd
    in
    let old_cwd = match cwd with
      | Some path ->
        let old = Unix.getcwd () in
        Unix.chdir path;
        Some old
      | None -> None
    in
    let pid = Unix.create_process executable (Array.of_list args) stdin_fd stdout_fd stderr_fd in
    close_if_devnull stdin_fd stdin;
    close_if_devnull stdout_fd stdout;
    close_if_devnull stderr_fd stderr;
    let _, status = Unix.waitpid [] pid in
    Option.iter Unix.chdir old_cwd;
    match status with
    | Unix.WEXITED n -> Ok n
    | Unix.WSIGNALED x -> Fmt.error_msg "%t failed with signal %a" pp Fmt.Dump.signal x
    | Unix.WSTOPPED x -> Fmt.error_msg "%t stopped with signal %a" pp Fmt.Dump.signal x
  with e ->
    Fmt.error_msg "%t raised %s\n%s" pp (Printexc.to_string e) (Printexc.get_backtrace ())

(* Reference for overriding in unit-tests.
   The function ignores proc_mgr - it's kept for API compatibility but we use
   Unix process execution directly. *)
let process_exec_impl = ref (fun ?cwd ?stdin ?stdout ?stderr ~pp argv ->
    default_exec ~proc_mgr:(Obj.magic ()) ?cwd ?stdin ?stdout ?stderr ~pp argv)

let process_exec ~proc_mgr ?cwd ?stdin ?stdout ?stderr ~pp argv =
  ignore proc_mgr;
  !process_exec_impl ?cwd ?stdin ?stdout ?stderr ~pp argv

let exec_result ?proc_mgr ?cwd ?stdin ?stdout ?stderr ~pp ?(is_success=((=) 0)) ?(cmd="") argv =
  Logs.info (fun f -> f "Exec %a" pp_cmd (cmd, argv));
  let stdin  = Option.map redirection stdin in
  let stdout = Option.map redirection stdout in
  let stderr = Option.map redirection stderr in
  let proc_mgr = match proc_mgr with Some p -> p | None -> Obj.magic () in
  let result = process_exec ~proc_mgr ?cwd ?stdin ?stdout ?stderr ~pp (cmd, Array.of_list argv) in
  match result with
  | Ok n when is_success n -> Ok ()
  | Ok n -> Fmt.error_msg "%t failed with exit status %d" pp n
  | Error e -> Error (e : [`Msg of string] :> [> `Msg of string])

let exec ?proc_mgr ?cwd ?stdin ?stdout ?stderr ?(is_success=((=) 0)) ?(cmd="") argv =
  Logs.info (fun f -> f "Exec %a" pp_cmd (cmd, argv));
  let pp f = pp_cmd f (cmd, argv) in
  let stdin  = Option.map redirection stdin in
  let stdout = Option.map redirection stdout in
  let stderr = Option.map redirection stderr in
  let proc_mgr = match proc_mgr with Some p -> p | None -> Obj.magic () in
  match process_exec ~proc_mgr ?cwd ?stdin ?stdout ?stderr ~pp (cmd, Array.of_list argv) with
  | Ok n when is_success n -> ()
  | Ok n -> Fmt.failwith "%t failed with exit status %d" pp n
  | Error (`Msg m) -> failwith m

(* Similar to default_exec except returns the pid for cleaner job cancellations *)
let open_process ?cwd ?env ?stdin ?stdout ?stderr ?pp:_ argv =
  Logs.info (fun f -> f "Fork exec %a" pp_cmd ("", argv));
  let stdin  = Option.map redirection stdin in
  let stdout = Option.map redirection stdout in
  let stderr = Option.map redirection stderr in
  let executable = List.hd argv in
  let pid = Unix.create_process_env executable (Array.of_list argv)
      (match env with Some e -> e | None -> Unix.environment ())
      (match stdin with Some (`FD_copy fd) -> fd | Some `Dev_null -> Unix.stdin | None -> Unix.stdin)
      (match stdout with Some (`FD_copy fd) -> fd | Some `Dev_null -> Unix.stdout | None -> Unix.stdout)
      (match stderr with Some (`FD_copy fd) -> fd | Some `Dev_null -> Unix.stderr | None -> Unix.stderr)
  in
  (match cwd with Some _ -> () | None -> ());
  (pid, fun () ->
     let _, status = Unix.waitpid [] pid in
     status)

let process_result ~pp proc =
  match proc () with
  | Unix.WEXITED 0 -> Ok ()
  | Unix.WEXITED n -> Fmt.error_msg "%t failed with exit status %d" pp n
  | Unix.WSIGNALED x -> Fmt.error_msg "%t failed with signal %a" pp Fmt.Dump.signal x
  | Unix.WSTOPPED x -> Fmt.error_msg "%t stopped with signal %a" pp Fmt.Dump.signal x

let running_as_root = not (Sys.unix) || Unix.getuid () = 0

let sudo ?proc_mgr ?stdin args =
  let args = if running_as_root then args else "sudo" :: "--" :: args in
  exec ?proc_mgr ?stdin args

let sudo_result ?proc_mgr ?cwd ?stdin ?stdout ?stderr ?is_success ~pp args =
  let args = if running_as_root then args else "sudo" :: "--" :: args in
  exec_result ?proc_mgr ?cwd ?stdin ?stdout ?stderr ?is_success ~pp args

let write_all fd buf ofs len =
  let rec aux ofs len =
    assert (len >= 0);
    if len = 0 then ()
    else
      let n = Unix.write fd buf ofs len in
      aux (ofs + n) (len - n)
  in
  aux ofs len

let write_all_string fd buf ofs len =
  write_all fd (Bytes.of_string buf) ofs len

let write_file ~path contents =
  let flags = [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC; Unix.O_CLOEXEC] in
  let fd = Unix.openfile path flags 0o644 in
  Fun.protect ~finally:(fun () -> Unix.close fd)
    (fun () -> write_all_string fd contents 0 (String.length contents))

let with_pipe_from_child fn =
  let r, w = Unix.pipe ~cloexec:true () in
  let w = { raw = w; needs_close = true } in
  Fun.protect ~finally:(fun () ->
      ensure_closed_unix w;
      Unix.close r)
    (fun () -> fn ~r ~w)

let with_pipe_to_child fn =
  let r, w = Unix.pipe ~cloexec:true () in
  let r = { raw = r; needs_close = true } in
  Fun.protect ~finally:(fun () ->
      ensure_closed_unix r;
      Unix.close w)
    (fun () -> fn ~r ~w)

let with_pipe_between_children fn =
  let r, w = Unix.pipe ~cloexec:true () in
  let r = { raw = r; needs_close = true } in
  let w = { raw = w; needs_close = true } in
  Fun.protect ~finally:(fun () ->
      ensure_closed_unix r;
      ensure_closed_unix w)
    (fun () -> fn ~r ~w)

let read_all fd =
  let buf = Buffer.create 1024 in
  let tmp = Bytes.create 4096 in
  let rec loop () =
    match Unix.read fd tmp 0 (Bytes.length tmp) with
    | 0 -> Buffer.contents buf
    | n ->
      Buffer.add_subbytes buf tmp 0 n;
      loop ()
  in
  loop ()

let pread ?proc_mgr ?stderr argv =
  with_pipe_from_child @@ fun ~r ~w ->
  exec ?proc_mgr ~stdout:(`FD_move_safely w) ?stderr argv;
  ensure_closed_unix w;
  read_all r

let pread_result ?proc_mgr ?cwd ?stdin ?stderr ~pp ?is_success ?cmd argv =
  with_pipe_from_child @@ fun ~r ~w ->
  match exec_result ?proc_mgr ?cwd ?stdin ~stdout:(`FD_move_safely w) ?stderr ~pp ?is_success ?cmd argv with
  | Ok () ->
    ensure_closed_unix w;
    Ok (read_all r)
  | Error e -> Error e

let pread_all ?proc_mgr ?stdin ~pp ?(cmd="") argv =
  with_pipe_from_child @@ fun ~r:r1 ~w:w1 ->
  with_pipe_from_child @@ fun ~r:r2 ~w:w2 ->
  Logs.info (fun f -> f "Exec %a" pp_cmd (cmd, argv));
  let stdin  = Option.map redirection stdin in
  let proc_mgr = match proc_mgr with Some p -> p | None -> Obj.magic () in
  match process_exec ~proc_mgr ?stdin ~stdout:(`FD_copy w1.raw) ~stderr:(`FD_copy w2.raw) ~pp
          (cmd, Array.of_list argv) with
  | Ok i ->
    ensure_closed_unix w1;
    ensure_closed_unix w2;
    let stdout_data = read_all r1 in
    let stderr_data = read_all r2 in
    (i, stdout_data, stderr_data)
  | Error (`Msg m) -> failwith m

let check_dir x =
  match Unix.lstat x with
  | Unix.{ st_kind = S_DIR; _ } -> `Present
  | _ -> Fmt.failwith "Exists, but is not a directory: %S" x
  | exception Unix.Unix_error(Unix.ENOENT, _, _) -> `Missing

let check_file x =
  match Unix.lstat x with
  | Unix.{ st_kind = S_REG; _ } -> `Present
  | _ -> Fmt.failwith "Exists, but is not a regular file: %S" x
  | exception Unix.Unix_error(Unix.ENOENT, _, _) -> `Missing

let ensure_dir ?(mode=0o777) path =
  match check_dir path with
  | `Present -> ()
  | `Missing -> Unix.mkdir path mode

let read_link x =
  match Unix.readlink x with
  | s -> Some s
  | exception Unix.Unix_error(Unix.ENOENT, _, _) -> None

let rm ?proc_mgr ~directory () =
  let pp _ ppf = Fmt.pf ppf "[ RM ]" in
  match sudo_result ?proc_mgr ~pp:(pp "RM") ["rm"; "-r"; directory ] with
  | Ok () -> ()
  | Error (`Msg m) ->
    Log.warn (fun f -> f "Failed to remove %s because %s" directory m)

let mv ?proc_mgr ~src dst =
  let pp _ ppf = Fmt.pf ppf "[ MV ]" in
  match sudo_result ?proc_mgr ~pp:(pp "MV") ["mv"; src; dst ] with
  | Ok () -> ()
  | Error (`Msg m) ->
    Log.warn (fun f -> f "Failed to move %s to %s because %s" src dst m)

let cp ?proc_mgr ~src dst =
  let pp _ ppf = Fmt.pf ppf "[ CP ]" in
  match sudo_result ?proc_mgr ~pp:(pp "CP") ["cp"; "-pRduT"; "--reflink=auto"; src; dst ] with
  | Ok () -> ()
  | Error (`Msg m) ->
    Log.warn (fun f -> f "Failed to copy from %s to %s because %s" src dst m)

let with_temp_dir ~prefix fn =
  let tmp = Filename.temp_dir prefix "" in
  Unix.chmod tmp 0o700;
  Fun.protect ~finally:(fun () ->
      (* Clean up temp directory *)
      Array.iter (fun f -> Unix.unlink (Filename.concat tmp f)) (Sys.readdir tmp);
      Unix.rmdir tmp)
    (fun () -> fn tmp)

let normalise_path root_dir =
  if Sys.win32 then
    let vol, _ = Fpath.(v root_dir |> split_volume) in
    vol ^ "\\"
  else
    root_dir

let free_space_percent root_dir =
  let vfs = ExtUnix.All.statvfs (normalise_path root_dir) in
  let used = Int64.sub vfs.f_blocks vfs.f_bfree in
  100. -. 100. *. (Int64.to_float used) /. Int64.(to_float (add used vfs.f_bavail))

let read_lines name process =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop ((process s) :: acc)
    | None -> close_in ic; acc in
  loop []
