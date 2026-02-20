module Os = Obuilder.Os

let ( / ) = Filename.concat

type t = {
  dir : string;
  cond : Eio.Condition.t;
  cond_mutex : Eio.Mutex.t;
  mutable builds : int;
}

let unix_path ~proc_mgr path =
  if Sys.win32 then
    let buf = Buffer.create 256 in
    Eio.Process.run proc_mgr ~stdout:(Eio.Flow.buffer_sink buf) [| "cygpath"; "-u"; path |];
    String.trim (Buffer.contents buf)
  else
    path

let delay_store = ref (fun () -> ())

let rec waitpid_non_intr pid =
  try Unix.waitpid [] pid
  with Unix.Unix_error (Unix.EINTR, _, _) -> waitpid_non_intr pid

let rm_r path =
  let rm = Unix.create_process "rm" [| "rm"; "-r"; "--"; path |] Unix.stdin Unix.stdout Unix.stderr in
  match waitpid_non_intr rm with
  | _, Unix.WEXITED 0 -> ()
  | _ -> failwith "rm -r failed!"

let build ~proc_mgr t ?base ~id fn =
  t.builds <- t.builds + 1;
  Fun.protect ~finally:(fun () ->
    t.builds <- t.builds - 1;
    Eio.Condition.broadcast t.cond
  ) @@ fun () ->
  base |> Option.iter (fun base -> assert (not (String.contains base '/')));
  let dir = t.dir / id in
  assert (Os.check_dir dir = `Missing);
  let tmp_dir = dir ^ "-tmp" in
  assert (not (Sys.file_exists tmp_dir));
  begin match base with
    | None -> Os.ensure_dir tmp_dir
    | Some base ->
      let src = unix_path ~proc_mgr (t.dir / base) in
      let dst = unix_path ~proc_mgr tmp_dir in
      Eio.Process.run proc_mgr [| "cp"; "-r"; src; dst |]
  end;
  let r = fn tmp_dir in
  !delay_store ();
  match r with
  | Ok () ->
    Unix.rename tmp_dir dir;
    Ok ()
  | Error _ as e ->
    let tmp_dir = unix_path ~proc_mgr tmp_dir in
    rm_r tmp_dir;
    e

let state_dir t = t.dir / "state"

let path t id = t.dir / id

let result t id =
  let dir = path t id in
  match Os.check_dir dir with
  | `Present -> Some dir
  | `Missing -> None

let log_file t id =
  t.dir / "logs" / (id  ^ ".log")

let rec finish t =
  if t.builds > 0 then (
    Logs.info (fun f -> f "Waiting for %d builds to finish" t.builds);
    Eio.Mutex.use_rw ~protect:false t.cond_mutex (fun () ->
      Eio.Condition.await t.cond t.cond_mutex);
    finish t
  )

let with_store ~fs fn =
  let dir = Filename.temp_file "mock-store-" "" in
  Unix.unlink dir;
  Unix.mkdir dir 0o755;
  let t = { dir; cond = Eio.Condition.create (); cond_mutex = Eio.Mutex.create (); builds = 0 } in
  Obuilder.Os.ensure_dir (state_dir t);
  Obuilder.Os.ensure_dir (t.dir / "logs");
  Fun.protect ~finally:(fun () -> finish t; rm_r dir) @@ fun () ->
  ignore fs;
  fn t

let delete t id =
  match result t id with
  | Some path -> rm_r path
  | None -> ()

let find ~output t =
  let rec aux = function
    | [] -> None
    | x :: xs ->
      let output_path = t.dir / x / "rootfs" / "output" in
      if Sys.file_exists output_path then (
        let ic = open_in_bin output_path in
        let len = in_channel_length ic in
        let data = really_input_string ic len in
        close_in ic;
        if data = output then Some x
        else aux xs
      ) else aux xs
  in
  let items = Sys.readdir t.dir |> Array.to_list |> List.sort String.compare in
  aux items

let cache ~user:_ _t _ = assert false

let delete_cache _t _ = assert false

let complete_deletes _t = ()

let root t = t.dir

let df _ = 100.
