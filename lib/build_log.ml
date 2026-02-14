open Lwt.Infix

let max_chunk_size = 4096

type t = {
  mutable state : [
    | `Open of Lwt_unix.file_descr * unit Lwt_condition.t  (* Fires after writing more data. *)
    | `Readonly of string
    | `Empty
    | `Finished
  ];
  mutable len : int;
}

let with_dup fd fn =
  let fd = Lwt_unix.dup ~cloexec:true fd in
  Lwt.finalize
    (fun () -> fn fd)
    (fun () -> Lwt_unix.close fd)

let catch_cancel fn =
  Lwt.catch fn
    (function
      | Lwt.Canceled -> Lwt_result.fail `Cancelled
      | ex -> Lwt.reraise ex
    )

let tail ?switch t dst =
  let rec readonly_tail ch buf =
    Lwt_io.read_into ch buf 0 max_chunk_size >>= function
    | 0 -> Lwt_result.return ()
    | n -> dst (Bytes.sub_string buf 0 n); readonly_tail ch buf
  in

  let rec open_tail fd cond buf i =
    match switch with
    | Some sw when not (Lwt_switch.is_on sw) -> Lwt_result.fail `Cancelled
    | Some _ | None ->
      let avail = min (t.len - i) max_chunk_size in
      if avail > 0 then (
        Lwt_unix.pread fd ~file_offset:i buf 0 avail >>= fun n ->
        dst (Bytes.sub_string buf 0 n);
        open_tail fd cond buf (i + avail)
      ) else (
        match t.state with
        | `Open _ -> Lwt_condition.wait cond >>= fun () -> open_tail fd cond buf i
        | `Readonly _ | `Empty | `Finished -> Lwt_result.return ()
      )
  in

  let interrupt th =
    catch_cancel @@ fun () ->
    Lwt_switch.add_hook_or_exec switch (fun () -> Lwt.cancel th; Lwt.return_unit) >>= fun () ->
    th
  in

  match t.state with
  | `Finished -> invalid_arg "tail: log is finished!"
  | `Readonly path ->
    if Sys.win32 then begin
      (* On Windows, Lwt_io hangs. Use synchronous read instead. *)
      let rec sync_read ic =
        match switch with
        | Some sw when not (Lwt_switch.is_on sw) -> Lwt_result.fail `Cancelled
        | Some _ | None ->
          let buf = Bytes.create max_chunk_size in
          let n = input ic buf 0 max_chunk_size in
          if n > 0 then begin
            dst (Bytes.sub_string buf 0 n);
            sync_read ic
          end else
            Lwt_result.return ()
      in
      let ic = open_in path in
      Lwt.finalize
        (fun () -> sync_read ic)
        (fun () -> close_in ic; Lwt.return_unit)
    end else begin
      let flags = [Unix.O_RDONLY; Unix.O_NONBLOCK; Unix.O_CLOEXEC] in
      Lwt_io.(with_file ~mode:input ~flags) path @@ fun ch ->
      let buf = Bytes.create max_chunk_size in
      interrupt (readonly_tail ch buf)
    end
  | `Empty -> Lwt_result.return ()
  | `Open (fd, cond) ->
    if Sys.win32 then begin
      (* On Windows, Lwt_unix.pread hangs. Use synchronous read with polling instead. *)
      let unix_fd = Lwt_unix.unix_file_descr fd in
      let rec sync_open_tail i =
        match switch with
        | Some sw when not (Lwt_switch.is_on sw) -> Lwt_result.fail `Cancelled
        | Some _ | None ->
          let avail = t.len - i in
          if avail > 0 then begin
            (* There's data to read *)
            let buf = Bytes.create (min avail max_chunk_size) in
            let n =
              try
                let _ = Unix.lseek unix_fd i Unix.SEEK_SET in
                Unix.read unix_fd buf 0 (Bytes.length buf)
              with _ -> 0
            in
            if n > 0 then begin
              dst (Bytes.sub_string buf 0 n);
              sync_open_tail (i + n)
            end else
              sync_open_tail i  (* Retry *)
          end else begin
            match t.state with
            | `Open _ ->
              (* Wait for more data to be written *)
              Lwt_condition.wait cond >>= fun () ->
              sync_open_tail i
            | `Readonly _ | `Empty | `Finished -> Lwt_result.return ()
          end
      in
      interrupt (sync_open_tail 0)
    end else begin
      (* Dup [fd], which can still work after [fd] is closed. *)
      with_dup fd @@ fun fd ->
      let buf = Bytes.create max_chunk_size in
      interrupt (open_tail fd cond buf 0)
    end

let create path =
  if Sys.win32 then begin
    (* On Windows, use Unix.openfile to avoid potential Lwt_unix.openfile issues *)
    let unix_fd = Unix.openfile path [Unix.O_CREAT; Unix.O_TRUNC; Unix.O_RDWR] 0o666 in
    let fd = Lwt_unix.of_unix_file_descr ~blocking:true unix_fd in
    let cond = Lwt_condition.create () in
    Lwt.return {
      state = `Open (fd, cond);
      len = 0;
    }
  end else
    Lwt_unix.openfile path Lwt_unix.[O_CREAT; O_TRUNC; O_RDWR; O_CLOEXEC] 0o666 >|= fun fd ->
    let cond = Lwt_condition.create () in
    {
      state = `Open (fd, cond);
      len = 0;
    }

let finish t =
  match t.state with
  | `Finished -> invalid_arg "Log is already finished!"
  | `Open (fd, cond) ->
    t.state <- `Finished;
    if Sys.win32 then begin
      (* On Windows, Lwt_unix.close may hang like other Lwt I/O operations.
         Use Unix.close directly to ensure the broadcast happens. *)
      (try Unix.close (Lwt_unix.unix_file_descr fd) with _ -> ());
      Lwt_condition.broadcast cond ();
      Lwt.return_unit
    end else
      Lwt_unix.close fd >|= fun () ->
      Lwt_condition.broadcast cond ()
  | `Readonly _ ->
    t.state <- `Finished;
    Lwt.return_unit
  | `Empty ->
    Lwt.return_unit (* Empty can be reused *)

let write t data =
  match t.state with
  | `Finished -> invalid_arg "write: log is finished!"
  | `Readonly _ | `Empty -> invalid_arg "Log is read-only!"
  | `Open (fd, cond) ->
    let len = String.length data in
    Os.write_all fd (Bytes.of_string data) 0 len >>= fun () ->
    t.len <- t.len + len;
    Lwt_condition.broadcast cond ();
    Lwt.return_unit

let of_saved path =
  Lwt_unix.lstat path >|= fun stat ->
  {
    state = `Readonly path;
    len = stat.st_size;
  }

let printf t fmt =
  Fmt.kstr (write t) fmt

let empty = {
  state = `Empty;
  len = 0;
}

let copy ~src ~dst =
  let buf = Bytes.create max_chunk_size in
  let rec aux () =
    Lwt_unix.read src buf 0 (Bytes.length buf) >>= function
    | 0 -> Lwt.return_unit
    | n -> write dst (Bytes.sub_string buf 0 n) >>= aux
  in
  aux ()
