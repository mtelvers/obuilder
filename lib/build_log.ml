let max_chunk_size = 4096

type t = {
  mutable state : [
    | `Open of Unix.file_descr * Eio.Condition.t * Eio.Mutex.t  (* Fires after writing more data. *)
    | `Readonly of string
    | `Empty
    | `Finished
  ];
  mutable len : int;
}

let catch_cancel fn =
  try Ok (fn ())
  with Eio.Cancel.Cancelled _ -> Error `Cancelled

(* Check if a switch is still active (not cancelled/failed) *)
let switch_is_on sw =
  try Eio.Fiber.check (); true
  with Eio.Cancel.Cancelled _ -> false

let tail ?sw t dst =
  let rec readonly_tail fd buf =
    match Unix.read fd buf 0 max_chunk_size with
    | 0 -> Ok ()
    | n -> dst (Bytes.sub_string buf 0 n); readonly_tail fd buf
  in

  let rec open_tail fd cond mutex buf i =
    match sw with
    | Some _sw when not (switch_is_on _sw) -> Error `Cancelled
    | Some _ | None ->
      let avail = min (t.len - i) max_chunk_size in
      if avail > 0 then (
        ignore (Unix.lseek fd i Unix.SEEK_SET);
        let n = Unix.read fd buf 0 avail in
        dst (Bytes.sub_string buf 0 n);
        open_tail fd cond mutex buf (i + n)
      ) else (
        match t.state with
        | `Open _ ->
          Eio.Mutex.lock mutex;
          Eio.Condition.await cond mutex;
          Eio.Mutex.unlock mutex;
          open_tail fd cond mutex buf i
        | `Readonly _ | `Empty | `Finished -> Ok ()
      )
  in

  match t.state with
  | `Finished -> invalid_arg "tail: log is finished!"
  | `Readonly path ->
    let flags = [Unix.O_RDONLY; Unix.O_CLOEXEC] in
    let fd = Unix.openfile path flags 0 in
    Fun.protect ~finally:(fun () -> Unix.close fd)
      (fun () ->
         let buf = Bytes.create max_chunk_size in
         catch_cancel (fun () ->
             match readonly_tail fd buf with
             | Ok () -> ()
             | Error _ -> ()))
  | `Empty -> Ok ()
  | `Open (fd, cond, mutex) ->
    (* Dup [fd], which can still work after [fd] is closed. *)
    let fd' = Unix.dup fd in
    Fun.protect ~finally:(fun () -> Unix.close fd')
      (fun () ->
         let buf = Bytes.create max_chunk_size in
         catch_cancel (fun () ->
             match open_tail fd' cond mutex buf 0 with
             | Ok () -> ()
             | Error _ -> ()))

let create path =
  let fd = Unix.openfile path [Unix.O_CREAT; Unix.O_TRUNC; Unix.O_RDWR; Unix.O_CLOEXEC] 0o666 in
  let cond = Eio.Condition.create () in
  let mutex = Eio.Mutex.create () in
  {
    state = `Open (fd, cond, mutex);
    len = 0;
  }

let finish t =
  match t.state with
  | `Finished -> invalid_arg "Log is already finished!"
  | `Open (fd, cond, _mutex) ->
    t.state <- `Finished;
    Unix.close fd;
    Eio.Condition.broadcast cond
  | `Readonly _ ->
    t.state <- `Finished
  | `Empty ->
    () (* Empty can be reused *)

let write t data =
  match t.state with
  | `Finished -> invalid_arg "write: log is finished!"
  | `Readonly _ | `Empty -> invalid_arg "Log is read-only!"
  | `Open (fd, cond, _mutex) ->
    let len = String.length data in
    Os.write_all fd (Bytes.of_string data) 0 len;
    t.len <- t.len + len;
    Eio.Condition.broadcast cond

let of_saved path =
  let stat = Unix.lstat path in
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
    match Unix.read src buf 0 (Bytes.length buf) with
    | 0 -> ()
    | n -> write dst (Bytes.sub_string buf 0 n); aux ()
  in
  aux ()
