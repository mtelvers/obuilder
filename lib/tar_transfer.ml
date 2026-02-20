let ( / ) = Filename.concat

let level = Tar.Header.GNU

let copy_to ~dst src =
  let len = 4096 in
  let buf = Bytes.create len in
  let rec aux () =
    match Unix.read src buf 0 len with
    | 0 -> ()
    | n -> Os.write_all dst buf 0 n; aux ()
  in
  aux ()

let get_ids = function
  | `Unix user -> Some user.Obuilder_spec.uid, Some user.gid, None, None
  | `Windows user when user.Obuilder_spec.name = "ContainerAdministrator" ->
    (* https://cygwin.com/cygwin-ug-net/ntsec.html#ntsec-mapping *)
    let x = 93 and rid = 1 in
    Some (0x1000 * x + rid), Some (0x1000 * x + rid), Some user.name, Some user.name
  | `Windows _ -> None, None, None, None

let write_block ~level (header: Tar.Header.t) (body: Unix.file_descr -> unit) (fd : Unix.file_descr) =
  let header_buf = Bytes.create Tar.Header.length in
  (match Tar.Header.marshal ~level header_buf header with
   | Ok () -> ()
   | Error (`Msg msg) -> failwith msg);
  Os.write_all fd header_buf 0 Tar.Header.length;
  body fd;
  let padding_len = Tar.Header.compute_zero_padding_length header in
  if padding_len > 0 then
    Os.write_all fd (Bytes.make padding_len '\000') 0 padding_len

let write_end (fd: Unix.file_descr) =
  let zero_block = Bytes.make Tar.Header.length '\000' in
  Os.write_all fd zero_block 0 (Bytes.length zero_block);
  Os.write_all fd zero_block 0 (Bytes.length zero_block)

let copy_file ~src ~dst ~to_untar ~user =
  let stat = Unix.LargeFile.lstat src in
  let user_id, group_id, uname, gname = get_ids user in
  let hdr = Tar.Header.make
      ~file_mode:(if stat.Unix.LargeFile.st_perm land 0o111 <> 0 then 0o755 else 0o644)
      ~mod_time:(Int64.of_float stat.Unix.LargeFile.st_mtime)
      ?user_id ?group_id ?uname ?gname
      dst stat.Unix.LargeFile.st_size
  in
  write_block ~level hdr (fun ofd ->
      let flags = [Unix.O_RDONLY; Unix.O_CLOEXEC] in
      let ifd = Unix.openfile src flags 0 in
      Fun.protect ~finally:(fun () -> Unix.close ifd)
        (fun () -> copy_to ~dst:ofd ifd)
    ) to_untar

let copy_symlink ~src ~target ~dst ~to_untar ~user =
  let stat = Unix.LargeFile.lstat src in
  let user_id, group_id, uname, gname = get_ids user in
  let hdr = Tar.Header.make
      ~file_mode:0o777
      ~mod_time:(Int64.of_float stat.Unix.LargeFile.st_mtime)
      ~link_indicator:Tar.Header.Link.Symbolic
      ~link_name:target
      ?user_id ?group_id ?uname ?gname
      dst 0L
  in
  write_block ~level hdr (fun _ -> ()) to_untar

let rec copy_dir ~src_dir ~src ~dst ~(items:(Manifest.t list)) ~to_untar ~user =
  Log.debug(fun f -> f "Copy dir %S -> %S" src dst);
  let stat = Unix.LargeFile.lstat (src_dir / src) in
  let user_id, group_id, uname, gname = get_ids user in
  let hdr = Tar.Header.make
      ~file_mode:0o755
      ~mod_time:(Int64.of_float stat.Unix.LargeFile.st_mtime)
      ?user_id ?group_id ?uname ?gname
      (dst ^ "/") 0L
  in
  write_block ~level hdr (fun _ -> ()) to_untar;
  send_dir ~src_dir ~dst ~to_untar ~user items

and send_dir ~src_dir ~dst ~to_untar ~user items =
  List.iter (function
      | `File (src, _) ->
        let src = src_dir / src in
        let dst = dst / Filename.basename src in
        copy_file ~src ~dst ~to_untar ~user
      | `Symlink (src, target) ->
        let src = src_dir / src in
        let dst = dst / Filename.basename src in
        copy_symlink ~src ~target ~dst ~to_untar ~user
      | `Dir (src, items) ->
        let dst = dst / Filename.basename src in
        copy_dir ~src_dir ~src ~dst ~items ~to_untar ~user
    ) items

let remove_leading_slashes = Astring.String.drop ~sat:((=) '/')

let send_files ~src_dir ~src_manifest ~dst_dir ~user ~to_untar =
  let dst = remove_leading_slashes dst_dir in
  send_dir ~src_dir ~dst ~to_untar ~user src_manifest;
  write_end to_untar

let send_file ~src_dir ~src_manifest ~dst ~user ~to_untar =
  let dst = remove_leading_slashes dst in
  begin
    match src_manifest with
    | `File (path, _) ->
      let src = src_dir / path in
      copy_file ~src ~dst ~to_untar ~user
    | `Symlink (src, target) ->
      let src = src_dir / src in
      copy_symlink ~src ~target ~dst ~to_untar ~user
    | `Dir (src, items) ->
      copy_dir ~src_dir ~src ~dst ~items ~to_untar ~user
  end;
  write_end to_untar

let transform ~user fname hdr =
  (* Make a copy to erase unneeded data from the tar headers. *)
  let hdr' = Tar.Header.(make ~file_mode:hdr.file_mode ~mod_time:hdr.mod_time hdr.file_name hdr.file_size) in
  let hdr' = match user with
    | `Unix user ->
      { hdr' with Tar.Header.user_id = user.Obuilder_spec.uid; group_id = user.gid; }
    | `Windows user when user.Obuilder_spec.name = "ContainerAdministrator" ->
      (* https://cygwin.com/cygwin-ug-net/ntsec.html#ntsec-mapping *)
      let id = let x = 93 and rid = 1 in 0x1000 * x + rid in
      { hdr' with user_id = id; group_id = id; uname = user.name; gname = user.name; }
    | `Windows _ -> hdr'
  in
  match hdr.Tar.Header.link_indicator with
  | Normal ->
    { hdr' with
      file_mode = if hdr.file_mode land 0o111 <> 0 then 0o755 else 0o644;
      file_name = fname hdr.file_name; }
  | Symbolic ->
    { hdr' with
      file_mode = 0o777;
      file_name = fname hdr.file_name;
      link_indicator = hdr.link_indicator;
      link_name = hdr.link_name; }
  | Directory ->
    { hdr' with
      file_mode = 0o755;
      file_name = fname hdr.file_name ^ "/"; }
  | _ -> Fmt.invalid_arg "Unsupported file type"

let rec map_transform ~dst transformations = function
  | `File (src, _) ->
    let dst = dst / Filename.basename src in
    Hashtbl.add transformations src dst
  | `Symlink (src, _) ->
    let dst = dst / Filename.basename src in
    Hashtbl.add transformations src dst
  | `Dir (src, items) ->
    let dst = dst / Filename.basename src in
    Hashtbl.add transformations src dst;
    Log.debug(fun f -> f "Copy dir %S -> %S" src dst);
    List.iter (map_transform ~dst transformations) items

and transform_files ~from_tar ~src_manifest ~dst_dir ~user ~to_untar =
  let dst = remove_leading_slashes dst_dir in
  let transformations = Hashtbl.create ~random:true 64 in
  List.iter (map_transform ~dst transformations) src_manifest;
  let fname file_name =
    match Hashtbl.find transformations file_name with
    | exception Not_found -> Fmt.failwith "Could not find mapping for %s" file_name
    | file_name -> file_name
  in
  (* Read tar entries from from_tar and write transformed to to_untar *)
  let buf = Bytes.create 4096 in
  let rec process_entries () =
    (* Read header *)
    let header_buf = Bytes.create Tar.Header.length in
    let rec read_header pos =
      if pos >= Tar.Header.length then ()
      else
        let n = Unix.read from_tar header_buf pos (Tar.Header.length - pos) in
        if n = 0 then ()
        else read_header (pos + n)
    in
    read_header 0;
    if Bytes.for_all ((=) '\000') header_buf then
      () (* End of archive *)
    else
      let header_cstruct = Cstruct.of_bytes header_buf in
      match Tar.Header.unmarshal (Cstruct.to_string header_cstruct) with
      | Error _ -> failwith "Failed to parse tar header"
      | Ok header ->
        let transformed = transform ~user fname header in
        write_block ~level transformed (fun ofd ->
            let remaining = ref (Int64.to_int header.file_size) in
            while !remaining > 0 do
              let to_read = min !remaining (Bytes.length buf) in
              let n = Unix.read from_tar buf 0 to_read in
              if n > 0 then begin
                Os.write_all ofd buf 0 n;
                remaining := !remaining - n
              end
            done;
            (* Skip padding in source *)
            let padding_size = (512 - (Int64.to_int header.file_size mod 512)) mod 512 in
            let padding_buf = Bytes.create padding_size in
            let _ = Unix.read from_tar padding_buf 0 padding_size in
            ()
          ) to_untar;
        process_entries ()
  in
  process_entries ();
  write_end to_untar

let transform_file ~from_tar ~src_manifest ~dst ~user ~to_untar =
  let dst = remove_leading_slashes dst in
  let transformations = Hashtbl.create ~random:true 1 in
  let map_transform = function
    | `File (src, _) -> Hashtbl.add transformations src dst
    | `Symlink (src, _) -> Hashtbl.add transformations src dst
    | `Dir (src, items) ->
      Hashtbl.add transformations src dst;
      Log.debug(fun f -> f "Copy dir %S -> %S" src dst);
      List.iter (map_transform ~dst transformations) items
  in
  map_transform src_manifest;
  let fname file_name =
    match Hashtbl.find transformations file_name with
    | exception Not_found -> Fmt.failwith "Could not find mapping for %s" file_name
    | file_name -> file_name
  in
  (* Read tar entries from from_tar and write transformed to to_untar *)
  let buf = Bytes.create 4096 in
  let rec process_entries () =
    let header_buf = Bytes.create Tar.Header.length in
    let rec read_header pos =
      if pos >= Tar.Header.length then ()
      else
        let n = Unix.read from_tar header_buf pos (Tar.Header.length - pos) in
        if n = 0 then ()
        else read_header (pos + n)
    in
    read_header 0;
    if Bytes.for_all ((=) '\000') header_buf then
      ()
    else
      let header_cstruct = Cstruct.of_bytes header_buf in
      match Tar.Header.unmarshal (Cstruct.to_string header_cstruct) with
      | Error _ -> failwith "Failed to parse tar header"
      | Ok header ->
        let transformed = transform ~user fname header in
        Log.debug (fun f -> f "Copying %s -> %s" header.Tar.Header.file_name transformed.Tar.Header.file_name);
        write_block ~level transformed (fun ofd ->
            let remaining = ref (Int64.to_int header.file_size) in
            while !remaining > 0 do
              let to_read = min !remaining (Bytes.length buf) in
              let n = Unix.read from_tar buf 0 to_read in
              if n > 0 then begin
                Os.write_all ofd buf 0 n;
                remaining := !remaining - n
              end
            done;
            let padding_size = (512 - (Int64.to_int header.file_size mod 512)) mod 512 in
            let padding_buf = Bytes.create padding_size in
            let _ = Unix.read from_tar padding_buf 0 padding_size in
            ()
          ) to_untar;
        process_entries ()
  in
  process_entries ();
  write_end to_untar
