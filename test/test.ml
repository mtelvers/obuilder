open Obuilder

module B = Builder(Mock_store)(Mock_sandbox)(Docker_extract)

let ( / ) = Filename.concat
let sprintf = Printf.sprintf
let root = if Sys.win32 then "C:/" else "/"

let () =
  Logs.(set_level ~all:true (Some Info));
  Logs.set_reporter @@ Logs_fmt.reporter ();
  Os.process_exec_impl := Mock_exec.exec

let build_result =
  Alcotest.of_pp @@ fun f x ->
  match x with
  | Error (`Msg msg) -> Fmt.string f msg
  | Error `Cancelled -> Fmt.string f "Cancelled"
  | Ok id -> Fmt.string f id

let get store path id =
  let result = Mock_store.path store id in
  let ic = open_in_bin (result / "rootfs" / path) in
  let len = in_channel_length ic in
  let data = really_input_string ic len in
  close_in ic;
  Ok data

let with_config ~sw ~env fn =
  let fs = Eio.Stdenv.fs env in
  let proc_mgr = Eio.Stdenv.process_mgr env in
  Mock_store.with_store ~fs @@ fun store ->
  let sandbox = Mock_sandbox.create () in
  let builder = B.v ~store ~sandbox in
  Fun.protect ~finally:(fun () -> B.finish builder) @@ fun () ->
  let src_dir = Mock_store.state_dir store / "src" in
  Os.ensure_dir src_dir;
  fn ~sw ~proc_mgr ~src_dir ~store ~sandbox ~builder

let with_default_exec fn =
  Fun.protect ~finally:(fun () -> Os.process_exec_impl := Mock_exec.exec) @@ fun () ->
  Os.process_exec_impl := Os.default_exec;
  fn ()

let with_file path flags perms fn =
  let fd = Unix.openfile path flags perms in
  Fun.protect ~finally:(fun () -> Unix.close fd) @@ fun () ->
  fn fd

let mock_op ?(result=Ok ()) ?(delay_store=fun () -> ()) ?cancel ?output () =
  fun ~sw ?stdin:_ ~log (config:Obuilder.Config.t) dir ->
  Mock_store.delay_store := delay_store;
  let cmd =
    match config.argv with
    | ["/usr/bin/env" ; "bash"; "-c"; cmd] | ["cmd"; "/S"; "/C"; cmd] -> cmd
    | x -> Fmt.str "%a" Fmt.(Dump.list string) x
  in
  Build_log.printf log "%s@." cmd;
  cancel |> Option.iter (fun cancel ->
      Eio.Switch.on_release sw (fun () ->
        if not (Eio.Promise.is_resolved cancel) then
          Eio.Promise.resolve cancel (Error `Cancelled)
      )
    );
  let rootfs = dir / "rootfs" in
  begin match output with
    | Some (`Constant v) ->
      let oc = open_out_bin (rootfs / "output") in
      output_string oc v;
      close_out oc
    | Some (`Append (v, src)) ->
      let ic = open_in_bin (rootfs / src) in
      let len = in_channel_length ic in
      let src_data = really_input_string ic len in
      close_in ic;
      let oc = open_out_bin (rootfs / "output") in
      output_string oc (src_data ^ v);
      close_out oc
    | Some `Append_cmd ->
      let ic = open_in_bin (rootfs / "output") in
      let len = in_channel_length ic in
      let src_data = really_input_string ic len in
      close_in ic;
      let oc = open_out_bin (rootfs / "output") in
      output_string oc (src_data ^ cmd);
      close_out oc
    | None -> ()
  end;
  result

let test_simple ~sw ~env () =
  with_config ~sw ~env @@ fun ~sw:_ ~proc_mgr ~src_dir ~store ~sandbox ~builder ->
  let log = Log.create "b" in
  let context = Context.v ~src_dir ~log:(Log.add log) () in
  let spec = Spec.(stage ~from:"base" [ run "Append" ]) in
  Mock_sandbox.expect sandbox (mock_op ~output:(`Append ("runner", "base-id")) ());
  let result = B.build ~proc_mgr builder context spec |> Result.bind (get store "output") in
  Alcotest.(check build_result) "Final result" (Ok "base-distro\nrunner") result;
  Log.check "Check log"
    (sprintf {|(from base)
      ;---> saved as .*
      %s: (run (shell Append))
      Append
      ;---> saved as .*
     |} root) log;
  (* Check result is cached *)
  Log.clear log;
  let result = B.build ~proc_mgr builder context spec |> Result.bind (get store "output") in
  Alcotest.(check build_result) "Final result cached" (Ok "base-distro\nrunner") result;
  Log.check "Check cached log"
    (sprintf {|(from base)
      ;---> using .* from cache
      %s: (run (shell Append))
      Append
      ;---> using .* from cache
     |} root) log

let test_prune ~sw ~env () =
  with_config ~sw ~env @@ fun ~sw:_ ~proc_mgr ~src_dir ~store:_ ~sandbox ~builder ->
  let start = Unix.(gettimeofday () |> gmtime) in
  let log = Log.create "b" in
  let context = Context.v ~src_dir ~log:(Log.add log) () in
  let spec = Spec.(stage ~from:"base" [ run "Append" ]) in
  Mock_sandbox.expect sandbox (mock_op ~output:(`Append ("runner", "base-id")) ());
  let result = B.build ~proc_mgr builder context spec in
  Alcotest.(check build_result) "Final result" (Ok ()) (Result.map (fun _ -> ()) result);
  Log.check "Check log"
    (sprintf {|(from base)
      ;---> saved as .*
      %s: (run (shell Append))
      Append
      ;---> saved as .*
     |} root) log;
  let log_fn id = Logs.info (fun f -> f "Deleting %S" id) in
  let n = B.prune ~log:log_fn builder ~before:start 10 in
  Alcotest.(check int) "Nothing before start time" 0 n;
  let end_time = Unix.(gettimeofday () +. 60.0 |> gmtime) in
  let n = B.prune ~log:log_fn builder ~before:end_time 10 in
  Alcotest.(check int) "Prune" 2 n

let sexp = Alcotest.of_pp Sexplib.Sexp.pp_hum

let remove_line_indents = function
  | (_ :: x :: _) as lines ->
    let indent = Astring.String.find ((<>) ' ') x |> Option.value ~default:0 in
    lines |> List.map (fun line ->
        Astring.String.drop line ~sat:((=) ' ') ~max:indent
      )
  | x -> List.map String.trim x

let remove_indent s =
  String.split_on_char '\n' s
  |> remove_line_indents
  |> String.concat "\n"

(* Check that parsing an S-expression and then serialising it again gets the same result. *)
let test_sexp () =
  let test name s =
    let s = String.trim (remove_indent s) in
    let s1 = Sexplib.Sexp.of_string s in
    let spec = Spec.t_of_sexp s1 in
    let s2 = Spec.sexp_of_t spec in
    Alcotest.(check sexp) name s1 s2;
    Alcotest.(check string) name s (Fmt.str "%a" Spec.pp spec)
  in
  test "copy" {|
     ((build tools
             ((from base)
              (run (shell "make tools"))))
      (from base)
      (comment "A test comment")
      (workdir /src)
      (run (shell "a command"))
      (run (cache (a (target /data)) (b (target /srv)))
           (secrets (a (target /run/secrets/a)) (b (target /b)))
           (shell "a very very very very very very very very very very very very very very very long command"))
      (copy (src a b) (dst c))
      (copy (src a b) (dst c) (exclude .git _build))
      (copy (from (build tools)) (src binary) (dst /usr/local/bin/))
      (env DEBUG 1)
      (user (uid 1) (gid 2))
     )|}

let test_docker_unix () =
  let test ~buildkit name expect sexp =
    let spec = Spec.t_of_sexp (Sexplib.Sexp.of_string sexp) in
    let got = Obuilder_spec.Docker.dockerfile_of_spec ~buildkit ~os:`Unix spec in
    let expect = remove_indent expect in
    Alcotest.(check string) name expect got
  in
  test ~buildkit:false "Dockerfile"
    {| FROM base
       # A test comment
       WORKDIR /src
       RUN command1
       SHELL [ "/bin/sh", "-c" ]
       RUN command2 && \
           command3
       COPY a b c
       COPY a b c
       ENV DEBUG="1"
       ENV ESCAPE="\"quote\\sla\\sh\""
       USER 1:2
       COPY --chown=1:2 a b c
    |} {|
     ((from base)
      (comment "A test comment")
      (workdir /src)
      (run (shell "command1"))
      (shell /bin/sh -c)
      (run
       (cache (a (target /data))
              (b (target /srv)))
       (shell "command2 &&
               command3"))
      (copy (src a b) (dst c))
      (copy (src a b) (dst c) (exclude .git _build))
      (env DEBUG 1)
      (env ESCAPE "\"quote\\sla\\sh\"")
      (user (uid 1) (gid 2))
      (copy (src a b) (dst c))
     ) |};
  test ~buildkit:true "BuildKit"
    {| FROM base
       # A test comment
       WORKDIR /src
       RUN command1
       SHELL [ "/bin/sh", "-c" ]
       RUN --mount=type=cache,id=a,target=/data,uid=0 --mount=type=cache,id=b,target=/srv,uid=0 command2
       COPY a b c
       COPY a b c
       ENV DEBUG="1"
       USER 1:2
       COPY --chown=1:2 a b c
    |} {|
     ((from base)
      (comment "A test comment")
      (workdir /src)
      (run (shell "command1"))
      (shell /bin/sh -c)
      (run
       (cache (a (target /data))
              (b (target /srv)))
       (shell "command2"))
      (copy (src a b) (dst c))
      (copy (src a b) (dst c) (exclude .git _build))
      (env DEBUG 1)
      (user (uid 1) (gid 2))
      (copy (src a b) (dst c))
     ) |};
  test ~buildkit:false "Multi-stage"
    {| FROM base as tools
       RUN make tools

       FROM base
       COPY --from=tools binary /usr/local/bin/
    |} {|
     ((build tools
             ((from base)
              (run (shell "make tools"))))
      (from base)
      (copy (from (build tools)) (src binary) (dst /usr/local/bin/))
     ) |};
  test ~buildkit:true "Secrets"
    {| FROM base as tools
       RUN make tools

       FROM base
       RUN --mount=type=secret,id=a,target=/secrets/a,uid=0 --mount=type=secret,id=b,target=/secrets/b,uid=0 command1
    |} {|
     ((build tools
            ((from base)
             (run (shell "make tools"))))
      (from base)
      (run
       (secrets (a (target /secrets/a))
                (b (target /secrets/b)))
       (shell "command1"))
     ) |}

let test_docker_windows () =
  let test ~buildkit name expect sexp =
    let spec = Spec.t_of_sexp (Sexplib.Sexp.of_string sexp) in
    let got = Obuilder_spec.Docker.dockerfile_of_spec ~buildkit ~os:`Windows spec in
    let expect = remove_indent expect in
    Alcotest.(check string) name expect got
  in
  test ~buildkit:false "Dockerfile"
    {| #escape=`
       FROM base
       # A test comment
       WORKDIR C:/src
       RUN command1
       SHELL [ "C:/Windows/System32/cmd.exe", "/c" ]
       RUN command2 && `
           command3
       COPY a b c
       COPY a b c
       ENV DEBUG="1"
       USER Zaphod
       COPY a b c
    |} {|
     ((from base)
      (comment "A test comment")
      (workdir C:/src)
      (run (shell "command1"))
      (shell C:/Windows/System32/cmd.exe /c)
      (run
       (cache (a (target /data))
              (b (target /srv)))
       (shell "command2 &&
               command3"))
      (copy (src a b) (dst c))
      (copy (src a b) (dst c) (exclude .git _build))
      (env DEBUG 1)
      (user (name Zaphod))
      (copy (src a b) (dst c))
     ) |};
  test ~buildkit:false "Multi-stage"
    {| #escape=`
       FROM base as tools
       RUN make tools

       FROM base
       COPY --from=tools binary /usr/local/bin/
    |} {|
     ((build tools
             ((from base)
              (run (shell "make tools"))))
      (from base)
      (copy (from (build tools)) (src binary) (dst /usr/local/bin/))
     ) |}

let manifest =
  Alcotest.result
    (Alcotest.testable
       (fun f x -> Sexplib.Sexp.pp_mach f (Manifest.sexp_of_t x))
       (fun a b -> Manifest.sexp_of_t a = Manifest.sexp_of_t b))
    (Alcotest.of_pp (fun f (`Msg m) -> Fmt.string f m))

(* Test copy step. *)
let test_copy generate =
  let tmp_dir = Filename.temp_file "test-copy-src-" "" in
  Unix.unlink tmp_dir;
  Unix.mkdir tmp_dir 0o755;
  let src_dir = tmp_dir in
  Fun.protect ~finally:(fun () ->
    let _ = Sys.command (Printf.sprintf "rm -rf %S" tmp_dir) in ()
  ) @@ fun () ->
  let oc = open_out_bin (src_dir / "file") in
  output_string oc "file-data";
  close_out oc;
  let root = if Sys.unix then "/root" else "C:/Windows" in
  (* Files *)
  let f1hash = Sha256.string "file-data" in
  let r = generate ~exclude:[] ~src_dir "file" in
  Alcotest.(check manifest) "File" (Ok (`File ("file", f1hash))) r;
  let r = generate ~exclude:[] ~src_dir "./file" in
  Alcotest.(check manifest) "File relative" (Ok (`File ("file", f1hash))) r;
  let r = generate ~exclude:[] ~src_dir "/file" in
  Alcotest.(check manifest) "File absolute" (Ok (`File ("file", f1hash))) r;
  let r = generate ~exclude:[] ~src_dir "file2" in
  Alcotest.(check manifest) "Missing" (Error (`Msg {|Source path "file2" not found|})) r;
  let r = generate ~exclude:[] ~src_dir "file/file2" in
  Alcotest.(check manifest) "Not dir" (Error (`Msg {|Not a directory: file (in "file/file2")|})) r;
  let r = generate ~exclude:[] ~src_dir "../file" in
  Alcotest.(check manifest) "Parent" (Error (`Msg {|Can't use .. in source paths! (in "../file")|})) r;
  (* Symlinks *)
  Unix.symlink ~to_dir:true root (src_dir / "link");
  let r = generate ~exclude:[] ~src_dir "link" in
  Alcotest.(check manifest) "Link" (Ok (`Symlink (("link", root)))) r;
  let r = generate ~exclude:[] ~src_dir "link/file" in
  Alcotest.(check manifest) "Follow link" (Error (`Msg {|Not a regular file: link (in "link/file")|})) r;
  (* Directories *)
  let r = generate ~exclude:["file"] ~src_dir "" in
  Alcotest.(check manifest) "Tree"
    (Ok (`Dir ("", [`Symlink ("link", root)]))) r;
  let r = generate ~exclude:[] ~src_dir "." in
  Alcotest.(check manifest) "Tree"
    (Ok (`Dir ("", [`File ("file", f1hash);
                    `Symlink ("link", root)]))) r;
  Unix.mkdir (src_dir / "dir1") 0o700;
  Unix.mkdir (src_dir / "dir1" / "dir2") 0o700;
  let oc = open_out_bin (src_dir / "dir1" / "dir2" / "file2") in
  output_string oc "file2";
  close_out oc;
  let f2hash = Sha256.string "file2" in
  let r = generate ~exclude:[] ~src_dir "dir1/dir2/file2" in
  Alcotest.(check manifest) "Nested file" (Ok (`File ("dir1/dir2/file2", f2hash))) r;
  let r = generate ~exclude:[] ~src_dir "dir1" in
  Alcotest.(check manifest) "Tree"
    (Ok (`Dir ("dir1", [`Dir ("dir1/dir2", [`File ("dir1/dir2/file2", f2hash)])]))) r

(* Test the Manifest module. *)
let test_copy_ocaml () =
  if Sys.win32 then
    Alcotest.skip ();
  test_copy (fun ~exclude ~src_dir src -> Manifest.generate ~exclude ~src_dir src)

let test_cache_id () =
  let check expected id =
    Alcotest.(check string) ("ID-" ^ id) expected (Escape.cache id)
  in
  check "c-ok" "ok";
  check "c-" "";
  check "c-123" "123";
  check "c-a1" "a1";
  check "c-a%2f1" "a/1";
  check "c-.." "..";
  check "c-%2520" "%20";
  check "c-foo%3abar" "foo:bar";
  check "c-Az09-id.foo_orig" "Az09-id.foo_orig"

let test_secrets_not_provided ~sw ~env () =
  with_config ~sw ~env @@ fun ~sw:_ ~proc_mgr ~src_dir ~store:_ ~sandbox ~builder ->
  let log = Log.create "b" in
  let context = Context.v ~src_dir ~log:(Log.add log) () in
  let spec = Spec.(stage ~from:"base" [ run ~secrets:[Secret.v ~target:"/run/secrets/test" "test"] "Append" ]) in
  Mock_sandbox.expect sandbox (mock_op ~output:(`Append ("runner", "base-id")) ());
  let result = B.build ~proc_mgr builder context spec in
  Alcotest.(check build_result) "Final result" (Error (`Msg "Couldn't find value for requested secret 'test'")) result

let test_secrets_simple ~sw ~env () =
  with_config ~sw ~env @@ fun ~sw:_ ~proc_mgr ~src_dir ~store:_ ~sandbox ~builder ->
  let log = Log.create "b" in
  let context = Context.v ~src_dir ~log:(Log.add log) ~secrets:["test", "top secret value"; "test2", ""] () in
  let spec = Spec.(stage ~from:"base" [ run ~secrets:[Secret.v ~target:"/testsecret" "test"; Secret.v "test2"] "Append" ]) in
  Mock_sandbox.expect sandbox (mock_op ~output:(`Append ("runner", "base-id")) ());
  let result = B.build ~proc_mgr builder context spec in
  Alcotest.(check build_result) "Final result" (Ok ()) (Result.map (fun _ -> ()) result);
  Log.check "Check b log"
    (sprintf {| (from base)
        ;---> saved as ".*"
         %s: (run (secrets (test (target /testsecret)) (test2 (target /run/secrets/test2)))
         [ ]+(shell Append))
         Append
        ;---> saved as ".*"
       |} root)
    log

let test_exec_nul () =
  with_default_exec @@ fun () ->
  let args = ["dummy"; "stdout"] in
  Os.exec ~stdout:`Dev_null ~stderr:`Dev_null args;
  let args = ["dummy"; "stderr"] in
  Os.exec ~stdout:`Dev_null ~stderr:`Dev_null args

let test_pread_nul () =
  with_default_exec @@ fun () ->
  let expected = "the quick brown fox jumps over the lazy dog" in
  let args = ["dummy"; "stdout"] in
  let actual = Os.pread ~stderr:`Dev_null args in
  Alcotest.(check string) "stdout" actual expected

let test_case name speed fn =
  Alcotest.test_case name speed @@ fun () ->
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  fn ~sw ~env ()

let () =
  Alcotest.run "OBuilder" [
    "spec", [
      Alcotest.test_case "Sexp"     `Quick test_sexp;
      Alcotest.test_case "Cache ID" `Quick test_cache_id;
      Alcotest.test_case "Docker Windows" `Quick test_docker_windows;
      Alcotest.test_case "Docker UNIX"    `Quick test_docker_unix;
    ];
    "manifest", [
      Alcotest.test_case "Copy using Manifest" `Quick test_copy_ocaml;
    ];
    "process", [
      Alcotest.test_case "Execute a process" `Quick test_exec_nul;
      Alcotest.test_case "Read stdout of a process" `Quick test_pread_nul;
    ];
    "build", [
      test_case "Simple"     `Quick test_simple;
      test_case "Prune"      `Quick test_prune;
    ];
    "secrets", [
      test_case "Simple"     `Quick test_secrets_simple;
      test_case "No secret provided" `Quick test_secrets_not_provided;
    ];
  ]
