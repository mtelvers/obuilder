open Sexplib.Conv

let ( / ) = Filename.concat

(* Check if a switch has been cancelled *)
let switch_is_cancelled _sw =
  try Eio.Fiber.check (); false
  with Eio.Cancel.Cancelled _ -> true

let copy_to_log ~src ~dst =
  let buf = Bytes.create 4096 in
  let rec aux () =
    match Unix.read src buf 0 (Bytes.length buf) with
    | 0 -> ()
    | n -> Build_log.write dst (Bytes.sub_string buf 0 n); aux ()
  in
  aux ()

type guest_os =
  | Linux
  | OpenBSD
  | Windows
[@@deriving sexp]

type guest_arch =
  | Amd64
  | Riscv64
[@@deriving sexp]

type t = {
  qemu_cpus : int;
  qemu_memory : int;
  qemu_guest_os : guest_os;
  qemu_guest_arch : guest_arch;
  qemu_boot_time : int;
}

type config = {
  cpus : int;
  memory : int;
  guest_os : guest_os;
  guest_arch : guest_arch;
  boot_time : int;
} [@@deriving sexp]

let get_free_port () =
  let fd = Unix.socket PF_INET SOCK_STREAM 0 in
  let () = Unix.bind fd (ADDR_INET(Unix.inet_addr_loopback, 0)) in
  let sa = Unix.getsockname fd in
  let () = Unix.close fd in
  match sa with
  | ADDR_INET (_, n) -> string_of_int n
  | ADDR_UNIX _ -> assert false;;

let run ~sw ?stdin ~log t config result_tmp =
  let pp f = Os.pp_cmd f ("", config.Config.argv) in

  let extra_mounts = List.map (fun { Config.Mount.src; _ } ->
    ["-drive"; "file=" ^ src / "rootfs" / "image.qcow2" ^ ",if=virtio"]
  ) config.Config.mounts |> List.flatten in

  Os.with_pipe_to_child @@ fun ~r:qemu_r ~w:qemu_w ->
  let qemu_stdin = `FD_move_safely qemu_r in
  let port = get_free_port () in
  let qemu_binary = match t.qemu_guest_arch with
    | Amd64 -> [ "qemu-system-x86_64"; "-machine"; "accel=kvm,type=pc"; "-cpu"; "host"; "-display"; "none";
                 "-device"; "virtio-net,netdev=net0" ]
    | Riscv64 -> [ "qemu-system-riscv64"; "-machine"; "type=virt"; "-nographic";
                   "-bios"; "/usr/lib/riscv64-linux-gnu/opensbi/generic/fw_jump.bin";
                   "-kernel"; "/usr/lib/u-boot/qemu-riscv64_smode/uboot.elf";
                   "-device"; "virtio-net-device,netdev=net0";
                   "-serial"; "none"] in
  let cmd = qemu_binary @ [
              "-monitor"; "stdio";
              "-m"; (string_of_int t.qemu_memory) ^ "G";
              "-smp"; string_of_int t.qemu_cpus;
              "-netdev"; "user,id=net0,hostfwd=tcp::" ^ port ^ "-:22";
              "-drive"; "file=" ^ result_tmp / "rootfs" / "image.qcow2" ^ ",if=virtio" ]
              @ extra_mounts in
  let qemu_pid, proc = Os.open_process ~stdin:qemu_stdin ~stdout:`Dev_null ~pp cmd in

  let ssh = ["ssh"; "opam@localhost"; "-p"; port; "-o"; "NoHostAuthenticationForLocalhost=yes"] in

  let rec loop = function
    | 0 -> Error (`Msg "No connection")
    | n ->
      match Os.exec_result ~pp (ssh @ ["exit"]) with
      | Ok _ -> Ok ()
      | _ -> Unix.sleepf 1.; loop (n - 1) in
  Unix.sleepf 5.;
  let _ = loop t.qemu_boot_time in

  List.iteri (fun i { Config.Mount.dst; _ } ->
    match t.qemu_guest_os with
    | Linux ->
      let dev = Printf.sprintf "/dev/vd%c1" (Char.chr (Char.code 'b' + i)) in
      Os.exec (ssh @ ["sudo"; "mount"; dev; dst])
    | OpenBSD ->
      let dev = Printf.sprintf "/dev/sd%ca" (Char.chr (Char.code '1' + i)) in
      Os.exec (ssh @ ["doas"; "fsck"; "-y"; dev]);
      Os.exec (ssh @ ["doas"; "mount"; dev; dst])
    | Windows ->
      Os.exec (ssh @ ["cmd"; "/c"; "rmdir /s /q '" ^ dst ^ "'"]);
      let drive_letter = String.init 1 (fun _ -> Char.chr (Char.code 'd' + i)) in
      Os.exec (ssh @ ["cmd"; "/c"; "mklink /j '" ^ dst ^ "' '" ^ drive_letter ^ ":\\'"])
  ) config.Config.mounts;

  Os.with_pipe_from_child @@ fun ~r:out_r ~w:out_w ->
  let stdin = Option.map (fun x -> `FD_move_safely x) stdin in
  let stdout = `FD_move_safely out_w in
  let stderr = stdout in
  let env = List.map (fun (k, v) -> k ^ "=" ^ v) config.Config.env |> Array.of_list in
  let sendenv = if Array.length env > 0 then List.map (fun (k, _) -> ["-o"; "SendEnv=" ^ k]) config.Config.env |> List.flatten else [] in
  let _, proc2 = Os.open_process ~env ?stdin ~stdout ~stderr ~pp (ssh @ sendenv @ ["cd"; config.Config.cwd; "&&"] @ config.Config.argv) in

  (* Handle cancellation via switch *)
  let cancelled = switch_is_cancelled sw in
  let qemu_write_quit () =
    let quit_cmd = "quit\n" in
    ignore (Unix.write qemu_w (Bytes.of_string quit_cmd) 0 (String.length quit_cmd))
  in

  let res = Os.process_result ~pp proc2 in
  copy_to_log ~src:out_r ~dst:log;

  (match t.qemu_guest_arch with
    | Amd64 ->
      Log.info (fun f -> f "Sending QEMU an ACPI shutdown event");
      let cmd = "system_powerdown\n" in
      ignore (Unix.write qemu_w (Bytes.of_string cmd) 0 (String.length cmd))
    | Riscv64 ->
      (* QEMU RISCV does not support ACPI until >= v9 *)
      Log.info (fun f -> f "Shutting down the VM");
      Os.exec (ssh @ ["sudo"; "poweroff"]));

  let rec wait_loop = function
  | 0 ->
    Log.warn (fun f -> f "Powering off QEMU");
    qemu_write_quit ()
  | n ->
    (* Check if process is still running by trying waitpid with WNOHANG *)
    begin try
      let pid, _ = Unix.waitpid [Unix.WNOHANG] qemu_pid in
      if pid = 0 then begin
        (* Process still running *)
        Unix.sleepf 1.;
        wait_loop (n - 1)
      end
      (* else process has finished *)
    with Unix.Unix_error _ ->
      (* Process not found, probably finished *)
      ()
    end
  in
  wait_loop t.qemu_boot_time;

  let _ = Os.process_result ~pp proc in

  if cancelled then Error `Cancelled
  else (res :> (unit, [`Msg of string | `Cancelled]) result)

let create (c : config) =
  { qemu_cpus = c.cpus; qemu_memory = c.memory; qemu_guest_os = c.guest_os; qemu_guest_arch = c.guest_arch; qemu_boot_time = c.boot_time }

let finished () = ()

let shell _ = Some []

let tar t =
  match t.qemu_guest_os with
  | Linux -> None
  | OpenBSD -> Some ["gtar"; "-xf"; "-"]
  | Windows -> Some ["/cygdrive/c/Windows/System32/tar.exe"; "-xf"; "-"; "-C"; "/"]

open Cmdliner

let docs = "QEMU BACKEND"

let cpus =
  Arg.value @@
  Arg.opt Arg.int 2 @@
  Arg.info ~docs
    ~doc:"Number of CPUs to be used by each QEMU machine."
    ~docv:"CPUS"
    ["qemu-cpus"]

let memory =
  Arg.value @@
  Arg.opt Arg.int 2 @@
  Arg.info ~docs
    ~doc:"The amount of memory allocated to the VM in gigabytes."
    ~docv:"MEMORY"
    ["qemu-memory"]

let guest_os =
  let options =
    [("linux", Linux);
     ("openbsd", OpenBSD);
     ("windows", Windows)] in
  Arg.value @@
  Arg.opt Arg.(enum options) Linux @@
  Arg.info ~docs
    ~doc:(Printf.sprintf "Set OS used by QEMU guest. $(docv) must be %s." (Arg.doc_alts_enum options))
    ~docv:"GUEST_OS"
    ["qemu-guest-os"]

let guest_arch =
  let options =
    [("amd64", Amd64);
     ("riscv64", Riscv64)] in
  Arg.value @@
  Arg.opt Arg.(enum options) Amd64 @@
  Arg.info ~docs
    ~doc:(Printf.sprintf "Set system architecture used by QEMU guest. $(docv) must be %s." (Arg.doc_alts_enum options))
    ~docv:"GUEST_OS"
    ["qemu-guest-arch"]

let boot_time =
  Arg.value @@
  Arg.opt Arg.int 30 @@
  Arg.info ~docs
    ~doc:"The maximum time in seconds to wait for the machine to boot/power off."
    ~docv:"BOOT_TIME"
    ["qemu-boot-time"]

let cmdliner : config Term.t =
  let make cpus memory guest_os guest_arch boot_time =
    { cpus; memory; guest_os; guest_arch; boot_time }
  in
  Term.(const make $ cpus $ memory $ guest_os $ guest_arch $ boot_time)
