(** Store build results using qemu-img. *)

include S.STORE

val create : root:string -> t
(** [create ~path] creates a new qemu store where everything will
    be stored under [path]. *)
