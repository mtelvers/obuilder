(** Store build results using overlayfs. *)

include S.STORE

val create : path:string -> t
(** [create ~path] creates a new overlayfs store where everything will
    be stored under [path]. *)
