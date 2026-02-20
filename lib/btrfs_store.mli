(** Store build results as Btrfs subvolumes. *)

include S.STORE

val create : ?proc_mgr:[`Generic] Eio.Process.mgr_ty Eio.Resource.t -> string -> t
(** [create ?proc_mgr path] is a new store in btrfs directory [path]. *)
