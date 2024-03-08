(** Import using LibIndex *)

val iter : string -> Fpath.t -> Db.Entry.t Iter.t
(** [iter pkg dir] iterates over all entries available in
    the various [*.cm*] files available in [dir]. *)
