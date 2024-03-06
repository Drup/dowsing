(** Import using LibIndex *)

val iter : Fpath.t List.t -> (Longident.t * Index.Info.t) Iter.t
(** [iter dirs] iterates over all entries available in
    the various [*.cm*] files available in [dirs]. *)
