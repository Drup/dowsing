(** Import using LibIndex *)

val iter : Fpath.t -> (Longident.t * Index.Info.t) Iter.t
(** [iter dir] iterates over all entries available in
    the various [*.cm*] files available in [dir]. *)
