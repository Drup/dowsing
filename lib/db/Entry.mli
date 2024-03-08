type t = {
  lid : LongIdent.t ;
  ty : Outcometree.out_type ;
  pkg : string ;
  pkg_dir : Fpath.t ;
}

val compare : t CCOrd.t
val is_internal : t -> Bool.t
val pp : t Fmt.t [@@ocaml.toplevel_printer]
