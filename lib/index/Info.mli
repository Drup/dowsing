type t = {
  lid : LongIdent.t ;
  out_ty : Outcometree.out_type ;
}

val compare : t CCOrd.t
val is_internal : t -> Bool.t
val pp : t Fmt.t [@@ocaml.toplevel_printer]
