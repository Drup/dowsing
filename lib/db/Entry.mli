type desc =
  | Val of Outcometree.out_type
  | Type of {
      params : string option list ;
      manifest : Outcometree.out_type option ;
    }

type t = {
  lid : LongIdent.t ;
  desc : desc ;
  pkg : string ;
  source_file : Fpath.t ;
}

val compare : t CCOrd.t
val is_internal : t -> Bool.t
val pp : t Fmt.t [@@ocaml.toplevel_printer]
