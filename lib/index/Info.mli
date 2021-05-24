module Signature : sig

  type t = {
    lid : LongIdent.t ;
    out_ty : Outcometree.out_type ;
  }

  val compare : t CCOrd.t

  val pp : t Fmt.t

end

type t

val update : Longident.t -> Signature.t -> t Option.t -> t

val pp : t Fmt.t [@@ocaml.toplevel_printer]
