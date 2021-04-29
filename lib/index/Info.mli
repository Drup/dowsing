module Signature : sig

  type t = {
    lid : LongIdent.t ;
  }
  val compare : t CCOrd.t
  val pp : t Fmt.t

end

type t

val add : Longident.t -> Signature.t -> t -> t
val update : Longident.t -> Signature.t -> t option -> t

val pp : t Fmt.t
