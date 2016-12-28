
module P : sig
  type t = Longident.t

  val compare : t -> t -> int
end

module rec Ty : sig
  type t =
    | Var of int
    | Constr of P.t * t array
    | Arrow of NSet.t * t
    | Tuple of NSet.t
    | Unknown of Outcometree.out_type
end
and NSet : Custom_set.S with type elt = Ty.t

type t = Ty.t
val compare : t -> t -> int
val equal : t -> t -> bool

val of_outcometree : Outcometree.out_type -> t
