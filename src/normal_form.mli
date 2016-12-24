
module P : sig
  type t = Longident.t

  val compare : t -> t -> int
end

module rec Ty : sig
  type t =
    | Var of string
    | Constr of P.t * t list
    | Arrow of NSet.t * t
    | Tuple of NSet.t
    | Unknown of Outcometree.out_type
end

and NSet : Set.S with type elt = Ty.t

type t = Ty.t
val compare : t -> t -> int
