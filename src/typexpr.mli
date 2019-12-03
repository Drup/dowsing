(** Type expressions *)

module P : sig
  type t = Longident.t

  val pp : Format.formatter -> t -> unit
  val compare : t -> t -> int
  val unit : t

  module Map : CCTrie.S with type key = t
  module HMap : CCHashtbl.S with type key = t
end

(** The skeleton of type expressions,
    with parametrized variable and set. *)
type ('v, 's) skel =
  | Var of 'v
  | Constr of P.t * ('v, 's) skel array
  | Tuple of 's
  | Unknown of int
  | Unit
  | Arrow of 's * ('v, 's) skel

(** Non-normalized type expressions.

    The set of smart constructors normalize partially
    the structure.

    Use {!normalize} to get a normalized type expression.
*)
module Raw : sig

  type t = private (string option, set) skel
  and set = S of t list

  val arrow : t -> t -> t
  val tuple : t list -> t
  val unknown : 'a -> t
  val constr : P.t -> t array -> t

  val var : string option -> t
end

module rec Nf : (Set.OrderedType with type t = (Variables.t, NSet.t) skel)
and NSet : sig
  include Custom_set.S with type elt = Nf.t
  val as_array : t -> elt array
end

type t = Nf.t
val compare : t -> t -> int
val equal : t -> t -> bool

module HC : Hashcons.S with type key = t

val normalize : ?gen:Variables.gen -> ?ht:HC.t -> Raw.t -> t

val vars : Nf.t -> Variables.t Iter.t

module Head : sig
  type t =
    | Var
    | Constr of P.t
    | Tuple
    | Other
    | Unit

  val get : Nf.t -> t
end

val pp : Format.formatter -> t -> unit
