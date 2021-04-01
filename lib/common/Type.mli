module rec Base : sig

  type t =
    | Var of Variable.t
    | Constr of LongIdent.t * t Array.t
    | Arrow of Set.t * t
    | Tuple of Set.t
    | Other of Int.t

  val compare : t -> t -> Int.t
  val equal : t -> t -> Bool.t

end

and Set : sig

  type elt = Base.t
  type t

  val compare : t -> t -> Int.t

  val empty : t
  val is_empty : t -> Bool.t
  val singleton : elt -> t
  val union : t -> t -> t
  val add : elt -> t -> t
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a

  val of_list : elt List.t -> t
  val of_iter : elt Iter.t -> t
  val to_iter : t -> elt Iter.t
  val as_array : t -> elt Array.t

  val pp : elt Fmt.t -> t Fmt.t

end

include module type of Base with type t = Base.t

module Hashcons : sig

  type t

  val make : Int.t -> t
  val hashcons : t -> Base.t -> Base.t

end

module Env : sig

  type t = {
    var_gen : Variable.Gen.t ;
    var_names : String.t Variable.HMap.t ;
    hcons : Hashcons.t ;
  }

  val make : Unit.t -> t

end

(* importation functions *)

val of_outcometree : Env.t -> Outcometree.out_type -> t
val of_parsetree : Env.t -> Parsetree.core_type -> t
val of_lexing : Env.t -> Lexing.lexbuf -> t
val of_string : Env.t -> String.t -> t

(* utility functions *)

val vars : t -> Variable.t Iter.t

val head : t -> t

val pp : String.t Variable.HMap.t -> t Fmt.t
