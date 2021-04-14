module Kind : sig

  type t =
    | Var
    | Constr
    | Arrow
    | Tuple
    | Other

  val to_int : t -> Int.t
  val of_int : Int.t -> t

  val to_string : t -> String.t

  val compare : t -> t -> Int.t
  val equal : t -> t -> Bool.t
  val hash : t -> Int.t

  module Map : CCMap.S with type key = t
  module HMap : CCHashtbl.S with type key = t

  val pp : t Fmt.t

end

module rec Base : sig

  type t =
    | Var of Variable.t
    | Constr of LongIdent.t * t Array.t
    | Arrow of MSet.t * t
    | Tuple of MSet.t
    | Other of Int.t

  val kind : t -> Kind.t

  val compare : t -> t -> Int.t
  val equal : t -> t -> Bool.t

end

and MSet : sig

  type elt = Base.t
  type t

  val compare : t -> t -> Int.t

  val of_list : elt List.t -> t
  val of_iter : elt Iter.t -> t
  val to_iter : t -> elt Iter.t
  val as_array : t -> elt Array.t

  val empty : t
  val is_empty : t -> Bool.t
  val length : t -> Int.t
  val singleton : elt -> t
  val union : t -> t -> t
  val add : elt -> t -> t
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val map : (elt -> elt) -> t -> t
  val min_elt : t -> elt

  val pp : elt Fmt.t -> t Fmt.t

end

include module type of Base with type t = Base.t

module Map : CCMap.S with type key = Base.t

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

  val make : ?var_gen:Variable.Gen.t -> ?var_names: String.t Variable.HMap.t -> Unit.t -> t

end

(* importation functions *)

val of_outcometree : Env.t -> Outcometree.out_type -> t
val of_parsetree : Env.t -> Parsetree.core_type -> t
val of_lexing : Env.t -> Lexing.lexbuf -> t
val of_string : Env.t -> String.t -> t

(* utility functions *)

val head : t -> t
val tail : t -> MSet.t

val substitute : t Variable.Map.t -> t -> t

val vars : t -> Variable.t Iter.t

(* several notions of size *)

module Size : sig

  type kind =
    | VarCount
    | NodeCount
    | HeadKind
    | TailRootVarCount
    | RootVarCount
    | TailLength

  type t = Int.t

  val pp : kind -> t Fmt.t

  module Map : CCMap.S with type key = t
  module HMap : CCHashtbl.S with type key = t

end

val size : Size.kind -> t -> Size.t

(* pretty printing *)

val pp : String.t Variable.HMap.t -> t Fmt.t
