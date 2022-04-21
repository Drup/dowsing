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

  val compare : t CCOrd.t
  val equal : t CCEqual.t
  val hash : t CCHash.t

  module Map : CCMap.S with type key = t
  module HMap : CCHashtbl.S with type key = t
  module MSet : CCMultiSet.S with type elt = t

  val pp : t Fmt.t [@@ocaml.toplevel_printer]

end

module Kind' : sig

  type t =
    | Var
    | Constr of LongIdent.t
    | Arrow
    | Tuple
    | Other

  val to_int : t -> Int.t

  val compare : t CCOrd.t
  val equal : t CCEqual.t
  val hash : t CCHash.t

  module Map : CCMap.S with type key = t
  module HMap : CCHashtbl.S with type key = t
  module MSet : CCMultiSet.S with type elt = t

end

module rec Base : sig

  type t = private
    | Var of Variable.t
    | FrozenVar of Variable.t
    | Constr of LongIdent.t * t Array.t
    | Arrow of NSet.t * t
    | Tuple of NSet.t
    | Other of Int.t

  val kind : t -> Kind.t
  val kind' : t -> Kind'.t

  val compare : t CCOrd.t
  val equal : t CCEqual.t

end

and NSet : sig

  type elt = Base.t
  type t

  val compare : t CCOrd.t

  val of_list : elt List.t -> t
  val of_iter : elt Iter.t -> t
  val to_iter : t -> elt Iter.t
  val as_array : t -> elt Array.t

  val empty : t
  val is_empty : t -> Bool.t
  val length : t -> Int.t
  val singleton : elt -> t
  val is_singleton : t -> elt Option.t
  val union : t -> t -> t
  val add : elt -> t -> t
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val map : (elt -> elt) -> t -> t

  val pp : elt Fmt.t -> t Fmt.t

end

include module type of Base with type t = Base.t

module HMap : CCHashtbl.S with type key = t
module Map : CCMap.S with type key = t
module Set : CCSet.S with type elt = t
module MSet : CCMultiSet.S with type elt = t

module Hashcons : sig

  type elt = t
  type t

  val make : Unit.t -> t
  val hashcons : t -> elt -> elt

end

module Env : sig

  type t = {
    var_gen : Variable.Gen.t ;
    hcons : Hashcons.t ;
  }

  val make : ?hcons:Hashcons.t -> Variable.Namespace.t -> t

end

(* smart constructors *)

val var : Env.t -> Variable.t -> t
val constr : Env.t -> LongIdent.t -> t Array.t -> t
val arrow : Env.t -> t -> t -> t
val arrows : Env.t -> NSet.t -> t -> t
val tuple : Env.t -> NSet.t -> t

(* freeze *)

val freeze_variables : Env.t -> t -> t

(* importation functions *)

val of_outcometree : Env.t -> Outcometree.out_type -> t
val of_parsetree : Env.t -> Parsetree.core_type -> t
val of_lexing : Env.t -> Lexing.lexbuf -> t
val of_string : Env.t -> String.t -> t

val of_outcometree' : Env.t -> Outcometree.out_type -> Variable.t String.HMap.t * t
val of_parsetree' : Env.t -> Parsetree.core_type -> Variable.t String.HMap.t * t
val of_lexing' : Env.t -> Lexing.lexbuf -> Variable.t String.HMap.t * t
val of_string' : Env.t -> String.t -> Variable.t String.HMap.t * t

(* utility functions *)

val head : t -> t
val tail : t -> NSet.t

val iter : t -> t Iter.t
val iter_vars : t -> Variable.t Iter.t

(* pretty printing *)

val pp : t Fmt.t [@@ocaml.toplevel_printer]
val pp_parens : t Fmt.t
