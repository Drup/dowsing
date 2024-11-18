module Kind : sig
  type t = Var | Constr | Arrow | Tuple | Other

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
  type t = Var | Constr of LongIdent.t | Arrow | Tuple | Other

  val to_int : t -> Int.t
  val to_string : t -> String.t
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
    (** Represents the types of the form [(a₁,...,aₙ) p] where [p] is a [Longident.t] *)
    | Arrow of Tuple.t * t
    (** Represents the types of the form [(a₁,...,aₙ) -> r] *)
    | Tuple of Tuple.t
    (** Represents tuples [(a₁*...*aₙ)] *)
    | Other of Int.t

  val hash : t -> int
  val kind : t -> Kind.t
  val kind' : t -> Kind'.t
  val compare : t CCOrd.t
  val equal : t CCEqual.t
end

and Tuple : sig
  module Complet : sig
    type t

    val unit : t
    val mk_l : Base.t list -> t
    val is_unit : t -> bool
    val size : t -> int
    val add : t -> Base.t -> t
    val singleton : Base.t -> t
    val is_singleton : t -> Base.t option
    val fold : (Base.t -> 'a -> 'a) -> t -> 'a -> 'a
    val to_iter : t -> Base.t Iter.t
    val pp : Base.t Fmt.t -> t Fmt.t
  end

  module Partial : sig
    type t

    val mk : ?tuple:Complet.t -> unit -> t
    val add : t -> Base.t -> unit
    val add_n : t -> Base.t -> int -> unit
    val freeze : t -> Complet.t
  end

  include module type of Complet
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
  type t = { var_gen : Variable.Gen.t; hcons : Hashcons.t }

  val make : ?hcons:Hashcons.t -> unit -> t

  (** [restart env] returns a new env with a fresh variable generator *)
  val restart : t -> t
end

(* smart constructors *)

val dummy : t
val var : Env.t -> Variable.t -> t
val frozen_var : Env.t -> Variable.t -> t
val constr : Env.t -> LongIdent.t -> t Array.t -> t
val arrow : Env.t -> t -> t -> t
val arrows : Env.t -> Tuple.t -> t -> t
val arrows_flatten : Env.t -> Tuple.t -> t -> t
val tuple : Env.t -> Tuple.t -> t

(* utility *)

val freeze_variables : Env.t -> t -> t
val refresh_variables : Env.t -> t -> t
val is_arrow : t -> bool
val is_tuple : t -> bool
val is_non_arrow_var : t -> bool
val is_non_tuple_var : t -> bool

val tuple_flat_map : ( t -> t ) -> Tuple.t -> Tuple.t
(** This should be use only to create the arguments of an arrows. *)

val tuple_map_type : Env.t -> ( t -> t ) -> Tuple.t -> t

(* import functions *)

val of_outcometree : Env.t -> Outcometree.out_type -> t
val of_parsetree : Env.t -> Parsetree.core_type -> t
val of_lexing : Env.t -> Lexing.lexbuf -> t
val of_string : Env.t -> String.t -> t

val of_parsetree' : Env.t -> Parsetree.core_type -> Variable.t String.HMap.t * t
val of_lexing' : Env.t -> Lexing.lexbuf -> Variable.t String.HMap.t * t
val of_string' : Env.t -> String.t -> Variable.t String.HMap.t * t
val outcome_of_string : String.t -> Outcometree.out_type

(* utility functions *)

val head : t -> t
val tail : t -> Tuple.t
val iter : t -> t Iter.t
val iter_vars : t -> Variable.t Iter.t
val iter_consts : t -> LongIdent.t Iter.t
val variable_clash : Variable.t -> t -> bool

(* pretty printing *)

val pp : t Fmt.t [@@ocaml.toplevel_printer]
val pp_parens : t Fmt.t
