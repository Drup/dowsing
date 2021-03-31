module Pure : sig
  type t =
    | Var of Variable.t
    | Constant of Type.Longident.t

  type term

  val pure : t -> term
  val tuple : t array -> term

  val pp : string Variable.HMap.t -> t Fmt.t [@@ocaml.toplevel_printer]
  val pp_term : string Variable.HMap.t -> term Fmt.t [@@ocaml.toplevel_printer]
  val as_typexpr : term -> Type.t
end

module Env : sig
  type t

  val make : ?gen:Variable.Gen.t -> unit -> t
  val copy : t -> t
  val pp : string Variable.HMap.t -> t Fmt.t [@@ocaml.toplevel_printer]
end

module Unifier : sig
  type t
  val pp : string Variable.HMap.t -> t Fmt.t [@@ocaml.toplevel_printer]
end

module System : sig
  type t
  val pp : t Fmt.t [@@ocaml.toplevel_printer]
end

val unify : Type.Env.t -> (Type.t * Type.t) List.t -> Unifier.t Iter.t

val unifiable : Type.Env.t -> (Type.t * Type.t) List.t -> Bool.t
