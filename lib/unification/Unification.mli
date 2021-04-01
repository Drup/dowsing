module T = Type
module Var = Variable

module Env : sig
  type t

  val make : ?gen:Var.Gen.t -> unit -> t
  val copy : t -> t
  val pp : string Var.HMap.t -> t Fmt.t [@@ocaml.toplevel_printer]
end

module Unifier : sig
  type t
  val pp : string Var.HMap.t -> t Fmt.t [@@ocaml.toplevel_printer]
end

val unify : Type.Env.t -> (Type.t * Type.t) List.t -> Unifier.t Iter.t

val unifiable : Type.Env.t -> (Type.t * Type.t) List.t -> bool
