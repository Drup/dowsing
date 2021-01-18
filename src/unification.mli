module T = Typexpr
module Var = Variables

module Pure : sig
  type t =
    | Var of Var.t
    | Constant of T.P.t

  type term = t array

  val pp : t Fmt.t
  val pp_term : term Fmt.t
  val as_typexpr : t array -> Typexpr.t
end

module Env : sig
  type t

  val make : ?gen:Var.gen -> unit -> t
  val copy : t -> t
  val pp : t Fmt.t
end

module Unifier : sig
  type t = Bitv.Default.t * Pure.t array Var.HMap.t

  val pp : t Fmt.t
end

module System : sig
  type t
  val pp : t Fmt.t
end

exception FailUnif

val fail : unit -> 'a

val insert : Env.t -> T.t -> T.t -> unit

val occur_check : Env.t -> bool

val occur_check_or_fail : Env.t -> unit

val get_system : Env.t -> System.t

val solve_system : Env.t -> System.t -> Unifier.t Iter.t

val unify :
  ?gen:Var.gen ->
  (T.t * T.t) list -> Unifier.t Iter.t
(** All-in one function *)
