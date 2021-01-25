module T = Typexpr
module Var = Variables

module Pure : sig
  type t =
    | Var of Var.t
    | Constant of T.P.t

  type term

  val pure : t -> term
  val tuple : t array -> term
  
  val pp : string Var.HMap.t -> t Fmt.t [@@ocaml.toplevel_printer]
  val pp_term : string Var.HMap.t -> term Fmt.t [@@ocaml.toplevel_printer]
  val as_typexpr : term -> Typexpr.t
end

module Env : sig
  type t

  val make : ?gen:Var.gen -> unit -> t
  val copy : t -> t
  val pp : string Var.HMap.t -> t Fmt.t [@@ocaml.toplevel_printer]
end

module Unifier : sig
  type t
  val pp : string Var.HMap.t -> t Fmt.t [@@ocaml.toplevel_printer]
end

module System : sig
  type t
  val pp : t Fmt.t [@@ocaml.toplevel_printer]
end

val unify :
  ?gen:Var.gen ->
  string Var.HMap.t ->
  (T.t * T.t) list -> Unifier.t Iter.t
(** All-in one function *)
