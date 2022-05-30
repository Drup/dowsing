module Env : sig
  type t

  val make : tyenv:Type.Env.t -> orig_vars:Variable.Set.t -> t
  val copy : t -> t
  val pp : t Fmt.t [@@ocaml.toplevel_printer]
end

val unifiers : Type.Env.t -> Type.t -> Type.t -> Subst.t Iter.t
(** [unifiers tyenv t1 t2] computes the unification for all equations in [l]
    and returns a lazy sequence of all potential unifiers.
*)

val unify : Type.Env.t -> Type.t -> Type.t -> Subst.t Option.t
(** [unify tyenv t1 t2] is as {!unifiers}, but returns only the smallest unifier. *)

val unifiable : Type.Env.t -> Type.t -> Type.t -> Bool.t
(** [unifiable tyenv t1 t2] is as {!unifiers} but returns [true] if there exists
    a unifier, and [false] otherwise. *)

type ord = Uncomparable | Smaller | Bigger | Equal

val pp_ord : Format.formatter -> ord -> unit

val compare :
  ?compat_leq:bool -> ?compat_geq:bool -> Type.Env.t -> Type.t -> Type.t -> ord
