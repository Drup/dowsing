module Env : sig

  type t

  val make : Type.Env.t -> t
  val copy : t -> t

  val pp : t Fmt.t [@@ocaml.toplevel_printer]

end

module Subst : sig

  type t

  val size : t -> Int.t
  val compare : t -> t -> Int.t

  val pp : t Fmt.t [@@ocaml.toplevel_printer]

end

(** [unifiers tyenv t1 t2] computes the unification for all equations in [l]
    and returns a lazy sequence of all potential unifiers.
*)
val unifiers : Type.Env.t -> Type.t -> Type.t -> Subst.t Iter.t

(** [unify tyenv t1 t2] is as {!unifiers}, but returns only the smallest unifier. *)
val unify : Type.Env.t -> Type.t -> Type.t -> Subst.t Option.t

(** [unifiable tyenv t1 t2] is as {!unifiers} but returns [true] if there exists
    a unifier, and [false] otherwise. *)
val unifiable : Type.Env.t -> Type.t -> Type.t -> Bool.t
