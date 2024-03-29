val unifiers : Type.Env.t -> Type.t -> Type.t -> Subst.t Iter.t
(** [unifiers tyenv t1 t2] computes the unification for all equations in [l]
    and returns a lazy sequence of all potential unifiers.
*)

val unify : Type.Env.t -> Type.t -> Type.t -> Subst.t Option.t
(** [unify tyenv t1 t2] is as {!unifiers}, but returns only the smallest unifier. *)

val unifiable : Type.Env.t -> Type.t -> Type.t -> Bool.t
(** [unifiable tyenv t1 t2] is as {!unifiers} but returns [true] if there exists
    a unifier, and [false] otherwise. *)
