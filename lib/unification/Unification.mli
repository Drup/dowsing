module Env : sig

  type t

  val make : Type.Env.t -> t
  val copy : t -> t

  val pp : t Fmt.t [@@ocaml.toplevel_printer]

end

module Unifier : sig

  type t

  val pp : String.t Variable.HMap.t -> t Fmt.t [@@ocaml.toplevel_printer]

end

(** [unifiers tyenv l] computes the unification for all equations in [l]
    and returns a lazy sequence of all potential unifiers.
*)
val unifiers : Type.Env.t -> (Type.t * Type.t) List.t -> Unifier.t Iter.t

(** [unify tyenv l] is as {!unifiers}, but returns only the smallest unifier. *)
val unify : Type.Env.t -> (Type.t * Type.t) List.t -> Unifier.t Option.t

(** [unifiable tyenv l] is as {!unifiers} but returns [true] if there exists
    a unifier, and [false] otherwise. *)
val unifiable : Type.Env.t -> (Type.t * Type.t) List.t -> Bool.t
