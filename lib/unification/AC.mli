(** Elementary AC-Unif *)

val solve :
  Env.t -> ACTerm.problem list ->
  (Type.t * Type.t) Iter.t Iter.t
