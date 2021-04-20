(** Elementary AC-Unif *)

val solve :
  Env.t -> ACTerm.problem list ->
  (Variable.t * ACTerm.t) Iter.t Iter.t
