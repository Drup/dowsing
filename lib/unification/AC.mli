(** Elementary AC-Unif *)

val solve :
  Env.t -> ACTerm.problem list ->
  (Bitv.t * ACTerm.t Variable.HMap.t) Iter.t
