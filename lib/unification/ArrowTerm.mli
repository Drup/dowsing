(** Term and Problem for arrows *)

type t = {
  args: ACTerm.t;
  ret: Pure.t;
}

type problem = {
  left: t;
  right: t;
}

val make : Pure.t array -> Pure.t -> t
val make_problem : t -> t -> problem
val pp_problem : string Variable.HMap.t -> problem Fmt.t
