(** Term and Problem for arrows *)

type t = {
  args: Type.Tuple.t;
  ret: Type.t;
}

type problem = {
  left: t;
  right: t;
}

val make : Type.Tuple.t -> Type.t -> t
val pp : t Fmt.t [@@ocaml.toplevel_printer]
val make_problem : t -> t -> problem
val pp_problem : problem Fmt.t [@@ocaml.toplevel_printer]
