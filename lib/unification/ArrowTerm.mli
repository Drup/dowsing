(** Term and Problem for arrows *)

type t = {
  args: Type.NSet.t;
  ret: Type.t;
}

type problem = {
  left: t;
  right: t;
}

val make : Type.NSet.t -> Type.t -> t
val pp : t Fmt.t [@@ocaml.toplevel_printer]
val make_problem : t -> t -> problem
val pp_problem : problem Fmt.t [@@ocaml.toplevel_printer]
