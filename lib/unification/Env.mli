(** Unification environment *)

type t

val make : tyenv:Type.Env.t -> orig_vars:Variable.Set.t -> t
val copy : t -> t
val gen : Variable.Flags.t -> t -> Variable.t
val vars : t -> Subst.t

val tyenv : t -> Type.Env.t

type representative =
  | V of Variable.t
  | E of Variable.t * Type.t

exception FlagsClash of Variable.t * Type.t

val representative : t -> Variable.t -> representative

val push_tuple : t -> ACTerm.t -> ACTerm.t -> unit
val push_arrow : t -> ArrowTerm.t -> ArrowTerm.t -> unit
val add : t -> Variable.t -> Type.t -> unit
val remove : t -> Variable.t -> unit
val init_partial : t -> Variable.t -> unit
val extend_partial : ?by:int -> t -> Variable.t -> Type.t -> unit

val merge : t -> t -> t * (Type.t * Type.t) list

val commit : t -> t * (Type.t * Type.t) list

val pop_tuple : t -> ACTerm.problem option
val pop_arrow : t -> ArrowTerm.problem option

val is_solved : t -> Subst.t option

val pp : t Fmt.t [@@ocaml.toplevel_printer]
