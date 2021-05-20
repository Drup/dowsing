(** Substitutions from variables to types *)

type t = Type.t Variable.Map.t

val simplify : Variable.Set.t -> t -> t

val size : t -> int
val compare : t CCOrd.t
val lt : t -> t -> bool

val pp : t Fmt.t [@@ocaml.toplevel_printer]
