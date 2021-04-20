(** Substitutions from variables to types *)

type t = Type.t Variable.Map.t

val simplify : string Variable.HMap.t -> t -> t

val size : t -> int
val compare : t -> t -> int
val lt : t -> t -> bool

val pp : string Variable.HMap.t -> t Fmt.t
