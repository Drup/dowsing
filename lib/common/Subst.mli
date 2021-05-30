type t = Type.t Variable.HMap.t

val apply : t -> Type.t -> Type.t
val simplify : Variable.Set.t -> t -> t

val size : t -> Int.t
val compare : t CCOrd.t
val lt : t -> t -> Bool.t

val pp : t Fmt.t [@@ocaml.toplevel_printer]
