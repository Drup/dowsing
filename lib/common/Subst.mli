type t = Type.t Variable.Map.t

val empty : t
val add : Variable.t -> Type.t -> t -> t
val remove : Variable.t -> t -> t

val apply : Type.Env.t -> t -> Type.t -> Type.t
val simplify : Type.Env.t -> Variable.Set.t -> t -> t

val cardinal : t -> Int.t
val size : t -> Int.t
val compare : t CCOrd.t
val lt : t -> t -> Bool.t

val pp : t Fmt.t [@@ocaml.toplevel_printer]
