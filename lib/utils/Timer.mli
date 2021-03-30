type t

val make : Unit.t -> t

val get : t -> Float.t

val tick : t -> Unit.t
val tock : t -> Unit.t
