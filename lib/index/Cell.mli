type t

val update : Longident.t -> Info.t -> t Option.t -> t
val iter : t -> Info.t Iter.t
