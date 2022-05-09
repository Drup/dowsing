type t

val update : Longident.t -> Info.t -> t Option.t -> t
val iter : t -> Info.t Iter.t
val id : t -> int
val refresh : int -> t -> unit
