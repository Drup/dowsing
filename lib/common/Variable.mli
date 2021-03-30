type t = private Int.t

val equal : t -> t -> Bool.t
val compare : t -> t -> Int.t

module Map : CCMap.S with type key = t
module Set: CCSet.S with type elt = t
module HMap : CCHashtbl.S with type key = t

module Gen : sig

  type var = t
  type t

  val make : Int.t -> t
  val gen : t -> var

end

val pp : String.t HMap.t -> t Fmt.t
