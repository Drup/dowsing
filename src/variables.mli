
type t = private int
val equal : t -> t -> bool
val compare : t -> t -> int
module Map : CCMap.S with type key = t
module Set: CCSet.S with type elt = t
module HMap : CCHashtbl.S with type key = t

type gen
val gen : gen -> t
val init : int -> gen
val inject : int -> t
val pp : string HMap.t -> t Fmt.t

