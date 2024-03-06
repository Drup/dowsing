include module type of CCString

module Map : CCMap.S with type key = t
module HMap : CCHashtbl.S with type key = t
module Set : CCSet.S with type elt = t
