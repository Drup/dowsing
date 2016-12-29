
module type S = sig
  type elt
  type t
  val add : elt -> t -> t
  val singleton : elt -> t
  val union : t -> t -> t
  val compare : t CCOrd.t
  val of_seq : elt Sequence.t -> t
  val to_seq : t -> elt Sequence.t
  val map : (elt -> elt) -> t -> t
end

module Array (M : Set.OrderedType) : S with type elt = M.t 
