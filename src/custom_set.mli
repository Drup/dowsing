
module type S = sig
  type elt
  type t
  val compare : t CCOrd.t
  val of_seq : elt Iter.t -> t
  val to_seq : t -> elt Iter.t
  val pp : elt CCFormat.printer -> t CCFormat.printer
end

module Array (M : Set.OrderedType) : sig
  include S with type elt = M.t
  val as_array : t -> elt array
end
