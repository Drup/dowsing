
module type S = sig
  type elt
  type t
  val compare : t CCOrd.t
  val of_seq : elt Sequence.t -> t
  val to_seq : t -> elt Sequence.t
  val pp : elt CCFormat.printer -> t CCFormat.printer
end

module Array (M : Set.OrderedType) : S with type elt = M.t
