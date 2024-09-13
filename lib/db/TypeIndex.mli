module type S = TypeIndexIntf.S

module Make (I : Set.OrderedType) : S with type ID.t = I.t
