module type S = TypeIndexIntf.S

module Make (Elt : Set.OrderedType) : S with type ID.t = Elt.t
