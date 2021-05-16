module type NODE = sig

  type 'v t

  val empty : 'v t
  val singleton : Type.t -> 'v -> 'v t
  val add_or_update : Type.t -> ('v Option.t -> 'v) -> 'v t -> 'v t
  val iter : 'v t -> (Type.t * 'v) Iter.t
  val iter_with : Type.t -> 'v t -> (Type.t * 'v) Iter.t

end

module Leaf : NODE
module Node (Feat : Feature.S) (Sub : NODE) : NODE
