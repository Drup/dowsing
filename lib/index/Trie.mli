module type NODE = sig

  type 'v t

  val empty : 'v t
  val singleton : Type.t -> 'v -> 'v t
  val add : Type.t -> 'v -> 'v t -> 'v t
  val iter : 'v t -> (Type.t * 'v list) Iter.t
  val iter_with : Type.t -> 'v t -> (Type.t * 'v list) Iter.t

end

module Leaf : NODE
module Node (Feat : Feature.S) (Sub : NODE) : NODE
