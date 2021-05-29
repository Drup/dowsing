module type NODE = sig

  type t

  val empty : t
  val add : Type.t -> t -> t
  val remove : Type.t -> t -> t
  val iter : t -> Type.t Iter.t
  val iter_with : Type.t -> t -> Type.t Iter.t

end

module Leaf : NODE
module Node (Feat : Feature.S) (Sub : NODE) : NODE
