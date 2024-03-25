module type S = sig
  type t

  val empty : t
  val add : TypeId.t -> t -> t
  val checker : Type.t -> t -> TypeId.Range.t
  val search : Type.t -> t -> Type.t Iter.t 
  val union : t -> t -> t
  val iter : t -> Type.t Iter.t
end

module Leaf : S
module Node (Feat : Feature.S) (Sub : S) : S

module Default : S
