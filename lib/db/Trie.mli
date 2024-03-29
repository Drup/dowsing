module type NODE = sig
  type t

  val empty : t
  val add : Type.t -> t -> t
  val remove : Type.t -> t -> t
  val iter : t -> Type.t Iter.t
  val iter_compatible : Type.t -> t -> TypeId.Range.t * Type.t Iter.t
  val range_compatible : Type.t -> t -> TypeId.Range.t
  val iterid : t -> TypeId.t Iter.t
  val refresh : start:int -> t -> int
end

module Leaf : NODE
module Node (Feat : Feature.S) (Sub : NODE) : NODE

val make : (module Feature.S) List.t -> (module NODE)
