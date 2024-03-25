module type S = sig
  type t

  val empty : t
  val add : TypeId.t -> t -> t
  val checker : Type.t -> t -> TypeId.Range.t
  val search : Type.t -> t -> Type.t Iter.t 
  val union : t -> t -> t
  val iter : t -> Type.t Iter.t
end

module Leaf : S = struct
  type t = { range : TypeId.Range.t; types : Type.Set.t }

  let empty = { range = TypeId.Range.empty; types = Type.Set.empty }
  let add (tyid : TypeId.t) t =
    let types = Type.Set.add tyid.ty t.types in
    let range = TypeId.Range.(union (singleton tyid.id) t.range) in
    { range ; types }
  
  let iter t = Type.Set.to_iter t.types
  
  let checker _ t = t.range

  let search _ t = iter t

  let union t1 t2 =
    let range = TypeId.Range.union t1.range t2.range in
    let types = Type.Set.union t1.types t2.types in
    { range; types }
end

module Node (Feat : Feature.S) (Sub : S) : S = struct
  module FeatMap = CCMap.Make (Feat)

  type t = Sub.t FeatMap.t

  let empty = FeatMap.empty

  let add tyid t =
    let query = Feat.compute @@ TypeId.ty tyid in
    t |> FeatMap.update query @@ fun sub ->
    let sub = CCOption.get_or ~default:Sub.empty sub in
    Some (Sub.add tyid sub)

  let iter t k =
    FeatMap.iter (fun _ sub -> Sub.iter sub k) t

  let checker ty t =
    let query = Feat.compute ty in
    FeatMap.fold (fun data sub acc ->
        if Feat.compatible ~query ~data then
          TypeId.Range.union (Sub.checker ty sub) acc
        else
          acc)
      t
      TypeId.Range.empty

  let search ty t k =
    let query = Feat.compute ty in
    FeatMap.iter (fun data sub ->
        if Feat.compatible ~query ~data then
          Sub.search ty sub k
        else ())
      t

  let union t1 t2 =
    FeatMap.union (fun _feat sub1 sub2 -> Some (Sub.union sub1 sub2)) t1 t2 
end

module Default : S =
  Node (Feature.Head) (
    Node (Feature.Tail) (
      Leaf
    )
  )
