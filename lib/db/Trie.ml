module type S = sig
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

module Leaf : S = struct
  type t = { mutable range : TypeId.Range.interval; types : Type.Set.t }

  let empty = { range = TypeId.Range.Interval.make 0 1; types = Type.Set.empty }
  let add ty t = { t with types = Type.Set.add ty t.types }
  let remove ty t = { t with types = Type.Set.remove ty t.types }
  let iter t = Type.Set.to_iter t.types

  let iter_compatible _ t =
    let rg = TypeId.Range.add t.range TypeId.Range.empty in
    (rg, Type.Set.to_iter t.types)

  let range_compatible _ t = TypeId.Range.add t.range TypeId.Range.empty

  let iterid t =
    let start = TypeId.Range.Interval.x t.range in
    Type.Set.to_iter t.types |> Iter.mapi (fun i ty -> TypeId.mk (start + i) ty)

  let refresh ~start t =
    let stop = start + Type.Set.cardinal t.types in
    t.range <- TypeId.Range.Interval.make start stop;
    stop
end

module Node (Feat : Feature.S) (Sub : S) : S = struct
  module FeatMap = CCMap.Make (Feat)

  type t = Sub.t FeatMap.t

  let empty = FeatMap.empty

  let add ty t =
    t
    |> FeatMap.update (Feat.compute ty) @@ fun sub ->
       let sub = CCOption.get_or ~default:Sub.empty sub in
       Some (Sub.add ty sub)

  let remove ty t =
    t
    |> FeatMap.update (Feat.compute ty) @@ function
       | None -> None
       | Some sub -> Some (Sub.remove ty sub)

  let iter t = FeatMap.values t |> Iter.flat_map Sub.iter

  let squash_ranges f l =
    let r = ref TypeId.Range.empty in
    let iters =
      List.map
        (fun x ->
          let range, it2 = f x in
          r := TypeId.Range.union range !r;
          it2)
        l
    in
    (!r, Iter.append_l iters)

  (*
     TODO: this should be more clever to avoid having
     to walk through the whole feature dictionary.
     In theory, we should be able to make `compare` and `compatible`
     ... compatible, so that we can make a range query.
  *)
  let iter_compatible ty t =
    t |> FeatMap.to_list
    |> squash_ranges (fun (feat, sub) ->
           if Feat.compatible ~query:(Feat.compute ty) ~data:feat then
             Sub.iter_compatible ty sub
           else (TypeId.Range.empty, Iter.empty))

  let range_compatible ty t =
    t |> FeatMap.to_list
    |> CCList.fold_left
         (fun acc (feat, sub) ->
           let r =
             if Feat.compatible ~query:(Feat.compute ty) ~data:feat then
               Sub.range_compatible ty sub
             else TypeId.Range.empty
           in
           TypeId.Range.union r acc)
         TypeId.Range.empty

  let iterid t = t |> FeatMap.values |> Iter.flat_map Sub.iterid

  let refresh ~start t =
    FeatMap.fold (fun _ t start -> Sub.refresh ~start t) t start
end

module Default : S =
  Node (Feature.Head) (
    Node (Feature.Tail) (
      Leaf
    )
  )
