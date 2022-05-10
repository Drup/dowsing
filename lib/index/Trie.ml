module type NODE = sig

  type t

  val empty : t
  val add : Type.t -> t -> t
  val remove : Type.t -> t -> t
  val iter : t -> Type.t Iter.t
  val iter_compatible : Type.t -> t -> TypeId.Range.t * Type.t Iter.t

  val iterid : t -> TypeId.t Iter.t
  val refresh : start:int -> t -> int
  
end

module Leaf : NODE = struct

  type t = {
    mutable range : TypeId.Range.t ;
    types : Type.Set.t ;
  }

  let empty = { range = TypeId.Range.singleton 0 1 ; types = Type.Set.empty }
  let add ty t =
    { t with types = Type.Set.add ty t.types }
  let remove ty t =
    { t with types = Type.Set.remove ty t.types }
  let iter t = Type.Set.to_iter t.types
  let iter_compatible _ t = t.range, Type.Set.to_iter t.types

  let iterid t =
    Type.Set.to_iter t.types |> Iter.mapi TypeId.mk

  let refresh ~start t =
    let stop = start + Type.Set.cardinal t.types in
    t.range <- TypeId.Range.singleton start stop ;
    stop
end

module Node (Feat : Feature.S) (Sub : NODE) : NODE = struct

  module FeatMap = CCMap.Make (Feat)

  type t = Sub.t FeatMap.t

  let empty = FeatMap.empty

  let add ty t =
    t |> FeatMap.update (Feat.compute ty) @@ fun sub ->
      let sub = CCOption.get_or ~default:Sub.empty sub in
      Some (Sub.add ty sub)

  let remove ty t =
    t |> FeatMap.update (Feat.compute ty) @@ function
      | None -> None
      | Some sub -> Some (Sub.remove ty sub)

  let squash_ranges f it =
    let r = ref TypeId.Range.empty in
    let it' =
      Iter.map (fun x ->
          let range, it2 = f x in
          r := TypeId.Range.union range !r;
          it2) it
    in
    it' @@ ignore ;
    !r, Iter.flatten it'
  
  let iter t =
    FeatMap.values t
    |> Iter.flat_map Sub.iter

  (*
     TODO: this should be more clever to avoid having
     to walk through the whole feature dictionary.
     In theory, we should be able to make `compare` and `compatible`
     ... compatible, so that we can make a range query.
  *)

  let iter_compatible ty t =
    t
    |> FeatMap.to_iter
    |> squash_ranges (fun (feat, sub) ->
      if Feat.compatible ~query:(Feat.compute ty) ~data:feat then
        Sub.iter_compatible ty sub
      else
        TypeId.Range.empty, Iter.empty
      )

  let iterid t =
    let it = FeatMap.values t in 
    let it' = Iter.flat_map Sub.iterid it in
    it'

  let refresh ~start t =
    FeatMap.fold (fun _  t start -> Sub.refresh ~start t) t start

end

let rec make feats =
  match feats with
  | [] ->
      (module Leaf : NODE)
  | feat :: feats ->
      (module Node (val feat : Feature.S) (val make feats))
