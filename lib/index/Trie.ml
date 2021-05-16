module type NODE = sig

  type 'v t

  val empty : 'v t
  val singleton : Type.t -> 'v -> 'v t
  val add_or_update : Type.t -> ('v Option.t -> 'v) -> 'v t -> 'v t
  val iter : 'v t -> (Type.t * 'v) Iter.t
  val iter_with : Type.t -> 'v t -> (Type.t * 'v) Iter.t

end

module Leaf : NODE = struct

  type 'v t = 'v Type.Map.t

  let empty = Type.Map.empty
  let singleton key v = Type.Map.singleton key v
  let add_or_update k f = Type.Map.update k @@ fun v -> Some (f v)
  let iter = Type.Map.to_iter
  let iter_with _ = iter

end

module Node (Feat : Feature.S) (Sub : NODE) : NODE = struct

  module FeatMap = CCMap.Make (Feat)

  type 'v t = 'v Sub.t FeatMap.t

  let empty = FeatMap.empty

  let singleton ty v =
    FeatMap.singleton (Feat.compute ty) (Sub.singleton ty v)

  let add_or_update ty f t =
    let key = Feat.compute ty in
    let sub =
      match FeatMap.find_opt key t with
      | None -> Sub.singleton ty @@ f None
      | Some sub -> Sub.add_or_update ty f sub
    in
    FeatMap.add key sub t

  let iter t =
    t
    |> FeatMap.values
    |> Iter.flat_map Sub.iter

  (*
     TODO: this should be more clever to avoid having
     to walk through the whole feature dictionary.
     In theory, we should be able to make `compare` and `compatible`
     ... compatible, so that we can make a range query.
  *)

  let iter_with ty t =
    t
    |> FeatMap.to_iter
    |> Iter.flat_map (fun (feat, sub) ->
      if Feat.compatible ~query:(Feat.compute ty) ~data:feat then
        Sub.iter_with ty sub
      else
        Iter.empty
    )

end
