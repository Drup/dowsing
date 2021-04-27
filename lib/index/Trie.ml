module type NODE = sig

  type key
  type 'v t

  val key : Type.t -> key

  val empty : 'v t
  val singleton : key -> 'v -> 'v t
  val add : key -> 'v -> 'v t -> 'v t
  val iter : 'v t -> (Type.t * 'v list) Iter.t
  val iter_with : key -> 'v t -> (Type.t * 'v list) Iter.t

end

module Leaf : NODE = struct

  type key = Type.t
  type 'v t = 'v list Type.Map.t

  let key = CCFun.id

  let empty = Type.Map.empty
  let singleton key v = Type.Map.singleton key [v]
  let add k v m =
    Type.Map.update k (function None -> Some [v] | Some l -> Some (v::l)) m
  let iter = Type.Map.to_iter
  let iter_with _ = iter

end

module Node (Feat : Feature.S) (Sub : NODE) : NODE = struct

  module FeatMap = CCMap.Make (Feat)

  type key = Feat.t * Sub.key
  type 'v t = 'v Sub.t FeatMap.t

  let key ty = Feat.compute ty, Sub.key ty

  let empty = FeatMap.empty

  let singleton (key, key') v =
    FeatMap.singleton key @@ Sub.singleton key' v

  let add (key, key') v t =
    let sub =
      match FeatMap.find_opt key t with
      | None -> Sub.singleton key' v
      | Some sub -> Sub.add key' v sub
    in
    FeatMap.add key sub t

  let iter t =
    t
    |> FeatMap.values
    |> Iter.flat_map Sub.iter

  (* TODO: this should be more clever to avoid having
     to walk through the whole feature dictionary.
     In theory, we should be able to make `compare` and `compatible`
     ... compatible, so that we can make a range query.
  *)

  let iter_with (key, key') t =
    t
    |> FeatMap.to_iter
    |> Iter.flat_map (fun (feat, sub) ->
      if Feat.compatible ~query:key ~data:feat then
        Sub.iter_with key' sub
      else
        Iter.empty
    )

end

module Make (Node : NODE) = struct

  type 'v t = 'v Node.t

  let empty = Node.empty
  let add ty = Node.add @@ Node.key ty
  let iter = Node.iter
  let iter_with ty = Node.iter_with @@ Node.key ty

end
