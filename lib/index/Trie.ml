module type NODE = sig

  type key
  type 'v t

  val key : Type.t -> key

  val empty : 'v t
  val singleton : key -> 'v -> 'v t
  val add : key -> 'v -> 'v t -> 'v t
  val iter : 'v t -> (Type.t * 'v) Iter.t
  val iter_filter : key -> (Type.t -> Bool.t) -> 'v t -> (Type.t * 'v) Iter.t

end

module Leaf : NODE = struct

  type key = Type.t
  type 'v t = 'v Type.Map.t

  let key = CCFun.id

  let empty = Type.Map.empty
  let singleton = Type.Map.singleton
  let add = Type.Map.add
  let iter = Type.Map.to_iter

  let iter_filter _ pred t =
    iter t
    |> Iter.filter (fun (ty, _) -> pred ty)

end

module Node (Feat : Feature.S) (Sub : NODE) : NODE = struct

  module FeatMap = CCMap.Make (Feat)

  type key = Feat.t * Sub.key
  type 'v t = 'v Sub.t FeatMap.t

  let key ty = Feat.compute ty, Sub.key ty

  let empty = FeatMap.empty

  let singleton (k, k') v =
    FeatMap.singleton k @@ Sub.singleton k' v

  let add (k, k') v t =
    match FeatMap.find_opt k t with
    | None -> FeatMap.add k (Sub.singleton k' v) t
    | Some sub -> FeatMap.add k (Sub.add k' v sub) t

  let iter t =
    t
    |> FeatMap.values
    |> Iter.flat_map Sub.iter

  (* TODO: this should be more clever to avoid having
     to walk through the whole feature dictionary.
     In theory, we should be able to make `compare` and `compatible`
     ... compatible, so that we can make a range query.
  *)

  let iter_filter (k, k') pred t =
    t
    |> FeatMap.to_iter
    |> Iter.flat_map (fun (feat, sub) ->
      if Feat.compatible ~query:k ~data:feat then
        Sub.iter_filter k' pred sub
      else
        Iter.empty
    )

end

module Make (Node : NODE) = struct

  type 'v t = 'v Node.t

  let empty = Node.empty
  let add ty = Node.add @@ Node.key ty
  let iter = Node.iter
  let iter_filter ty = Node.iter_filter @@ Node.key ty

end
