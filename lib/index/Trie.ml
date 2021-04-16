module type NODE = sig

  type key
  type 'v t

  val key : Type.t -> key

  val empty : 'v t
  val singleton : key -> 'v -> 'v t
  val add : key -> 'v -> 'v t -> 'v t
  val iter : 'v t -> 'v Iter.t
  val iteri : 'v t -> (Type.t * 'v) Iter.t
  val iter' : Type.Env.t -> key -> 'v t -> 'v Iter.t
  val iteri' : Type.Env.t -> key -> 'v t -> (Type.t * 'v) Iter.t

end

module Leaf : NODE = struct

  type key = Type.t
  type 'v t = 'v Type.Map.t

  let key = CCFun.id

  let empty = Type.Map.empty
  let singleton = Type.Map.singleton
  let add = Type.Map.add
  let iter = Type.Map.values
  let iteri = Type.Map.to_iter

  let iter' env ty t =
    t
    |> Type.Map.to_iter
    |> Iter.filter_map (fun (ty', v) ->
      if Unification.unifiable env [ ty, ty' ] then
        Some v
      else None
    )

  let iteri' env ty t =
    t
    |> Type.Map.to_iter
    |> Iter.filter_map (fun (ty', v) ->
      if Unification.unifiable env [ ty, ty' ] then
        Some (ty', v)
      else None
    )

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

  let iteri t =
    t
    |> FeatMap.values
    |> Iter.flat_map Sub.iteri

  (* TODO: this should be more clever to avoid having
     to walk through the whole feature dictionary.
     In theory, we should be able to make `compare` and `compatible`
     ... compatible, so that we can make a range query.
  *)

  let iter' env (k, k') t =
    t
    |> FeatMap.to_iter
    |> Iter.flat_map (fun (feat, sub) ->
      if Feat.compatible k feat then
        Sub.iter' env k' sub
      else
        Iter.empty
    )

  let iteri' env (k, k') t =
    t
    |> FeatMap.to_iter
    |> Iter.flat_map (fun (feat, sub) ->
      if Feat.compatible k feat then
        Sub.iteri' env k' sub
      else
        Iter.empty
    )

end

module Make (Node : NODE) = struct

  type 'v t = 'v Node.t

  let empty = Node.empty
  let add ty = Node.add @@ Node.key ty
  let iter = Node.iter
  let iteri = Node.iteri
  let iter' env ty = Node.iter' env @@ Node.key ty
  let iteri' env ty = Node.iteri' env @@ Node.key ty

end
