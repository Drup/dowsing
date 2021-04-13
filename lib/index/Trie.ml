module type NODE = sig
  type key
  type 'a t

  val compute : Type.Env.t -> Type.t -> key
  
  val empty : 'a t
  val singleton : key -> 'a -> 'a t
  val add :  key -> 'a -> 'a t -> 'a t
  val get : Type.Env.t -> 'a t -> key -> 'a Iter.t
end

module Leaf : NODE with type key = Type.t = struct
  type key = Type.t 
  type 'a t = 'a Type.Map.t
  let compute _ ty = ty
  let empty = Type.Map.empty
  let singleton k v = Type.Map.singleton k v
  let add k v m = Type.Map.add k v m
  let get env m k0 =
    let aux (k, v) =
      if Unification.unifiable env [k0, k] then Some v else None
    in
    Type.Map.to_iter m
    |> Iter.filter_map aux
end

module Node (Feature : Feature.S) (Sub : NODE)
  : NODE with type key = Feature.t * Sub.key
= struct
  module M = CCMap.Make(Feature)
  type key = Feature.t * Sub.key
  type 'a t = 'a Sub.t M.t

  let compute env ty = Feature.compute env ty, Sub.compute env ty

  let empty = M.empty
  let singleton (k,k') v = M.singleton k (Sub.singleton k' v)
  let add (k,k') v m =
    match M.find_opt k m with 
    | None -> M.add k (Sub.singleton k' v) m
    | Some subm -> M.add k (Sub.add k' v subm) m

  (* TODO: this should be more clever to avoid having
     to walk through the whole feature dictionary.

     In theory, we should be able to make `compare` and `compatible`
     ... compatible, so that we can make a range query.
  *)
  let get env m (k0,k') =
    let aux (k, m) =
      if Feature.compatible ~source:k0 ~target:k then
        Sub.get env m k'
      else
        Iter.empty
    in
    let it = M.to_iter m in
    Iter.flat_map aux it
end
