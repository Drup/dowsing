module type NODE = sig

  type t

  val empty : t
  val add : Type.t -> t -> t
  val remove : Type.t -> t -> t
  val iter : t -> Type.t Iter.t
  val iter_with : Type.t -> t -> Type.t Iter.t

end

module Leaf : NODE = struct

  type t = Type.MSet.t

  let empty = Type.MSet.empty
  let add ty t = Type.MSet.add t ty
  let remove ty t = Type.MSet.remove t ty
  let iter t = Type.MSet.to_iter_mult t |> Iter.map fst
  let iter_with _ = iter

end

module Node (Feat : Feature.S) (Sub : NODE) : NODE = struct

  module FeatMap = CCMap.Make (Feat)

  type t = Sub.t FeatMap.t

  let empty = FeatMap.empty

  let add ty t =
    t |> FeatMap.update (Feat.compute ty) @@ fun sub ->
      let sub = CCOpt.get_or ~default:Sub.empty sub in
      Some (Sub.add ty sub)

  let remove ty t =
    t |> FeatMap.update (Feat.compute ty) @@ function
      | None -> None
      | Some sub -> Some (Sub.remove ty sub)

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

let rec make feats =
  match feats with
  | [] ->
      (module Leaf : NODE)
  | feat :: feats ->
      (module Node (val feat : Feature.S) (val make feats))
