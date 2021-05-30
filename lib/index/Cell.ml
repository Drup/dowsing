module Slot = struct

  include CCSet.Make (Info)

  let prune t =
      let t' = filter (CCFun.negate Info.is_internal) t in
      if not @@ is_empty t' then t' else t

  let representative = min_elt

  let pp = Fmt.using representative Info.pp

end

type t = Slot.t LongIdent.Map.t

let empty = LongIdent.Map.empty

let singleton lid info =
  LongIdent.Map.add lid (Slot.singleton info) empty

let add lid info t =
  t |> LongIdent.Map.update lid @@ function
    | None -> Some (Slot.singleton info)
    | Some slot -> Some (Slot.add info slot)

let update lid info = function
  | None -> singleton lid info
  | Some t -> add lid info t

let pp ppf t =
  LongIdent.Map.to_iter_values t
  |> Iter.map Slot.prune
  |> Iter.sort ~cmp:Slot.compare
  |> Fmt.(vbox @@ iter Iter.iter Slot.pp) ppf
