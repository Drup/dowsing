module Class = struct

  include CCSet.Make (Info)

  let representative t =
    t
    |> find_first_opt @@ CCFun.negate Info.is_internal
    |> CCOption.get_lazy @@ fun () -> min_elt t

end

type t = Class.t LongIdent.Map.t

let empty = LongIdent.Map.empty

let singleton lid info =
  LongIdent.Map.add lid (Class.singleton info) empty

let add lid info t =
  t |> LongIdent.Map.update lid @@ function
    | None -> Some (Class.singleton info)
    | Some slot -> Some (Class.add info slot)

let update lid info = function
  | None -> singleton lid info
  | Some t -> add lid info t

let iter t =
  t
  |> LongIdent.Map.to_iter_values
  |> Iter.map Class.representative
  |> Iter.sort ~cmp:Info.compare
