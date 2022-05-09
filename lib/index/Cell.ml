module Class = struct

  include CCSet.Make (Info)

  let representative t =
    t
    |> find_first_opt @@ CCFun.negate Info.is_internal
    |> CCOption.get_lazy @@ fun () -> min_elt t

end

type t = {
  mutable id : int ;
  entries : Class.t LongIdent.Map.t ;
}

(* let make () = { id = 0 ; entries = LongIdent.Map.empty } *)

let singleton lid info =
  { id = 0 ;
    entries = LongIdent.Map.add lid (Class.singleton info) LongIdent.Map.empty ;
  }

let add lid info { id ; entries } =
  let entries =
    entries |> LongIdent.Map.update lid @@ function
    | None -> Some (Class.singleton info)
    | Some slot -> Some (Class.add info slot)
  in
  { id ; entries }

let update lid info = function
  | None -> singleton lid info
  | Some t -> add lid info t

let iter t =
  t.entries
  |> LongIdent.Map.to_iter_values
  |> Iter.map Class.representative
  |> Iter.sort ~cmp:Info.compare

let id t = t.id
let refresh id t = t.id <- id
