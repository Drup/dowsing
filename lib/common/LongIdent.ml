include (Longident : sig
  type t = Longident.t =
    | Lident of string
    | Ldot of t * string
    | Lapply of t * t
end)

let compare = compare
let equal = (=)

let unit = Lident "unit"

let of_list strs =
  if strs = [] then
    invalid_arg "Type.Longident.of_list"
  else
    let root = Lident (CCList.hd strs) in
    let strs = CCList.tl strs in
    CCList.fold_left (fun id str -> Ldot (id, str)) root strs

let rec of_outcometree =
  let open Outcometree in
  function
    | Oide_apply (oid1, oid2) ->
        Lapply (of_outcometree oid1, of_outcometree oid2)
    | Oide_dot (oid, str) ->
        Ldot (of_outcometree oid, str)
    | Oide_ident { printed_name = str } ->
        Lident str

let rec to_iter id k =
  match id with
  | Lident str ->
      k str
  | Ldot (id, str) ->
      to_iter id k ;
      k str
  | Lapply (id1, id2) ->
      to_iter id1 k ;
      to_iter id2 k

module Map = CCTrie.Make (struct
  type nonrec t = t
  type char_ = String.t
  let compare = CCString.compare
  let to_iter = to_iter
  let of_list = of_list
end)

module HMap = CCHashtbl.Make (struct
  type nonrec t = t
  let equal = equal
  let hash = CCHash.poly
end)

let pp = Pprintast.longident
