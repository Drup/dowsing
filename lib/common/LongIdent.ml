type t = Longident.t =
  | Lident of String.t
  | Ldot of t * String.t
  | Lapply of t * t

let unit = Lident "unit"

let of_list strs =
  if strs = [] then
    invalid_arg "Common.LongIdent.of_list"
  else
    let root = Lident (CCList.hd strs) in
    let strs = CCList.tl strs in
    CCList.fold_left (fun t str -> Ldot (t, str)) root strs

let rec of_outcometree =
  let open Outcometree in
  function
    | Oide_apply (oid1, oid2) ->
        Lapply (of_outcometree oid1, of_outcometree oid2)
    | Oide_dot (oid, str) ->
        Ldot (of_outcometree oid, str)
    | Oide_ident { printed_name = str } ->
        Lident str

let rec to_iter t k =
  match t with
  | Lident str ->
      k str
  | Ldot (t, str) ->
      to_iter t k ;
      k str
  | Lapply (t1, t2) ->
      to_iter t1 k ;
      to_iter t2 k

let compare = compare
let equal = (=)

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
