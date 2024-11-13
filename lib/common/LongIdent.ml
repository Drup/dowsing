type t = Longident.t =
  | Lident of String.t
  | Ldot of t * String.t
  | Lapply of t * t
[@@deriving ord, eq] 

let hash = CCHash.poly

let unit = Lident "unit"

let of_list strs =
  if strs = [] then
    invalid_arg "Common.LongIdent.of_list"
  else
    let root = Lident (CCList.hd strs) in
    let strs = CCList.tl strs in
    CCList.fold_left (fun t str -> Ldot (t, str)) root strs

let rec of_outcometree (out_id : Outcometree.out_ident) =
  match out_id with
  | Oide_apply (out_id1, out_id2) ->
      Lapply (of_outcometree out_id1, of_outcometree out_id2)
  | Oide_dot (out_id, str) ->
      Ldot (of_outcometree out_id, str)
  | Oide_ident { printed_name = str } ->
      Lident str

let rec to_iter t k =
  match t with
  | Lident str -> k str
  | Ldot (t, str) -> to_iter t k ; k str
  | Lapply (t1, t2) -> to_iter t1 k ; to_iter t2 k

module Map = CCTrie.Make (struct
  type nonrec t = t
  type char_ = String.t
  let compare = String.compare
  let to_iter = to_iter
  let of_list = of_list
end)

module HMap = CCHashtbl.Make (struct
  type nonrec t = t
  let equal = equal
  let hash = hash
end)

let pp = Fmt.hbox Pprintast.longident

(* crude way of comparing, but useful for users *)
let compare_humans =
  let compare_length_lexi str1 str2 =
    CCOrd.(map String.length int str1 str2
      <?> (string, str1, str2))
  in
  CCOrd.map (Fmt.to_to_string pp) compare_length_lexi
