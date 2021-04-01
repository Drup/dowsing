type t = Longident.t =
  | Lident of string
  | Ldot of t * string
  | Lapply of t * t

val compare : t -> t -> Int.t
val equal : t -> t -> Bool.t

val unit : t

val of_list : String.t List.t -> t
val of_outcometree : Outcometree.out_ident -> t
val to_iter : t -> String.t Iter.t

module Map : CCTrie.S with type key = t and type char_ = String.t
module HMap : CCHashtbl.S with type key = t

val pp : t Fmt.t
