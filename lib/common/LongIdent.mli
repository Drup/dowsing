type t = Longident.t =
  | Lident of String.t
  | Ldot of t * String.t
  | Lapply of t * t

val unit : t

val of_list : String.t List.t -> t
val of_outcometree : Outcometree.out_ident -> t
val to_iter : t -> String.t Iter.t

val compare : t -> t -> Int.t
val equal : t -> t -> Bool.t

module Map : CCTrie.S with type key = t and type char_ = String.t
module HMap : CCHashtbl.S with type key = t

val pp : t Fmt.t [@@ocaml.toplevel_printer]
val compare_humans : t -> t -> int
