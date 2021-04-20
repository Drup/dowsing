(** Several notions of size *)

type kind =
  | VarCount
  | AllVarCount
  | NodeCount
  | HeadKind
  | TailSpineVarCount
  | SpineVarCount
  | TailLength

val all : (string * kind) list

type t = Int.t

module Map : CCMap.S with type key = t
module HMap : CCHashtbl.S with type key = t

val pp : kind -> t Fmt.t

val size : kind -> Type.t -> t
