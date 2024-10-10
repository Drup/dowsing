module Flags : sig
  type t
  type field

  val empty : t
  val non_arrow : field
  val non_tuple : field

  val equal : t -> t -> bool

  val set : field -> t -> t

  val union : t -> t -> t
end

type t
type var = t

val as_int : t -> int 
val equal : t CCEqual.t
val compare : t CCOrd.t

val is_pure : t -> bool
val is_non_arrow : t -> bool
val is_non_tuple : t -> bool

val get_flags : t -> Flags.t

(** [is_flags_includes v1 v2] check if the flags of [v1] are included in the flags of [v2] *)
val are_flags_included : t -> t -> bool

(** [merge v1 v2 gen] merge the flags of v1 v2 into a fresh variable
    created by gen. 
 *)
val merge_flags : t -> t -> ( Flags.t -> t) -> t

val find_most_general : t list -> (t, Flags.t) Either.t

module Map : CCMap.S with type key = t
module HMap : CCHashtbl.S with type key = t
module Set : CCSet.S with type elt = t

(* module Namespace : sig *)

(*   type t = Data | Query *)

(*   val seed : t -> var *)
(*   val next : var -> var *)
(*   val get : var -> t *)
(*   val count : var -> Int.t *)

(* end *)

module Gen : sig

  type t

  val make : unit -> t
  val gen : Flags.t -> t -> var

end

val to_string : t -> string
val pp : t Fmt.t [@@ocaml.toplevel_printer]
