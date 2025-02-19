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
type rel = Smaller | Bigger | Equal | Incomparable

val as_int : t -> int 
val equal : t CCEqual.t
val compare : t CCOrd.t

(** Define a partial order on the variable that extend the partial order of inclusion of
    the flags. In the Env, we must have that a variable [v1] point to a variable [v2]
    then [v1] must be [Smaller] than [v2]. *)
val rel : t -> t -> rel

val is_pure : t -> bool
val is_non_arrow : t -> bool
val is_non_tuple : t -> bool

val get_flags : t -> Flags.t

(** [is_flags_includes v1 v2] check if the flags of [v1] are included in the flags of [v2] *)
val are_flags_included : t -> t -> bool

(** [get_most_general gen l] given the list [l] of variable, find the biggest variable according
    to {rel} if it exist, otherwise create it with [gen].
 *)
val get_most_general : (Flags.t -> t) -> t list -> t

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
