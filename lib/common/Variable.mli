type t [@@ocaml.immediate]
type var = t

val as_int : t -> int 
val equal : t CCEqual.t
val compare : t CCOrd.t

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
  val gen : t -> var

end

val to_string : t -> string
val pp : t Fmt.t [@@ocaml.toplevel_printer]
