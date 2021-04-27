type t [@@ocaml.immediate]

module Gen : sig

  type var = t
  type t

  val make : [< `Query | `Data] -> t
  val gen : t -> var

end

val equal : t -> t -> Bool.t
val compare : t -> t -> Int.t

module Map : CCMap.S with type key = t
module Set: CCSet.S with type elt = t
module HMap : CCHashtbl.S with type key = t

val pp : t Fmt.t [@@ocaml.toplevel_printer]
