module type S = sig
  type storage
  type t = private storage
  val empty : t
  val (&&) : t -> t -> t
  val (||) : t -> t -> t
  val not : t -> t
  val add : t -> int -> t
  val singleton : int -> t
  val all_until : int -> t
  val is_empty : t -> bool
  val mem : int -> t -> bool
  val is_subset : t -> t -> bool
  val is_singleton : t -> bool
  val pp : Format.formatter -> t -> unit

  val storage : t -> storage

  val capacity : int
end


module Default : S with type storage = int
include S with type storage = int and type t = Default.t
