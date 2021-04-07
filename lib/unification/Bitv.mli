module type S = sig

  type t
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

  val capacity : int

end

module Int : S with type t = private int
module Z : S with type t = private Z.t

include module type of Z
