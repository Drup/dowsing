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

module Default = struct

  type t = int
  type storage = int

  let capacity = Sys.int_size - 1
  let check_len len =
    assert (len <= capacity)

  let empty = 0
  let (&&) = (land)
  let (||) = (lor)
  let not = lnot
  let singleton i = check_len i ; 1 lsl i
  let add x i = x || singleton i
  let all_until i = check_len (i+1) ; (1 lsl (i+1) - 1)

  let is_empty n = (n = 0)
  let is_singleton n = n land (n-1) = 0
  let is_subset i b = (i && b) <> 0
  let mem i b = is_subset (singleton i) b

  let pp = CCInt.pp_binary
  let storage x = x
end
