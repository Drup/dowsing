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

module Int : S with type t = private int = struct

  type t = int

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
  let is_subset i b = (i && b) = i
  let mem i b = is_subset (singleton i) b

  let pp = CCInt.pp_binary
end


module Z : S with type t = private Z.t = struct
  open Z.Compare
  
  type t = Z.t

  let capacity = max_int
  let empty = Z.zero
  let (&&) = Z.(land)
  let (||) = Z.(lor)
  let not = Z.lognot
  let singleton i = Z.(one lsl i)
  let add x i = x || singleton i
  let all_until i = let x = i+1 in Z.(one lsl x - one)

  let is_empty n = Z.(n = zero)
  let is_singleton n = Z.(n land (n-one) = zero)
  let is_subset i b = Z.((i && b) = i)
  let mem i b = is_subset (singleton i) b

  let pp = Z.pp_print
end

(* module Bitv : S = struct
 * 
 *   type t = Bitv.t
 *   type storage = Bitv.t
 *   let capacity = 128
 *   
 *   let empty = Bitv.create capacity
 *   let (&&) = Bitv.bw_and
 *   let (||) = Bitv.bw_or
 *   let not = Bivt.bw_not
 *   let singleton i = Bitv.set
 *   let add x i = x || singleton i
 *   let all_until i = check_len (i+1) ; (1 lsl (i+1) - 1)
 * 
 *   let is_empty n = (n = 0)
 *   let is_singleton n = n land (n-1) = 0
 *   let is_subset i b = (i && b) <> 0
 *   let mem i b = is_subset (singleton i) b
 * 
 *   let pp = CCInt.pp_binary
 *   let storage x = x
 * 
 *   let pp = Bitv.L.print
 *   let storage x = x
 *   
 * end *)

include Z
