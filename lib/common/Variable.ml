module Flags : sig
  type t = private int
  type field

  val equal : t -> t -> bool

  val empty : t

  val non_arrow : field

  val set : field -> t -> t
  val get : field -> t -> bool

  val union : t -> t -> t
end = struct
  type t = int
  type field = int

  let max_field = ref 0
  let mk_field () = CCRef.get_then_incr max_field

  let equal = Int.equal

  let empty = 0

  let non_arrow = mk_field ()

  let set f b = (1 lsl f) lor b
  let get f b = (1 lsl f) land b <> 0

  let union b1 b2 = b1 lor b2
end

type t = { id: Int.t; flags: Flags.t }
type var = t

let as_int v = v.id
let compare v1 v2 = CCInt.compare v1.id v2.id
let equal v1 v2 =
  if CCInt.equal v1.id v2.id then (assert (Flags.equal v1.flags v2.flags); true) else false

let is_non_arrow v = Flags.(get non_arrow v.flags)

let merge_flags v1 v2 gen =
  gen (Flags.union v1.flags v2.flags)

module Map = CCMap.Make (struct
    type nonrec t = t
    let compare = compare
  end)
module HMap = CCHashtbl.Make (struct
    type nonrec t = t
    let equal = equal
    let hash v = CCInt.hash v.id
  end)
module Set = CCSet.Make (struct
    type nonrec t = t
    let compare = compare
  end)

(* module Namespace () = struct *)
(*   type t = int *)
(*   let compare = Int.compare *)
(*   let equal = Int.equal *)
(*   let pp = Fmt.int *)
(*   let zero = 0 *)
(*   let incr x = x+1 *)
(* end *)

module Gen  = struct

  type t = int ref

  let make () =
    ref 0

  let gen flags t =
    { id = CCRef.get_then_incr t; flags }

end

let to_string =
  let base_26 start_chr =
    let start_chr = CCChar.code start_chr in
    let rec aux i =
      assert (i >= 0) ;
      CCChar.chr (i mod 26 + start_chr) :: (
        if i < 26 then []
        else aux @@ i / 26
      )
    in
    aux
  in
  fun var ->
    let str =
      var.id
      |> base_26 'a'
      |> (fun l -> if Flags.get Flags.non_arrow var.flags then '>'::l else l)
      |> String.of_list
    in
    str

let pp  = Fmt.of_to_string @@ to_string
