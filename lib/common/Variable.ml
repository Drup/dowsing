type rel = Smaller | Bigger | Equal | Incomparable

module Flags : sig
  type t = private int
  type field

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val rel : t -> t -> rel

  val empty : t

  val non_arrow : field
  val non_tuple : field

  val set : field -> t -> t
  val get : field -> t -> bool

  val union : t -> t -> t

  (** [subset f1 f2] return true if [f2] contains all the flags of [f1].
      Say otherwise, if [f1] is a subset of flags of [f2].*)
  val subset : t -> t -> bool
end = struct
  type t = int
  type field = int

  let max_field = ref 0
  let mk_field () = CCRef.get_then_incr max_field

  let equal = Int.equal

  (* Writen reversed so that variable with the non_tuple flags are the smallest. *)
  let compare b1 b2 = Int.compare b2 b1

  let empty = 0

  (* This must be in this order, because of the compare function. *)
  let non_arrow = mk_field ()
  let non_tuple = mk_field ()

  let set f b = (1 lsl f) lor b
  let get f b = (1 lsl f) land b <> 0

  let union b1 b2 = b1 lor b2

  let subset b1 b2 = (b1 land b2) = b1

  let rel b1 b2 =
    if b1 = b2 then Equal
    else if subset b1 b2 then Smaller
    else if subset b2 b1 then Bigger
    else Incomparable
end

type t = { id: Int.t; flags: Flags.t }
type var = t

let as_int v = v.id

(* The order must guarantee that the variable with the flags non_tuple are smaller than
   variable without this flags. *)
let compare v1 v2 =
  CCPair.compare Flags.compare CCInt.compare (v1.flags, v1.id) (v2.flags, v2.id)
let equal v1 v2 =
  if CCInt.equal v1.id v2.id then (assert (Flags.equal v1.flags v2.flags); true) else false

let rel v1 v2 =
  match Flags.rel v1.flags v2.flags with
  | Equal ->
      if v1.id = v2.id then Equal
      else if v1.id < v2.id then Smaller
      else Bigger
  | r -> r

let is_pure v = Flags.(equal empty v.flags)
let is_non_arrow v = Flags.(get non_arrow v.flags)
let is_non_tuple v = Flags.(get non_tuple v.flags)

let get_flags {flags; _} = flags

let are_flags_included v1 v2 = Flags.subset v1.flags v2.flags

let merge_flags v1 v2 gen =
  gen (Flags.union v1.flags v2.flags)

let get_most_general gen =
  let rec get_most_general_rec flags v = function
      | h :: t ->
        begin match v with
          | Some v ->
            assert (v.flags = flags);
            begin match rel h v with
              | Equal | Smaller -> get_most_general_rec flags (Some v) t
              | Bigger -> get_most_general_rec h.flags (Some h) t
              | Incomparable -> get_most_general_rec (Flags.union flags h.flags) None t
            end
          | None ->
            begin match Flags.rel h.flags flags with
              | Equal | Bigger -> get_most_general_rec h.flags (Some h) t
              | Smaller | Incomparable -> get_most_general_rec (Flags.union h.flags flags) None t
            end
        end
      | [] ->
        match v with
        | Some v -> v
        | None -> gen flags
  in
  get_most_general_rec Flags.empty None

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

  let max_var = max_int

  let make () =
    ref 0

  let gen flags t =
    if !t = max_var then failwith "Too many variable created";
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
      |> (fun l -> if is_non_arrow var then '>' :: l else l)
      |> (fun l -> if is_non_tuple var then '*' :: l else
        l)
      |> String.of_list
    in
    str

let pp  = Fmt.of_to_string @@ to_string
