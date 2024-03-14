type t = Int.t
type var = t

let as_int v = v
let compare = CCInt.compare
let equal = CCInt.equal

module Map = CCMap.Make (CCInt)
module HMap = CCHashtbl.Make (CCInt)
module Set = CCSet.Make (CCInt)

(* module Namespace () = struct *)
(*   type t = int *)
(*   let compare = Int.compare *)
(*   let equal = Int.equal *)
(*   let pp = Fmt.int *)
(*   let zero = 0 *)
(*   let incr x = x+1 *)
(* end *)

module Gen  = struct

  type t = var ref

  let make () =
    ref 0

  let gen t =
    let var = !t in
    t := var+1 ;
    var

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
      var
      |> base_26 'a'
      |> String.of_list
    in
    str

let pp  = Fmt.of_to_string @@ to_string
