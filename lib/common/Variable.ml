type t = Int.t
type var = t

let equal = CCInt.equal
let compare = CCInt.compare

module Map = CCMap.Make (CCInt)
module HMap = CCHashtbl.Make (CCInt)
module Set = CCSet.Make (CCInt)

module Namespace = struct

  type t = Data | Query

  let seed = function
    | Data -> 1
    | Query -> -1

  let next var = var + CCInt.sign var

  let get var =
    assert (var <> 0) ;
    if var > 0 then Data else Query

  let count var = CCInt.abs var - 1

end

module Gen = struct

  type t = var ref

  let make namespace =
    ref @@ Namespace.seed namespace

  let gen t =
    let var = ! t in
    t := Namespace.next var ;
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
    let start_chr =
      match Namespace.get var with
      | Data -> 'a'
      | Query -> 'A'
    in
    "'" ^ CCString.of_list @@ base_26 start_chr @@ Namespace.count var

let pp = Fmt.of_to_string to_string
