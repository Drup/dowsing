type t = Int.t

module Gen = struct

  type var = t
  type t = {
    mutable content : int ;
    dir : int ;
  }   

  let dir_as_int = function `Data -> 1 | `Query -> -1
  
  let make dir = {
    content = 0 ;
    dir = dir_as_int dir ;
  }

  let gen t =
    let var = t.content in
    t.content <- var + t.dir ;
    var

end

let equal = Int.equal
let compare = Int.compare

module Map = CCMap.Make (CCInt)
module HMap = CCHashtbl.Make (CCInt)
module Set = CCSet.Make (CCInt)

let rec base_26 start_chr i =
  assert (i >= 0);
  Char.chr (i mod 26 + start_chr) :: (
    if i < 26 then []
    else
      base_26 start_chr (i/26)
  )
let to_string i =
  let root = if i >= 0 then Char.code 'a' else Char.code 'A' in
  CCString.of_list @@ base_26 root @@ abs i

let pp fmt t =
  Fmt.pf fmt "'%s" (to_string t)
