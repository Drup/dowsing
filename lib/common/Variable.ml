include CCInt

module Map = CCMap.Make (CCInt)
module HMap = CCHashtbl.Make (CCInt)
module Set = CCSet.Make (CCInt)

module Gen = struct

  type var = t
  type t = var ref

  let make = ref

  let gen t =
    let var = ! t in
    incr t ;
    var

end

let pp names fmt t =
  match HMap.get names t with
  | Some name -> Fmt.pf fmt "'%s" name
  | None -> Fmt.pf fmt "\\%i" t
