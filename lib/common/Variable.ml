include CCInt

module Map = CCMap.Make (CCInt)
module HMap = CCHashtbl.Make (CCInt)
module Set = CCSet.Make (CCInt)

module Gen = struct

  type var = t
  type t = var ref

  let make = ref

  let gen self =
    let var = ! self in
    incr self ;
    var

end

let pp names fmt self =
  match HMap.get names self with
  | Some name -> Fmt.pf fmt "'%s" name
  | None -> Fmt.pf fmt "\\%i" self
