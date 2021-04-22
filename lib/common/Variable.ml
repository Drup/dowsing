type t = Int.t

module Gen = struct

  type var = t
  type t = var ref

  let make = ref

  let gen t =
    let var = ! t in
    incr t ;
    var

end

let equal = Int.equal
let compare = Int.compare

module Map = CCMap.Make (CCInt)
module HMap = CCHashtbl.Make (CCInt)
module Set = CCSet.Make (CCInt)

let pp names fmt t =
  match HMap.get names t with
  | Some name ->
      if Logs.level () = Some Debug then
        Fmt.pf fmt "'%s/%i" name t
      else
        Fmt.pf fmt "'%s" name
  | None ->
      Fmt.pf fmt "\\%i" t
