
include CCInt
module Map = CCMap.Make(CCInt)
module HMap = CCHashtbl.Make(CCInt)

type gen = int ref
let gen r = let x = !r in incr r ; x
let init i = ref i
let inject i = i
let pp nametbl fmt i =
  match HMap.get nametbl i with
  | Some s -> Fmt.pf fmt "'%s" s
  | None -> Fmt.pf fmt "\\%i" i

module Set = CCSet.Make(CCInt)
