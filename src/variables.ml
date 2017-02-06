
include CCInt
module Map = CCMap.Make(CCInt)

type gen = int ref
let gen r = let x = !r in incr r ; x
let init i = ref i
let inject i = i
let pp fmt i = Fmt.pf fmt "\\%i" i
