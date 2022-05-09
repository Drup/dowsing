module M = Diet.Int

include M

let singleton start stop =
  add (Interval.make start stop) empty


let pp_range fmt t =
  Fmt.pf fmt "[%i-%i]" (Interval.x t) (Interval.y t)
let pp = Fmt.(iter ~sep:nop) iter pp_range
