type id = int

module M = struct
  type t = { id : id; ty : Type.t }

  let equal x y = CCInt.equal x.id y.id
  let compare x y = CCInt.compare x.id y.id
  let hash x = x.id
end

include M
module Set = CCSet.Make (M)
module Map = CCMap.Make (M)

let mk id ty = { id; ty }
let ty t = t.ty

module Range = struct
  include Diet.Int

  let singleton start stop = add (Interval.make start stop) empty
  let pp_range fmt t = Fmt.pf fmt "[%i-%i]" (Interval.x t) (Interval.y t)
  let pp = Fmt.(iter ~sep:nop) iter pp_range
end

let check x rg = Range.mem x.id rg
let pp fmt { id; ty } = Fmt.pf fmt "@[%a@,[%i]@]" Type.pp ty id
