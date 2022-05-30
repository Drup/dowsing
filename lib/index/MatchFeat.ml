module type S = sig
  type t

  val name : String.t
  val compute : Type.t -> t
  val compare : t CCOrd.t
  val compatible : query:t -> data:t -> Bool.t
end

module Const : S = struct
  let name = "constants set"

  module CSet = CCMultiSet.Make (LongIdent)

  type t = CSet.t

  let compute (ty : Type.t) =
    let it = Type.iter_consts ty in
    let set_0 = CSet.empty in
    Iter.fold (fun set lid -> CSet.add set lid) set_0 it

  let compare = CSet.compare

  let subset t1 t2 =
    let check_mem b cnt ty = b && cnt <= CSet.count t2 ty in
    CSet.fold t1 true check_mem

  (*compatible t1 t2 returns false if t1 cannot match with t2 in the sense t1 <= t2*)
  let compatible ~query:t1 ~data:t2 = subset t2 t1
end

let all = [ (module Const : S) ]

let all_with_names =
  CCList.map (fun ((module Feat : S) as feat) -> (Feat.name, feat)) all

let all_names = CCList.map fst all_with_names
let to_string (module Feat : S) = Feat.name
let of_string = CCFun.flip List.assoc @@ all_with_names
let pp = Fmt.of_to_string to_string
