module type S = sig
  type t
  val pp : t Fmt.t

  val name : String.t
  val compute : Type.t -> t
  val compare : t CCOrd.t
  val compatible : t -> t -> Acic.hint
end

module Const : S = struct
  let name = "constants set"

  module CSet = CCMultiSet.Make (LongIdent)
  let pp fmt x =
    Fmt.pf fmt "@[{%a}@]"
      (Fmt.iter_bindings
         ~sep:(Fmt.any ",@ ")
         (fun f x -> CSet.to_iter_mult x |> Iter.iter (fun (k, n) -> f k n))
         Fmt.(pair ~sep:(any ":") LongIdent.pp int))
      x

  type t = CSet.t

  let compute (ty : Type.t) =
    let it = Type.iter_consts ty in
    let set_0 = CSet.empty in
    Iter.fold (fun set lid -> CSet.add set lid) set_0 it

  let compare = CSet.compare

  (*compatible t1 t2 returns false if t1 cannot match with t2 in the sense t1 <= t2*)
  let compatible t1 t2 : Acic.hint =
    match CSet.contains t2 t1, CSet.contains t1 t2 with
    | true, true -> Unsure
    | false, false -> Incompatible
    | true, false -> Maybe_bigger
    | false, true -> Maybe_smaller
end

let all = [ (module Const : S) ]
let to_string (module Feat : S) = Feat.name
let pp = Fmt.of_to_string to_string

let compatible (t1 : Type.t) (t2 : Type.t) =
  let check_feat h0 feat =
    let module Ft = (val feat : S) in
    let feat1 = Ft.compute t1 and feat2 = Ft.compute t2 in
    Logs.debug (fun m -> m "@[<v2>Matching features:@ @[%a -> %a@]@ @[%a -> %a@]@]"
      Type.pp t1 Ft.pp feat1
      Type.pp t2 Ft.pp feat2);
    let h = Ft.compatible feat1 feat2 in
    Logs.debug (fun m -> m "Result: %a" Acic.pp_hint h);
    Acic.combine_hint h0 h
  in
  let feats = all in
  CCList.fold_left check_feat Unsure feats

let compare env t1 t2 =
  if not (Feature.compatible t1 t2) then Acic.Incomparable
  else
    let hint = compatible t1 t2 in
    Acic.compare ~hint env t1 t2
