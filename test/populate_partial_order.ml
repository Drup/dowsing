module P = Index__Poset.Poset
module Index = (val Index.(make Feature.all))

let e = Common.Type.Env.make Data
let x = P.init e

let add i (t, _) =
  Format.printf "%i: %a@." i Common.Type.pp t;
  ignore @@ P.add x t

let () =
  let idx = Index.load @@ Fpath.v Sys.argv.(1) in
  Index.iter idx |> Iter.iteri add;
  Format.printf "%a@." P.pp x
