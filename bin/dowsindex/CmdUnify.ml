open CommonOpts
module Trace = Trace_core

type opts = { copts : copts; all_unifs : Bool.t; ty1 : Type.t; ty2 : Type.t }

let main opts =
  Logs.info (fun m -> m "@[<2>type1:@ %a@]" Type.pp opts.ty1);
  Logs.info (fun m -> m "@[<2>type2:@ %a@]" Type.pp opts.ty2);
  let unifs = if opts.all_unifs then
    Acic.unifiers env opts.ty1 opts.ty2
    |> Iter.sort ~cmp:Subst.compare
    |> Iter.to_list
  else Acic.unify env opts.ty1 opts.ty2 |> CCOption.to_list in
  if unifs = [] then Fmt.pr "no unifier@."
  else Fmt.pr "@[<v2>unifiers:@ %a@]@." Fmt.(list ~sep:sp Subst.pp) unifs

let main copts all_unifs ty1 ty2 = main { copts; all_unifs; ty1; ty2 }

open Cmdliner

let all_unifs =
  let doc = "Report all unifiers." in
  Arg.(value & flag & info [ "a" ] ~doc)

let ty1 =
  let docv = "type1" in
  Arg.(required & pos 0 (some @@ Convs.typ env) None & info [] ~docv)

let ty2 =
  let docv = "type2" in
  Arg.(required & pos 1 (some @@ Convs.typ env) None & info [] ~docv)

let cmd =
  let doc = "unify two types" in
  let i = Cmd.info "unify" ~sdocs:Manpage.s_common_options ~doc in
  let t = Term.(const main $ copts $ all_unifs $ ty1 $ ty2) in
  Cmd.v i t
