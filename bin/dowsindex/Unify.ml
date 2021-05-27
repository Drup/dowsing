open Common

let main _ all_unifs ty1 ty2 =
  Logs.info (fun m -> m "@[<2>type1:@ %a@]" Type.pp ty1) ;
  Logs.info (fun m -> m "@[<2>type2:@ %a@]" Type.pp ty2) ;
  let unifs =
    Unification.unifiers env ty1 ty2
    |> Iter.sort ~cmp:Unification.Subst.compare
    |> Iter.to_list
  in
  let unifs =
    if all_unifs then unifs
    else CCList.take 1 unifs
  in
  if unifs = [] then
    Fmt.pr "no unifier@."
  else
    Fmt.pr "@[<v2>unifiers:@ %a@]@."
      Fmt.(list ~sep:sp Unification.Subst.pp) unifs

open Cmdliner

let all_unifs =
  let doc = "Report all unifiers." in
  Arg.(value & flag & info [ "a" ] ~doc)

let ty1 =
  let docv = "type1" in
  Arg.(required & pos 0 (some conv_type) None & info [] ~docv)

let ty2 =
  let docv = "type2" in
  Arg.(required & pos 1 (some conv_type) None & info [] ~docv)

let cmd =
  let doc = "unify two types" in
  Term.(const main $ copts $ all_unifs $ ty1 $ ty2),
  Term.(info "unify" ~exits:default_exits ~sdocs:Manpage.s_common_options ~doc)
