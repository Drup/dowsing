open Common

let main _ exhaustive cnt idx_file ty =
  let idx =
    try Index.load idx_file
    with Sys_error _ ->
      error @@ Fmt.str "cannot open index file '%s'." idx_file
  in
  let res =
    let find = if exhaustive then Index.find else Index.find_with in
    find idx env ty
    |> Iter.sort ~cmp:(fun (ty1, _, unif1) (ty2, _, unif2) ->
      CCOrd.(Unification.Subst.compare unif1 unif2
        <?> (Type.compare, ty1, ty2))
    )
  in
  let res = CCOpt.fold (CCFun.flip Iter.take) res cnt in
  Fmt.pr "@[<v>%a@]@."
    (Fmt.iter Iter.iter @@ fun fmt (_, info, _) -> Index.Info.pp fmt info) res

let main copts exhaustive cnt idx_file ty =
  try Ok (main copts exhaustive cnt idx_file ty)
  with Error msg -> Error (`Msg msg)

open Cmdliner

let exhaustive =
  let doc = "Use exhaustive search (slow)." in
  Arg.(value & flag & info [ "exhaustive" ] ~doc)

let cnt =
  let docv = "n" in
  let doc = "Report only the first $(docv) results." in
  Arg.(value & opt (some int) None & info [ "n" ] ~docv ~doc)

let idx_file =
  let docv = "index" in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv)

let ty =
  let docv = "type" in
  Arg.(required & pos 1 (some conv_type) None & info [] ~docv)

let cmd =
  let doc = "search index" in
  Term.(term_result (const main $ copts $ exhaustive $ cnt $ idx_file $ ty)),
  Term.(info "search" ~exits:default_exits ~sdocs:Manpage.s_common_options ~doc)
