open CommonOpts

type opts = { copts : copts; ty1 : Type.t; ty2 : Type.t }

let main opts =
  Logs.info (fun m -> m "@[<2>type1:@ %a@]" Type.pp opts.ty1);
  Logs.info (fun m -> m "@[<2>type2:@ %a@]" Type.pp opts.ty2);
  let unifs =
    Tracing.with_span ~__FUNCTION__ ~__FILE__ ~__LINE__ "Unifiable cmd" (fun _ ->
      Acic.unifiable env opts.ty1 opts.ty2)
  in
  Logs.debug (fun m -> m "Number of AC solutions: %i" (Tracing.get_nb_ac ()));
  Logs.debug (fun m -> m "Number of Arrow solutions: %i" (Tracing.get_nb_arrow ()));
  if unifs then Fmt.pr "Unifiable@." else Fmt.pr "No unifier@."

let main copts ty1 ty2 = main { copts; ty1; ty2 }

open Cmdliner

let ty1 =
  let docv = "type1" in
  Arg.(required & pos 0 (some @@ Convs.typ env) None & info [] ~docv)

let ty2 =
  let docv = "type2" in
  Arg.(required & pos 1 (some @@ Convs.typ env) None & info [] ~docv)

let cmd =
  let doc = "Check if two types are unifiable" in
  let i = Cmd.info "unifiable" ~sdocs:Manpage.s_common_options ~doc in
  let t = Term.(const main $ copts $ ty1 $ ty2) in
  Cmd.v i t
