open CommonOpts
module Trace = Trace_core

type opts = { copts : copts; all_unifs : Bool.t; quiet : Bool.t; stat : Bool.t; ty1 : Type.t; ty2 : Type.t }

let check_features t1 t2 =
  not
  @@ List.for_all
       (fun (module F : Db.Feature.S) ->
         let f1 = F.compute t1 and f2 = F.compute t2 in
         F.compatible ~query:f1 ~data:f2)
       Db.Feature.all


let main opts =
  let timer = Timer.make () in
  Logs.info (fun m -> m "@[<2>type1:@ %a@]" Type.pp opts.ty1);
  Logs.info (fun m -> m "@[<2>type2:@ %a@]" Type.pp opts.ty2);
  Timer.start timer;
  let unifs = if opts.all_unifs then
    Acic.unifiers env opts.ty1 opts.ty2
    |> Iter.sort ~cmp:Subst.compare
    |> Iter.to_list
  else Acic.unify env opts.ty1 opts.ty2 |> CCOption.to_list in
  Timer.stop timer;
  if not opts.quiet then
    if unifs = [] then Fmt.pr "no unifier@."
    else Fmt.pr "@[<v2>unifiers:@ %a@]@." Fmt.(list ~sep:sp Subst.pp) unifs;
  if opts.stat then (* TODO: Forgot if this can be cut by the index. *)
    Fmt.pr "%f | %b | %i | %i | %i@."
      (Timer.get timer) (check_features opts.ty1 opts.ty2)
      (Tracing.get_nb_ac ()) (Tracing.get_nb_arrow ()) (Tracing.get_nb_timeout ())

let main copts all_unifs quiet stat ty1 ty2 = main { copts; all_unifs; quiet; stat; ty1; ty2 }

open Cmdliner

let all_unifs =
  let doc = "Report all unifiers." in
  Arg.(value & flag & info [ "a" ] ~doc)

let stat =
  let doc = "Report statistics on the unification" in
  Arg.(value & flag & info [ "s" ] ~doc)

let quiet =
  let doc = "Don't print the unifier." in
  Arg.(value & flag & info [ "q" ] ~doc)

let ty1 =
  let docv = "type1" in
  Arg.(required & pos 0 (some @@ Convs.typ env) None & info [] ~docv)

let ty2 =
  let docv = "type2" in
  Arg.(required & pos 1 (some @@ Convs.typ env) None & info [] ~docv)

let cmd =
  let doc = "unify two types" in
  let i = Cmd.info "unify" ~sdocs:Manpage.s_common_options ~doc in
  let t = Term.(const main $ copts $ all_unifs $ quiet $ stat $ ty1 $ ty2) in
  Cmd.v i t
