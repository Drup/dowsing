open Common

type opts = {
  copts : copts ;
  verbose : Bool.t ;
  feats : (module Index.Feature.S) List.t ;
  idx_file : Fpath.t ;
  pkgs : String.t List.t ;
}

let main opts =
  let module Index = (val Index.make_ opts.feats) in
  Bos.OS.Dir.create ~path:true @@ Fpath.parent opts.idx_file
  |> CCResult.iter_err (fun (`Msg msg) -> error msg) ;
  let pkgs =
    try
      if opts.pkgs = []
      then Package.find_all ()
      else Package.find opts.pkgs
    with
    | Package.Error pkg ->
        error @@ Fmt.str "cannot find package `%s'"
          pkg
  in
  if opts.verbose then
    Fmt.pr "@[<v2>found %i packages:@ %a@]@."
      (CCList.length pkgs)
      Fmt.(list ~sep:sp @@ using snd Fpath.pp) pkgs ;
  let idx =
    try Index.load opts.idx_file
    with Sys_error _ -> Index.make ()
  in
  pkgs |> CCList.iter @@ CCFun.uncurry @@ Index.add idx ;
  try
    Index.save idx opts.idx_file
  with Sys_error _ ->
    error @@ Fmt.str "cannot write index file `%a'"
      Fpath.pp opts.idx_file

let main copts verbose feats idx_file pkgs =
  try Ok (main { copts ; verbose ; feats ; idx_file ; pkgs })
  with Error msg -> Error (`Msg msg)

open Cmdliner

let verbose =
  let doc = "Enable verbose output." in
  Arg.(value & flag & info [ "verbose" ] ~doc)

let feats =
  let docv = "features" in
  let doc =
    Fmt.str "Set used features: %s."
      (Arg.doc_alts Index.Feature.all_names)
  in
  Arg.(value & opt Convs.feats Index.Feature.all & info [ "features" ] ~docv ~doc)

let idx_file =
  let docv = "file" in
  let doc = "Set index file." in
  Arg.(value & opt Convs.path Paths.idx_file & info [ "index" ] ~docv ~doc)

let pkgs =
  let docv = "package" in
  Arg.(value & pos_all string [] & info [] ~docv)

let cmd =
  let doc = "save index" in
  Term.(term_result (const main $ copts $ verbose $ feats $ idx_file $ pkgs)),
  Term.(info "save" ~exits:default_exits ~sdocs:Manpage.s_common_options ~doc)
