open CommonOpts

type opts = {
  copts : copts;
  verbose : Bool.t;
  idx_file : Fpath.t;
  pkgs : String.t List.t;
}

let main opts =
  let module Index = (val opts.copts.idx) in
  Bos.OS.Dir.create ~path:true @@ Fpath.parent opts.idx_file
  |> CCResult.iter_err (fun (`Msg msg) -> error msg);
  let pkgs =
    try
      if opts.pkgs = [] then
        Dowsing_findlib.find_all ()
      else
        Dowsing_findlib.find opts.pkgs
    with Dowsing_findlib.Error pkg ->
      error @@ Fmt.str "cannot find package `%s'" pkg
  in
  if opts.verbose then
    Fmt.pr "@[<v2>found %i packages:@ %a@]@." (CCList.length pkgs)
      Fmt.(list ~sep:sp @@ using snd Fpath.pp)
      pkgs;
  let idx =
    try Index.load opts.idx_file with Sys_error _ -> Index.make env_data
  in
  Index.import idx @@ List.map (fun (pkg, dir) -> (pkg, dir, Dowsing_libindex.iter dir)) pkgs;
  Fmt.pr "@[<2>Create an index:@,%a@]@."
    Index.pp_metrics idx;
  try
    Index.save idx opts.idx_file
  with Sys_error _ ->
    error @@ Fmt.str "cannot write index file `%a'" Fpath.pp opts.idx_file

let main copts verbose idx_file pkgs =
  try Ok (main { copts; verbose; idx_file; pkgs })
  with Error msg -> Error (`Msg msg)

open Cmdliner

let verbose =
  let doc = "Enable verbose output." in
  Arg.(value & flag & info [ "verbose" ] ~doc)

let idx_file =
  let docv = "file" in
  let doc = "Set index file." in
  Arg.(value & opt Convs.path Paths.idx_file & info [ "index" ] ~docv ~doc)

let pkgs =
  let docv = "package" in
  Arg.(value & pos_all string [] & info [] ~docv)

let cmd =
  let doc = "save index" in
  Cmd.v
    (Cmd.info "save" ~sdocs:Manpage.s_common_options ~doc)
    Term.(term_result (const main $ copts $ verbose $ idx_file $ pkgs))
