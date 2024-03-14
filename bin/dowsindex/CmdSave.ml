open CommonOpts

type opts = {
  copts : copts;
  verbose : bool;
  idx_file : Fpath.t;
  dependencies : bool ;
  with_poset : bool ;
  pkgs : String.t list;
  files : Fpath.t list
}

let main opts =
  Bos.OS.Dir.create ~path:true @@ Fpath.parent opts.idx_file
  |> CCResult.iter_err (fun (`Msg msg) -> error msg);
  let pkgs =
    try
      if opts.pkgs = [] && opts.files = [] then
        Dowsing_odig.find_all ()
      else
        Dowsing_odig.find ~dependencies:opts.dependencies opts.pkgs
    with Dowsing_odig.Unbound_package pkg ->
      error @@ Fmt.str "cannot find package `%s'" pkg
  in
  if opts.verbose then
    Fmt.pr "@[<v2>found %i packages:@ %a@]@." (CCList.length pkgs)
      Fmt.(list ~sep:sp @@ using fst string)
      pkgs;
  let pkgs =
    if opts.files <> []
    then ("", opts.files) :: pkgs
    else pkgs
  in
  let infos =
    Dowsing_odoc.iter pkgs
  in
  let db = Db.create ~with_poset:opts.with_poset env_data infos in
  Fmt.pr "@[<2>Create an index:@,%a@]@."
    Db.DefaultIndex.pp_metrics db.idx;
  try
    Db.save db opts.idx_file
  with Sys_error _ ->
    error @@ Fmt.str "cannot write index file `%a'" Fpath.pp opts.idx_file

let main copts verbose idx_file dependencies pkgs files with_poset =
  let opts =
    { copts; verbose; idx_file; dependencies; pkgs; files; with_poset }
  in
  try Ok (main opts) with Error msg -> Error (`Msg msg)

open Cmdliner

let verbose =
  let doc = "Enable verbose output." in
  Arg.(value & flag & info [ "verbose" ] ~doc)

let idx_file =
  let docv = "INDEX" in
  let doc = "Set index file." in
  Arg.(value & opt Convs.path Paths.idx_file & info [ "index" ] ~docv ~doc)

let pkgs =
  let docv = "PACKAGES" in
  let doc = "Indexed packages" in
  Arg.(value & opt Arg.(list string) [] & info ["p";"pkgs"] ~docv ~doc)

let dependencies =
  let doc = "Load package dependencies" in
  Arg.(value & flag & info ["dependencies"] ~doc)

let odocls =
  let docv = "ODOCL" in
  let doc = "Indexed .odocl files" in
  Arg.(value & pos_all Convs.file [] & info [] ~docv ~doc)

let with_poset =
  let doc = "Enable poset construction" in
  Arg.(value & opt ~vopt:true bool false & info ["poset"] ~doc)

let cmd =
  let doc = "save index" in
  Cmd.v
    (Cmd.info "save" ~sdocs:Manpage.s_common_options ~doc)
    Term.(term_result (
        const main
        $ copts $ verbose
        $ idx_file
        $ dependencies $ pkgs $ odocls
        $ with_poset
      ))
