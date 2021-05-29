open Common

let main _ verbose idx_file pkgs =
  let idx_file =
    match idx_file with
    | Some idx_file -> idx_file
    | None ->
        Bos.OS.Dir.create Paths.data_dir ~path:true
        |> CCResult.iter_err (fun (`Msg msg) -> error msg) ;
        Paths.idx_file
  in
  let pkgs =
    try
      if pkgs = []
      then FindPackage.find_all ()
      else FindPackage.find pkgs
    with
    | FindPackage.Error pkg ->
        error @@ Fmt.str "cannot find package `%s'"
          pkg
  in
  if verbose then
    Fmt.pr "@[<v2>found %i packages:@ %a@]@."
      (CCList.length pkgs)
      Fmt.(list ~sep:sp @@ using snd Fpath.pp) pkgs ;
  let idx =
    try Index.load idx_file
    with Sys_error _ -> Index.make ()
  in
  pkgs |> CCList.iter @@ CCFun.uncurry @@ Index.add idx ;
  try
    Index.save idx idx_file
  with Sys_error _ ->
    error @@ Fmt.str "cannot write index file `%a'"
      Fpath.pp idx_file

let main copts verbose idx_file pkgs =
  try Ok (main copts verbose idx_file pkgs)
  with Error msg -> Error (`Msg msg)

open Cmdliner

let verbose =
  let doc = "Enable verbose output." in
  Arg.(value & flag & info [ "verbose" ] ~doc)

let idx_file =
  let docv = "file" in
  let doc = "Set index file." in
  Arg.(value & opt (some Conv.path) None & info [ "index" ] ~docv ~doc)

let pkgs =
  let docv = "package" in
  Arg.(value & pos_all string [] & info [] ~docv)

let cmd =
  let doc = "save index" in
  Term.(term_result (const main $ copts $ verbose $ idx_file $ pkgs)),
  Term.(info "save" ~exits:default_exits ~sdocs:Manpage.s_common_options ~doc)
