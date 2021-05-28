open Common

let main _ verbose idx_file pkgs =
  let pkgs_dirs =
    try
      if pkgs = []
      then FindPackage.find_all ()
      else FindPackage.find pkgs
    with
    | FindPackage.Error pkg ->
        error @@ Fmt.str "cannot find package '%s'" pkg
  in
  if verbose then
    Fmt.pr "@[<v2>found %i packages:@ %a@]@."
      (CCList.length pkgs_dirs)
      Fmt.(list ~sep:sp string) pkgs_dirs ;
  Index.(save @@ make pkgs_dirs) idx_file

let main copts verbose idx_file pkgs =
  try Ok (main copts verbose idx_file pkgs)
  with Error msg -> Error (`Msg msg)

open Cmdliner

let verbose =
  let doc = "Enable verbose output." in
  Arg.(value & flag & info [ "verbose" ] ~doc)

let idx_file =
  let docv = "index" in
  Arg.(required & pos 0 (some string) None & info [] ~docv)

let pkgs =
  let docv = "packages" in
  Arg.(value & pos_right 0 string [] & info [] ~docv)

let cmd =
  let doc = "save index" in
  Term.(term_result (const main $ copts $ verbose $ idx_file $ pkgs)),
  Term.(info "save" ~exits:default_exits ~sdocs:Manpage.s_common_options ~doc)
