let name = "save"
let usage = "<file> <package>..."

let verbose = ref false

let options = [
  "--verbose", Arg.Set verbose, "\tEnable verbose output" ;
]

let file = ref None
let pkgs = ref []

let anon_fun arg =
  if CCOpt.is_none ! file then
    file := Some arg
  else
    pkgs := ! pkgs @ [ arg ]

let main verbose file pkgs =
  let pkgs_dirs =
    try
      if pkgs = [] then
        FindPackage.find_all ()
      else
        FindPackage.find pkgs
    with
    | FindPackage.Error pkg ->
        Common.error () ~msg:(Fmt.str "cannot find package '%s'." pkg)
  in
  if verbose then
    Fmt.pr "@[<v2>found %i packages:@ %a@]@."
      (CCList.length pkgs_dirs)
      Fmt.(list ~sep:sp string) pkgs_dirs ;
  Index.(save @@ make pkgs_dirs) file

let main () =
  if CCOpt.is_none ! file then
    raise @@ Arg.Bad "too few arguments" ;
  main ! verbose (Option.get ! file) ! pkgs
