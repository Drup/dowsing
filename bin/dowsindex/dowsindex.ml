let prog_name = "dowsindex"

let timer = Timer.make ()

let exit ?(code = 0) () =
  exit code

let error msg =
  Printf.eprintf "error: %s\n" msg ;
  exit () ~code:1

let type_of_string env str =
  try Type.of_string env str
  with Syntaxerr.Error _ ->
    error "syntax error"

(* command line arguments *)

module Args = struct

  module type Command = sig
      val name : String.t
      val usage : String.t
      val options : (Arg.key * Arg.spec * Arg.doc) List.t
      val anon_fun : String.t -> Unit.t
      val main : Unit.t -> Unit.t
  end

  let debug = ref false

  let options = [
    "--debug", Arg.Set debug, "\tEnable debug mode" ;
  ]

  module StringHMap = CCHashtbl.Make (CCString)
  let cmds = StringHMap.create 17

  let add_cmd ((module Cmd : Command) as cmd) =
    StringHMap.add cmds Cmd.name cmd

  let parse () =
    let error usage msg =
      error @@ msg ^ ".\n" ^ usage
    in
    let error' =
      cmds
      |> StringHMap.keys_list
      |> CCString.concat " | "
      |> Printf.sprintf "usage: %s [ %s ] <arguments>" prog_name
      |> error
    in
    if CCArray.length Sys.argv < 2 then
      error' "unspecified subcommand" ;
    match StringHMap.get cmds Sys.argv.(1) with
    | None ->
        error' "illegal subcommand"
    | Some (module Cmd) ->
        let name = prog_name ^ " " ^ Cmd.name in
        let options = Arg.align @@ options @ Cmd.options in
        let usage = Printf.sprintf "usage: %s [<options>] %s" name Cmd.usage in
        Sys.argv.(1) <- "error" ;
        Arg.current := 1 ;
        Arg.parse options Cmd.anon_fun usage ;
        try Cmd.main ()
        with Arg.Bad msg ->
          let usage = CCString.rtrim @@ Arg.usage_string options usage in
          error usage msg

end

(* [unify] command *)

let () = Args.add_cmd (module struct

  let name = "unify"
  let usage = "<type1> <type2>"
  let options = []

  let ty1 = ref None
  let ty2 = ref None

  let anon_fun arg =
    if CCOpt.is_none ! ty1 then
      ty1 := Some arg
    else if CCOpt.is_none ! ty2 then
      ty2 := Some arg
    else
      raise @@ Arg.Bad "too many arguments"

  let main str1 str2 =
    let env = Type.Env.make () in
    let ty1 = type_of_string env str1 in
    let ty2 = type_of_string env str2 in
    CCFormat.printf "@[<2>t1:@ %a@]@." (Type.pp env.var_names) ty1 ;
    CCFormat.printf "@[<2>t2:@ %a@]@." (Type.pp env.var_names) ty2 ;
    let unifs = Iter.to_list @@ Unification.unify env [ ty1, ty2 ] in
    if not @@ CCList.is_empty unifs then
      CCFormat.printf "@[<v2>Unifiers@ %a@]@."
        Fmt.(list ~sep:sp @@ Unification.Unifier.pp env.var_names) unifs

  let main () =
    if Option.(is_none ! ty1 || is_none ! ty2) then
      raise @@ Arg.Bad "too few arguments" ;
    main (Option.get ! ty1) (Option.get ! ty2)

end)

(* [save] command *)

let () = Args.add_cmd (module struct

  let name = "save"
  let usage = "<file>"
  let options = []

  let file_name = ref None

  let anon_fun arg =
    if CCOpt.is_none ! file_name then
      file_name := Some arg
    else
      raise @@ Arg.Bad "too many arguments"

  let main file_name =
    Index.(save @@ make ()) file_name

  let main () =
    if CCOpt.is_none ! file_name then
      raise @@ Arg.Bad "too few arguments" ;
    main @@ Option.get ! file_name

end)

(* [stats] command *)

let () = Args.add_cmd (module struct

  let name = "stats"
  let usage = "<file> <type>"
  let options = []

  let file_name = ref None
  let ty = ref None

  let anon_fun arg =
    if CCOpt.is_none ! file_name then
      file_name := Some arg
    else if CCOpt.is_none ! ty then
      ty := Some arg
    else
      raise @@ Arg.Bad "too many arguments"

  let main file_name str =
    let env = Type.Env.make () in
    let ty' = type_of_string env str in
    let idx = Index.load file_name in
    Timer.start timer ;
    idx
    |> Index.iter (fun _ Index.{ ty } ->
      ignore @@ Unification.unifiable env [ ty, ty' ]) ;
    Timer.stop timer ;
    CCFormat.printf "exhaustive lookup: %f@." @@ Timer.get timer

  let main () =
    if CCOpt.(is_none ! file_name || is_none ! ty) then
      raise @@ Arg.Bad "too few arguments" ;
    main (Option.get ! file_name) (Option.get ! ty)

end)

(* main *)

let () =
  CCFormat.set_margin 100 ;
  Args.parse ()
