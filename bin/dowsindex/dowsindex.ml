let prog_name = "dowsindex"

let timer = Timer.make ()

let exit ?(code = 0) ?(out = stdout) ?msg () =
  CCOpt.iter (Printf.fprintf out "%s\n") msg ;
  exit code

let error msg =
  let msg = Printf.sprintf "error: %s" msg in
  exit () ~code:1 ~out:stderr ~msg

let type_of_string env str =
  try Type.of_string env str
  with Syntaxerr.Error _ ->
    error "syntax error in type argument."

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
    let usage =
      cmds
      |> StringHMap.keys_list
      |> CCString.concat "|"
      |> Printf.sprintf "usage: %s {%s} <argument>..." prog_name
    in
    let error' = error usage in
    if CCArray.length Sys.argv < 2 then
      error' "unspecified subcommand" ;
    if CCArray.mem Sys.argv.(1) [| "-help" ; "--help" |] then
      exit () ~msg:usage ;
    match StringHMap.get cmds Sys.argv.(1) with
    | None ->
        error' "illegal subcommand"
    | Some (module Cmd) ->
        let name = prog_name ^ " " ^ Cmd.name in
        let options = Arg.align @@ options @ Cmd.options in
        let usage = Printf.sprintf "usage: %s [<option>...] %s\noptions:" name Cmd.usage in
        Sys.argv.(1) <- "error" ;
        Arg.current := 1 ;
        Arg.parse options Cmd.anon_fun usage ;
        fun () ->
          try Cmd.main ()
          with Arg.Bad msg ->
            let usage = CCString.rtrim @@ Arg.usage_string options usage in
            error usage msg

end

(* [unify] command *)

let () = Args.add_cmd (module struct

  let name = "unify"
  let usage = "<type1> <type2>"

  let ty1 = ref None
  let ty2 = ref None
  let all_unifs = ref false

  let options = [
    "-a", Arg.Set all_unifs, "\tReport all unifiers" ;
  ]

  let anon_fun arg =
    if CCOpt.is_none ! ty1 then
      ty1 := Some arg
    else if CCOpt.is_none ! ty2 then
      ty2 := Some arg
    else
      raise @@ Arg.Bad "too many arguments"

  let main all_unifs str1 str2 =
    let env = Type.Env.make () in
    let ty1 = type_of_string env str1 in
    let ty2 = type_of_string env str2 in
    Logs.debug (fun m -> m "@[<2>type1:@ %a@]" (Type.pp env.var_names) ty1) ;
    Logs.debug (fun m -> m "@[<2>type2:@ %a@]" (Type.pp env.var_names) ty2) ;
    let unifs =
      [ ty1, ty2 ]
      |> Unification.unifiers env
      |> Iter.sort ~cmp:Unification.Unifier.compare
      |> Iter.to_list
    in
    let unifs =
      if all_unifs then unifs
      else CCList.take 1 unifs
    in
    if unifs = [] then
      Fmt.pr "no unifier@."
    else
      Fmt.pr "@[<v2>unifiers:@ %a@]@."
        Fmt.(list ~sep:sp @@ Unification.Unifier.pp env.var_names) unifs

  let main () =
    if CCOpt.(is_none ! ty1 || is_none ! ty2) then
      raise @@ Arg.Bad "too few arguments" ;
    main ! all_unifs (Option.get ! ty1) (Option.get ! ty2)

end)

(* [save] command *)

let () = Args.add_cmd (module struct

  let name = "save"
  let usage = "<file> <package>..."
  let options = []

  let file = ref None
  let pkgs = ref []

  let anon_fun arg =
    if CCOpt.is_none ! file then
      file := Some arg
    else
      pkgs := ! pkgs @ [ arg ]

  let main file pkgs =
    Findlib.init () ;
    let pkgs () =
      if pkgs = [] then
        Findlib.list_packages' ()
      else
        let pkgs = Findlib.package_deep_ancestors [] pkgs in
        Logs.app (fun m ->
            m "@[<hv 2>Findlib packages:@ %a@]"
              Fmt.(list ~sep:sp string) pkgs
          );
        pkgs
    in
    let pkg_dirs =
      try
        pkgs ()
        |> CCList.map Findlib.package_directory
        (* we need this because [Findlib.list_packages'] is faulty:
           it gives some unknown packages *)
        |> CCList.filter Sys.file_exists
      with
      | Findlib.No_such_package (pkg, _) ->
        Logs.err (fun m -> m "Cannot find package '%s'." pkg);
        exit ~code:1 ()
    in
    Logs.app (fun m -> m "Found %i findlib packages" (List.length pkg_dirs));
    Index.(save @@ make pkg_dirs) file

  let main () =
    if CCOpt.is_none ! file then
      raise @@ Arg.Bad "too few arguments" ;
    main (Option.get ! file) ! pkgs

end)

(* [stats] command *)

let () = Args.add_cmd (module struct

  let name = "stats"
  let usage = "<file> <type>"

  let file = ref None
  let ty = ref None
  let filter = ref false

  let sz_kind = ref Type.Size.VarCount
  let sz_kind_syms =
    let open Type.Size in [
      "vars", VarCount ;
      "nodes", NodeCount ;
      "head", HeadKind ;
      "tail-root-vars", TailRootVarCount ;
      "root-vars", RootVarCount ;
      "tail-length", TailLength ;
    ]
  let set_sz_kind =
    let tbl = Hashtbl.create @@ CCList.length sz_kind_syms in
    CCList.iter (CCFun.uncurry @@ Hashtbl.add tbl) sz_kind_syms ;
    fun sym ->
      match Hashtbl.find_opt tbl sym with
      | Some sz_kind' -> sz_kind := sz_kind'
      | None -> assert false
  let sz_kind_syms =
    CCList.map fst sz_kind_syms

  let options = [
    "--size", Arg.Symbol (sz_kind_syms, set_sz_kind), "\tSet type size kind" ;
    "--filter", Arg.Set filter, "\tEnable feature filtering" ;
  ]

  let anon_fun arg =
    if CCOpt.is_none ! file then
      file := Some arg
    else if CCOpt.is_none ! ty then
      ty := Some arg
    else
      raise @@ Arg.Bad "too many arguments"

  let main sz_kind filter file str =
    let idx =
      try Index.load file
      with Sys_error _ ->
        error @@ Printf.sprintf "cannot open file '%s'." file
    in
    let env = Index.get_env idx in
    let ty = type_of_string env str in
    let iter_idx =
      if filter then
        Index.iteri' idx env ty
      else
        Index.iteri idx
    in
    let tbl = ref Type.Size.Map.empty in
    iter_idx (fun (ty', _) ->
      Timer.start timer ;
      ignore @@ Unification.unifiable env [ ty, ty' ] ;
      Timer.stop timer ;
      let time = Timer.get timer in
      let sz = Type.size sz_kind ty' in
      tbl := ! tbl |> Type.Size.Map.update sz @@ function
        | None -> Some (time, 1)
        | Some (time', cnt) -> Some (time +. time', cnt + 1)
    ) ;
    let col_names = [| "size" ; "total time (ms)" ; "avg. time (μs)" ; "# unif." |] in
    let sep = 4 in
    let col_cnt = CCArray.length col_names in
    let col_widths = CCArray.map CCString.length col_names in
    let total_time = ref 0. in
    let tbl =
      ! tbl |> Type.Size.Map.mapi (fun sz (time, cnt) ->
        total_time := ! total_time +. time ;
        let row = [|
          CCFormat.asprintf "%a" (Type.Size.pp sz_kind) sz ;
          Printf.sprintf "%g" @@ 1e3 *. time ;
          Printf.sprintf "%g" @@ 1e6 *. time /. CCFloat.of_int cnt ;
          CCInt.to_string cnt ;
        |] in
        for i = 0 to col_cnt - 1 do
          col_widths.(i) <- max col_widths.(i) @@ CCString.length row.(i)
        done ;
        row
      )
    in
    for i = 0 to col_cnt - 2 do
      col_widths.(i) <- col_widths.(i) + sep
    done ;
    let print_hline =
      let width = CCArray.fold (+) 0 col_widths in
      let hline = CCString.make width '-' in
      fun () -> CCFormat.print_string hline
    in
    CCFormat.open_tbox () ;
    print_hline () ;
    CCFormat.printf "@\n" ;
    for i = 0 to col_cnt - 1 do
      CCFormat.set_tab () ;
      CCFormat.printf "%-*s" col_widths.(i) col_names.(i)
    done ;
    CCFormat.print_tab () ;
    print_hline () ;
    Type.Size.Map.values tbl (fun row ->
      for i = 0 to col_cnt - 1 do
        CCFormat.print_tab () ;
        CCFormat.print_string row.(i)
      done
    ) ;
    CCFormat.print_tab () ;
    print_hline () ;
    CCFormat.close_tbox () ;
    CCFormat.printf "@\ntotal time: %g@." ! total_time

  let main () =
    if CCOpt.(is_none ! file || is_none ! ty) then
      raise @@ Arg.Bad "too few arguments" ;
    main ! sz_kind ! filter (Option.get ! file) (Option.get ! ty)

end)

(* [search] command *)

let () = Args.add_cmd (module struct

  let name = "search"
  let usage = "<file> <type>"
  let options = []

  let file = ref None
  let ty = ref None

  let anon_fun arg =
    if CCOpt.is_none ! file then
      file := Some arg
    else if CCOpt.is_none ! ty then
      ty := Some arg
    else
      raise @@ Arg.Bad "too many arguments"

  let main file str =
    let env = Type.Env.make () in
    let ty = type_of_string env str in
    let idx =
      try Index.load file
      with Sys_error _ ->
        error @@ Printf.sprintf "cannot open file '%s'." file
    in
    let aux k (ty', lid) =
      [ ty, ty' ]
      |> Unification.unify env
      |> CCOpt.iter (fun unif -> k (Unification.Unifier.size unif, lid, ty'))
    in
    let res =
      (fun k -> Index.iteri' idx env ty @@ aux k)
      |> Iter.sort ~cmp:CCOrd.(triple int LongIdent.compare Type.compare)
    in
    Fmt.pr "@[<v>" ;
    res |> Iter.iter (fun (_, lid, ty) ->
      Fmt.pr "@[<2>%a:@ @[<2>%a@]@]@,"
        LongIdent.pp lid
        (Type.pp env.var_names) ty
    ) ;
    Fmt.pr "@]@?"

  let main () =
    if CCOpt.(is_none ! file || is_none ! ty) then
      raise @@ Arg.Bad "too few arguments" ;
    main (Option.get ! file) (Option.get ! ty)

end)

(* main *)

let () =
  let main = Args.parse () in
  Logs.(set_reporter @@ format_reporter ()) ;
  Logs.set_level @@ Some (if ! Args.debug then Logs.Debug else Logs.Info) ;
  main ()
