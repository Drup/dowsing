let prog_name = "dowsindex"

module StringHMap = CCHashtbl.Make (CCString)

let timer = Timer.make ()

let exit ?(code = 0) ?(out = stdout) ?msg () =
  CCOpt.iter (Printf.fprintf out "%s\n") msg ;
  exit code

let error ?msg () =
  let msg = CCOpt.map (Fmt.str "error: %s") msg in
  exit () ~code:1 ~out:stderr ?msg

let type_of_string env str =
  try Type.of_string env str
  with Syntaxerr.Error _ ->
    error () ~msg:"syntax error in type argument."

let pp_table ?(sep = 4) col_names rows =
  let col_cnt = CCArray.length col_names in
  let col_widths = CCArray.map CCString.length col_names in
  let rows = Iter.persistent rows in
  rows |> Iter.iter (fun row ->
    assert (CCArray.length row = col_cnt) ;
    for i = 0 to col_cnt - 1 do
      col_widths.(i) <- max col_widths.(i) @@ CCString.length row.(i)
    done
  ) ;
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
  rows |> Iter.iter (fun row ->
    for i = 0 to col_cnt - 1 do
      CCFormat.print_tab () ;
      CCFormat.print_string row.(i)
    done
  ) ;
  CCFormat.print_tab () ;
  print_hline () ;
  CCFormat.close_tbox ()

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

  let cmds = StringHMap.create 17

  let add_cmd ((module Cmd : Command) as cmd) =
    StringHMap.add cmds Cmd.name cmd

  let parse () =
    let error usage msg =
      error () ~msg:(msg ^ ".\n" ^ usage)
    in
    let usage =
      cmds
      |> StringHMap.keys_list
      |> CCString.concat "|"
      |> Fmt.str "usage: %s {%s} <argument>..." prog_name
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
        let usage = Fmt.str "usage: %s [<option>...] %s\noptions:" name Cmd.usage in
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

  let all_unifs = ref false

  let options = [
    "-a", Arg.Set all_unifs, "\tReport all unifiers" ;
  ]

  let ty1 = ref None
  let ty2 = ref None

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
      Unification.unifiers env ty1 ty2
      |> Iter.sort ~cmp:Unification.Subst.compare
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
        Fmt.(list ~sep:sp @@ Unification.Subst.pp env.var_names) unifs

  let main () =
    if CCOpt.(is_none ! ty1 || is_none ! ty2) then
      raise @@ Arg.Bad "too few arguments" ;
    main ! all_unifs (Option.get ! ty1) (Option.get ! ty2)

end)

(* [save] command *)

let () = Args.add_cmd (module struct

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
          error () ~msg:(Fmt.str "cannot find package '%s'." pkg)
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

end)

(* [stats] command *)

let () = Args.add_cmd (module struct

  let name = "stats"
  let usage = "<file> <type>"

  let filter = ref false

  let meas_kind =
    ref Measure.Kind.VarCount
  let set_meas_kind str =
    meas_kind := Measure.Kind.of_string str
  let meas_kinds_strs =
    Measure.Kind.(CCList.map to_string all)

  let options = [
    "--filter", Arg.Set filter, "\tEnable feature filtering" ;
    "--measure", Arg.Symbol (meas_kinds_strs, set_meas_kind), "\tSet type size kind" ;
  ]

  let file = ref None
  let ty = ref None

  let anon_fun arg =
    if CCOpt.is_none ! file then
      file := Some arg
    else if CCOpt.is_none ! ty then
      ty := Some arg
    else
      raise @@ Arg.Bad "too many arguments"

  let main meas_kind filter file str =
    let idx =
      try Index.load file
      with Sys_error _ ->
        error () ~msg:(Fmt.str "cannot open file '%s'." file)
    in
    let env = Index.get_env idx in
    let ty = type_of_string env str in
    let aux ?stats0 iter_idx =
      let tbl = ref Measure.Map.empty in
      iter_idx (fun (ty', _) ->
        Timer.start timer ;
        ignore @@ Unification.unifiable env ty ty' ;
        Timer.stop timer ;
        let time = Timer.get timer in
        let meas = Measure.make meas_kind ty' in
        tbl := ! tbl |> Measure.Map.update meas @@ function
          | None -> Some (time, 1)
          | Some (time', cnt) -> Some (time +. time', cnt + 1)
      ) ;
      let total_time = ref 0. in
      let total_cnt = ref 0 in
      let rows =
        ! tbl
        |> Measure.Map.to_iter
        |> Iter.map (fun (meas, (time, cnt)) ->
          total_time := ! total_time +. time ;
          total_cnt := ! total_cnt + cnt ;
          [|
            Fmt.to_to_string (Measure.pp meas_kind) meas ;
            Fmt.(to_to_string float) @@ time *. 1e3 ;
            Fmt.(to_to_string float) @@ time /. CCFloat.of_int cnt *. 1e6 ;
            CCInt.to_string cnt ;
          |]
        )
      in
      pp_table [| "size" ; "total time (ms)" ; "avg. time (μs)" ; "# unif." |] rows ;
      let total_time = ! total_time in
      let total_cnt = ! total_cnt in
      Fmt.pr "@,total time: %g" total_time ;
      stats0 |> CCOpt.iter (fun (total_time', _) ->
        Fmt.pr " (%g %%)" @@ total_time /. total_time' *. 100.
      ) ;
      Fmt.pr "@,total # unif.: %i" total_cnt ;
      let total_cnt = CCFloat.of_int total_cnt in
      stats0 |> CCOpt.iter (fun (_, total_cnt') ->
        Fmt.pr " (%g %%)" @@ total_cnt /. total_cnt' *. 100.
      ) ;
      total_time, total_cnt
    in
    Fmt.pr "@[<v>" ;
    let stats0 = aux @@ Index.iter idx in
    if filter then begin
      Fmt.pr "@," ;
      ignore @@ aux ~stats0 @@ Index.iter_with idx ty
    end ;
    Fmt.pr "@]@."

  let main () =
    if CCOpt.(is_none ! file || is_none ! ty) then
      raise @@ Arg.Bad "too few arguments" ;
    main ! meas_kind ! filter (Option.get ! file) (Option.get ! ty)

end)

(* [search] command *)

let () = Args.add_cmd (module struct

  let name = "search"
  let usage = "<file> <type>"

  let exhaustive = ref false

  let cnt = ref None
  let set_cnt i = cnt := Some i

  let options = [
    "--exhaustive", Arg.Set exhaustive, "\tUse exhaustive search (slow)" ;
    "--take", Arg.Int set_cnt, "\tReport only the first results" ;
  ]

  let file = ref None
  let ty = ref None

  let anon_fun arg =
    if CCOpt.is_none ! file then
      file := Some arg
    else if CCOpt.is_none ! ty then
      ty := Some arg
    else
      raise @@ Arg.Bad "too many arguments"

  let main exhaustive cnt file str =
    let idx =
      try Index.load file
      with Sys_error _ ->
        error () ~msg:(Fmt.str "cannot open file '%s'." file)
    in
    let env = Index.get_env idx in
    let ty = type_of_string env str in
    let res =
      let find = if exhaustive then Index.find else Index.find_with in
      find idx env ty
      |> Iter.sort ~cmp:(fun (_, info1, unif1) (_, info2, unif2) ->
        CCOrd.(Unification.Subst.compare unif1 unif2
          <?> (LongIdent.compare, info1.Index.lid, info2.Index.lid))
      )
    in
    let res = CCOpt.fold (CCFun.flip Iter.take) res cnt in
    Fmt.pr "@[<v>" ;
    res |> Iter.iter (fun (ty, info, _) ->
      Fmt.pr "@[<2>%a:@ @[<2>%a@]@]@,"
        LongIdent.pp info.Index.lid
        (Type.pp env.var_names) ty
    ) ;
    Fmt.pr "@]@?"

  let main () =
    if CCOpt.(is_none ! file || is_none ! ty) then
      raise @@ Arg.Bad "too few arguments" ;
    main ! exhaustive ! cnt (Option.get ! file) (Option.get ! ty)

end)

(* main *)

let () =
  let main = Args.parse () in
  Logs.(set_reporter @@ format_reporter ()) ;
  Logs.set_level @@ Some (if ! Args.debug then Logs.Debug else Logs.Info) ;
  main ()
