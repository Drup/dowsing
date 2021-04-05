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
        let usage = Printf.sprintf "usage: %s [<options>] %s\noptions:" name Cmd.usage in
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
    if CCOpt.(is_none ! ty1 || is_none ! ty2) then
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

  let file_name = ref None
  let ty = ref None

  let sz_kind = ref Type.Size.VarCount
  let sz_kind_syms = [ "vars" ; "nodes" ; "head" ]
  let set_sz_kind = function
    | "vars" -> sz_kind := VarCount
    | "nodes" -> sz_kind := NodeCount
    | "head" -> sz_kind := HeadKind
    | _ -> assert false

  let options = [
    "--size", Arg.Symbol (sz_kind_syms, set_sz_kind), "\tSet type size kind" ;
  ]

  let anon_fun arg =
    if CCOpt.is_none ! file_name then
      file_name := Some arg
    else if CCOpt.is_none ! ty then
      ty := Some arg
    else
      raise @@ Arg.Bad "too many arguments"

  let main sz_kind file_name str =
    let env = Type.Env.make () in
    let ty' = type_of_string env str in
    let idx =
      try Index.load file_name
      with Sys_error _ ->
        error @@ Printf.sprintf "cannot open file '%s'" file_name
    in
    let tbl = ref Type.Size.Map.empty in
    idx |> Index.iter (fun _ Index.{ ty } ->
      Timer.start timer ;
      ignore @@ Unification.unifiable env [ ty, ty' ] ;
      Timer.stop timer ;
      let time = Timer.get timer in
      let sz = Type.size sz_kind ty in
      tbl := ! tbl |> Type.Size.Map.update sz @@ function
        | None -> Some (time, 1)
        | Some (time', cnt) -> Some (time +. time', cnt + 1)
    ) ;
    let col_names = [| "size" ; "avg. unif." ; "# unif." |] in
    let sep = 3 in
    let col_cnt = CCArray.length col_names in
    let col_widths = CCArray.map CCString.length col_names in
    let tbl =
      ! tbl |> Type.Size.Map.mapi (fun sz (time, cnt) ->
        let row = [|
          CCFormat.asprintf "%a" (Type.Size.pp sz_kind) sz ;
          Printf.sprintf "%g" @@ time /. CCFloat.of_int cnt ;
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
    CCFormat.open_tbox () ;
    for i = 0 to col_cnt - 1 do
      CCFormat.set_tab () ;
      CCFormat.printf "%-*s" col_widths.(i) col_names.(i)
    done ;
    CCFormat.print_tab () ;
    CCFormat.print_string @@ CCString.make (CCArray.fold (+) 0 col_widths) '-' ;
    Type.Size.Map.values tbl (fun row ->
      for i = 0 to col_cnt - 1 do
        CCFormat.print_tab () ;
        CCFormat.print_string row.(i)
      done
    ) ;
    CCFormat.close_tbox () ;
    CCFormat.print_newline ()

  let main () =
    if CCOpt.(is_none ! file_name || is_none ! ty) then
      raise @@ Arg.Bad "too few arguments" ;
    main ! sz_kind (Option.get ! file_name) (Option.get ! ty)

end)

(* main *)

let () =
  CCFormat.set_margin 100 ;
  Args.parse ()
