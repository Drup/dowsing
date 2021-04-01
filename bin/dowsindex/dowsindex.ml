let timer = Timer.make ()

let usage () =
  CCFormat.printf "usage: dowsindex [unify | save | test] <args>...@." ;
  exit 1

let unify str1 str2 =
  let env = Type.Env.make () in
  let ty1 = Type.of_string env str1 in
  let ty2 = Type.of_string env str2 in
  CCFormat.printf "@[<2>t1:@ %a@]@." (Type.pp env.var_names) ty1 ;
  CCFormat.printf "@[<2>t2:@ %a@]@." (Type.pp env.var_names) ty2 ;
  let unifs = Iter.to_list @@ Unification.unify env [ ty1, ty2 ] in
  CCFormat.printf "@[<v2>Unifiers@ %a@]@."
    Fmt.(list ~sep:sp @@ Unification.Unifier.pp env.var_names) unifs

let save file_name =
  Index.(save @@ make ()) file_name

let test file_name str =
  let env = Type.Env.make () in
  let ty' = Type.of_string env str in
  let idx = Index.load file_name in
  Timer.start timer ;
  idx
  |> Index.iter (fun _ Index.{ ty } ->
    ignore @@ Unification.unifiable env [ ty, ty' ]) ;
  Timer.stop timer ;
  CCFormat.printf "exhaustive lookup: %f@." (Timer.get timer)

let () =
  CCFormat.set_margin 100 ;
  if Array.length Sys.argv < 2 then
    usage () ;
  match Sys.argv.(1) with
  | "unify" ->
      if Array.length Sys.argv < 4 then
        usage () ;
      unify Sys.argv.(2) Sys.argv.(3)
  | "save" ->
      if Array.length Sys.argv < 3 then
        usage () ;
      save Sys.argv.(2)
  | "test" ->
      if Array.length Sys.argv < 4 then
        usage () ;
      test Sys.argv.(2) Sys.argv.(3)
  | _ ->
      usage ()
