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
  let env = Type.Env.make Query in
  let ty1 = Common.type_of_string env str1 in
  let ty2 = Common.type_of_string env str2 in
  Logs.info (fun m -> m "@[<2>type1:@ %a@]" Type.pp ty1) ;
  Logs.info (fun m -> m "@[<2>type2:@ %a@]" Type.pp ty2) ;
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
      Fmt.(list ~sep:sp Unification.Subst.pp) unifs

let main () =
  if CCOpt.(is_none ! ty1 || is_none ! ty2) then
    raise @@ Arg.Bad "too few arguments" ;
  main ! all_unifs (Option.get ! ty1) (Option.get ! ty2)
