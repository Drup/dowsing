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
      Common.error () ~msg:(Fmt.str "cannot open file '%s'." file)
  in
  let env = Type.Env.make Query in
  let ty = Common.type_of_string env str in
  let res =
    let find = if exhaustive then Index.find else Index.find_with in
    find idx env ty
    |> Iter.sort ~cmp:(fun (ty1, _, unif1) (ty2, _, unif2) ->
      CCOrd.(Unification.Subst.compare unif1 unif2
        <?> (Type.compare, ty1, ty2))
    )
  in
  let res = CCOpt.fold (CCFun.flip Iter.take) res cnt in
  Fmt.pr "@[<v>%a@]@."
    (Fmt.iter Iter.iter @@ fun fmt (_, info, _) -> Index.Info.pp fmt info) res

let main () =
  if CCOpt.(is_none ! file || is_none ! ty) then
    raise @@ Arg.Bad "too few arguments" ;
  main ! exhaustive ! cnt (Option.get ! file) (Option.get ! ty)
