type t = Type.t Variable.HMap.t

let rec apply t =
  let substitute ty = apply t ty in
  fun (ty : Type.t) ->
    match ty with
    | Var var ->
        CCOpt.get_or ~default:ty @@ Variable.HMap.get t var
    | Constr (lid, params) ->
        Type.constr lid @@ CCArray.map substitute params
    | Arrow (params, ret) ->
        Type.(arrow (tuple @@ NSet.map substitute params) (substitute ret))
    | Tuple elts ->
        Type.(tuple @@ NSet.map substitute elts)
    | Other _ ->
        ty

let simplify _vars t = t

let size = Variable.HMap.length
let compare t1 t2 = compare (size t1) (size t2)
let lt t1 t2 = compare t1 t2 < 0

let pp =
  Fmt.vbox @@
    Variable.HMap.pp ~pp_sep:Fmt.cut ~pp_arrow:(Fmt.any " -> ")
      Variable.pp Type.pp_parens
