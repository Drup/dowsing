type t = Type.t Variable.Map.t

let empty = Variable.Map.empty
let add = Variable.Map.add

let rec apply (env : Type.Env.t) t =
  let substitute ty = apply env t ty in
  fun (ty : Type.t) ->
    match ty with
    | Var var ->
        CCOption.get_or ~default:ty @@ Variable.Map.get var t
    | Constr (lid, params) ->
        Type.constr env lid @@ CCArray.map substitute params
    | Arrow (params, ret) ->
      Type.(arrows env
              (NSet.map substitute params) (substitute ret))
    | Tuple elts ->
        Type.(tuple env @@ NSet.map substitute elts)
    | Other _ | FrozenVar _ ->
        ty

let simplify _vars t = t

let size = Variable.Map.cardinal
let compare t1 t2 = compare (size t1) (size t2)
let lt t1 t2 = compare t1 t2 < 0

let pp =
  Fmt.vbox @@
    Variable.Map.pp ~pp_sep:Fmt.cut ~pp_arrow:(Fmt.any " -> ")
      Variable.pp Type.pp_parens
