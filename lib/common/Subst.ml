type t = Type.t Variable.Map.t

let empty = Variable.Map.empty
let add = Variable.Map.add

let rec apply (env : Type.Env.t) t =
  let substitute ty = apply env t ty in
  fun (ty : Type.t) ->
    match ty with
    | Var var -> (
        match Variable.Map.get var t with
        | None -> ty
        | Some ty ->
            assert (not (Type.variable_clash var ty));
            substitute ty)
    | Constr (lid, params) ->
        Type.constr env lid @@ CCArray.map substitute params
    | Arrow (params, ret) ->
        Type.(arrows env (NSet.map substitute params) (substitute ret))
    | Tuple elts -> Type.(tuple env @@ NSet.map substitute elts)
    | Other _ | FrozenVar _ -> ty

let simplify env vars t =
  let unfold var =
    match Variable.Map.get var t with
    | Some ty -> Some (var, apply env t ty)
    | None -> None
  in
  vars |> Variable.Set.to_iter |> Iter.filter_map unfold |> Variable.Map.of_iter

let cardinal = Variable.Map.cardinal
let weight_ty = function Type.Var _ -> 0 | ty -> Measure.make NodeCount ty
let size t = Variable.Map.values t |> Iter.map weight_ty |> Iter.sum

let compare t1 t2 =
  CCOrd.(
    int (size t1) (size t2)
    (* <?> (int, size t1, size t2) *)
    <?> (Variable.Map.compare Type.compare, t1, t2))

let lt t1 t2 = compare t1 t2 < 0

let pp =
  Fmt.vbox
  @@ Variable.Map.pp ~pp_sep:Fmt.cut ~pp_arrow:(Fmt.any " -> ") Variable.pp
       (Fmt.box Type.pp_parens)
