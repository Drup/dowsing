(** Utilities to record and expand type aliases.

    Type aliases are only stored *fully expanded*.
    Furthermore, their parameter variables are free in their arguments.
    This means we never need to substitute in their expansion.
*)

type tbl = (Variable.var array * Type.t) LongIdent.HMap.t

(** [type_apply env ['a₁...'aₙ] τ [τ₁,...τₙ]]
    applies [Λ'a₁...'aₙ.τ] to [τ₁,...τₙ]. *)
let apply (env : Type.Env.t) params t0 args =
  let sub =
    CCArray.fold2
      (fun t v k -> Variable.Map.add v k t)
      Variable.Map.empty
      params args
  in
  let rec substitute (ty : Type.t) =
    match ty with
    | Var var -> (
        match Variable.Map.get var sub with
        | None -> ty
        | Some ty -> ty)
    | Constr (lid, params) ->
        Type.constr env lid @@ CCArray.map substitute params
    | Arrow (params, ret) ->
        Type.(arrows env (NSet.map substitute params) (substitute ret))
    | Tuple elts -> Type.(tuple env @@ NSet.map substitute elts)
    | Other _ | FrozenVar _ -> ty
  in
  substitute t0

let rec expand_type env tbl (t : Type.t) = match t with
  | Var _
  | FrozenVar _
  | Other _
    -> t
  | Constr (lid, args) ->
    let args = Array.map (expand_type env tbl) args in
    begin match LongIdent.HMap.get tbl lid with
      | None ->
        Type.constr env lid args
      | Some (params, def) ->
        apply env params def args
    end
  | Arrow (tail, head) ->
    Type.arrows env
      (Type.NSet.map (expand_type env tbl) tail)
      (expand_type env tbl head)    
  | Tuple tup ->
    Type.tuple env
      (Type.NSet.map (expand_type env tbl) tup)

let add env tbl lid params def =
  let def = expand_type env tbl def in
  (* Assert that types are closed after expansion *)
  assert(
    Iter.for_all (fun v -> List.exists (Variable.equal v) params)
    @@ Type.iter_vars def
  );
  LongIdent.HMap.add tbl lid (Array.of_list params, def)
