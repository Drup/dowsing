module StringHMap = CCHashtbl.Make (CCString)

module Kind = struct

  type t =
    | VarCount
    | UniqueVarCount
    | NodeCount
    | HeadKind
    | TailSpineVarCount
    | TailSpineNonVarCount
    | SpineVarCount
    | TailLength

  let all = [
    VarCount, "vars" ;
    UniqueVarCount, "unique-vars" ;
    NodeCount, "nodes" ;
    HeadKind, "head" ;
    TailSpineVarCount, "tail-spine-vars" ;
    TailSpineNonVarCount, "tail-spine-non-vars" ;
    SpineVarCount, "spine-vars" ;
    TailLength, "tail-length" ;
  ]

  let of_string =
    let tbl = StringHMap.create @@ CCList.length all in
    CCList.iter (fun (t, str) -> StringHMap.add tbl str t) all ;
    fun str ->
      match StringHMap.get tbl str with
      | Some t -> t
      | None -> invalid_arg "Common.Measure.Kind.of_string"

  let to_string t = CCList.assoc ~eq:(=) t all

  let all = List.map fst all

  let pp = Fmt.of_to_string to_string

end

type t = Int.t

let rec make (kind : Kind.t) ty =
  match kind with
  | VarCount ->
      Iter.length @@ Type.iter_vars ty
  | UniqueVarCount ->
      Variable.Set.(cardinal @@ of_iter @@ Type.iter_vars ty)
  | NodeCount ->
      let rec aux (ty : Type.t) =
        match ty with
        | Var _ | Other _ -> 1
        | Constr (_, args) -> 1 + aux_array args
        | Arrow (args, ret) -> 1 + aux_set args + aux ret
        | Tuple elts -> 1 + aux_set elts
      and aux_array ty_arr =
        CCArray.fold (fun acc ty -> acc + aux ty) 0 ty_arr
      and aux_set ty_set =
        Type.NSet.fold (fun ty acc -> aux ty + acc) ty_set 0
      in
      aux ty
  | HeadKind ->
      Type.(Kind.to_int @@ kind @@ head ty)
  | TailSpineVarCount ->
      let aux ty =
        (+) @@ CCBool.to_int Type.(kind ty = Var)
      in
      Type.(NSet.fold aux (tail ty) 0)
  | TailSpineNonVarCount ->
      let aux ty =
        (+) @@ CCBool.to_int Type.(kind ty <> Var)
      in
      Type.(NSet.fold aux (tail ty) 0)
  | SpineVarCount ->
      let meas = make TailSpineVarCount ty in
      let hd_kind = Type.(kind @@ head ty) in
      meas + CCBool.to_int (hd_kind = Var)
  | TailLength ->
      Type.(NSet.length @@ tail ty)

module Map = CCMap.Make (CCInt)
module HMap = CCHashtbl.Make (CCInt)

let pp (kind : Kind.t) fmt t =
  match kind with
  | HeadKind -> Type.Kind.(pp fmt @@ of_int t)
  | _ -> Fmt.int fmt t
