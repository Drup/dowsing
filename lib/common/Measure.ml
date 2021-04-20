open Type

type kind =
  | VarCount
  | AllVarCount
  | NodeCount
  | HeadKind
  | TailSpineVarCount
  | SpineVarCount
  | TailLength

type t = Int.t

module Map = CCMap.Make (CCInt)
module HMap = CCHashtbl.Make (CCInt)

let pp kind fmt t =
  match kind with
  | HeadKind -> Kind.(pp fmt @@ of_int t)
  | VarCount | AllVarCount | NodeCount
  | TailSpineVarCount | SpineVarCount
  | TailLength -> Fmt.int fmt t

(** The counting functions *)

let var t =
  Variable.Set.cardinal @@ Variable.Set.of_iter @@ vars t

let all_var t =
  Iter.length @@ Type.vars t

let node =
  let rec aux = function
    | Var _ | Other _ ->
      1
    | Constr (_, args) ->
      1 + CCArray.fold (fun acc t -> acc + aux t) 0 args
    | Arrow (args, ret) ->
      1 + aux_set args + aux ret
    | Tuple elts ->
      1 + aux_set elts
  and aux_set set =
    MSet.fold (fun t acc -> aux t + acc) set 0
  in
  aux

let head_kind t = 
  Kind.to_int @@ kind @@ head t

let tail_spine_var t =
  let aux t =
    (+) @@ CCBool.to_int (kind t = Kind.Var)
  in
  MSet.fold aux (tail t) 0

let spine_var t = 
  let sz = tail_spine_var t in
  let hd_kind = kind @@ head t in
  sz + CCBool.to_int (hd_kind = Kind.Var)

let tail_length t =
  MSet.length @@ tail t

let size (sz_kind : kind) t =
  match sz_kind with
  | VarCount -> var t
  | AllVarCount -> all_var t
  | NodeCount -> node t
  | HeadKind -> head_kind t
  | TailSpineVarCount -> tail_spine_var t
  | SpineVarCount -> spine_var t
  | TailLength -> tail_length t
