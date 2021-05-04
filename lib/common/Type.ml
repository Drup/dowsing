module StringHMap = CCHashtbl.Make (CCString)

(* Kind *)

module Kind = struct

  type t =
    | Var
    | Constr
    | Arrow
    | Tuple
    | Other

  let to_int = function
    | Var -> 0
    | Constr -> 1
    | Arrow -> 2
    | Tuple -> 3
    | Other -> 4

  let of_int = function
    | 0 -> Var
    | 1 -> Constr
    | 2 -> Arrow
    | 3 -> Tuple
    | 4 -> Other
    | _ -> assert false

  let to_string = function
    | Var -> "variable"
    | Constr -> "constructor"
    | Arrow -> "arrow"
    | Tuple -> "tuple"
    | Other -> "other"

  let compare t1 t2 = CCInt.compare (to_int t1) (to_int t2)
  let equal t1 t2 = compare t1 t2 = 0
  let hash = CCHash.poly

  module Map = CCMap.Make (struct
    type nonrec t = t
    let compare = compare
  end)

  module HMap = CCHashtbl.Make (struct
    type nonrec t = t
    let equal = equal
    let hash = hash
  end)

  module MSet = CCMultiSet.Make (struct
    type nonrec t = t
    let compare = compare
  end)

  let pp fmt t =
    Fmt.string fmt @@ to_string t

end

(* Kind' *)

module Kind' = struct

  type t =
    | Var
    | Constr of LongIdent.t
    | Arrow
    | Tuple
    | Other

  let to_int = function
    | Var -> 0
    | Constr _ -> 1
    | Arrow -> 2
    | Tuple -> 3
    | Other -> 4

  let compare t1 t2 =
    match t1, t2 with
    | Var, Var
    | Arrow, Arrow
    | Tuple, Tuple
    | Other, Other -> 0
    | Constr lid1, Constr lid2 -> LongIdent.compare lid1 lid2
    | _ -> CCInt.compare (to_int t1) (to_int t2)

  let equal t1 t2 = compare t1 t2 = 0
  let hash = CCHash.poly

  module Map = CCMap.Make (struct
    type nonrec t = t
    let compare = compare
  end)

  module HMap = CCHashtbl.Make (struct
    type nonrec t = t
    let equal = equal
    let hash = hash
  end)

  module MSet = CCMultiSet.Make (struct
    type nonrec t = t
    let compare = compare
  end)

end

(* Base *)

module rec Base : sig

  type t =
    | Var of Variable.t
    | Constr of LongIdent.t * t Array.t
    | Arrow of MSet.t * t
    | Tuple of MSet.t
    | Other of Int.t

  val kind : t -> Kind.t
  val kind' : t -> Kind'.t

  val compare : t -> t -> Int.t
  val equal : t -> t -> Bool.t
  val hash : t -> Int.t

end = struct

  type t =
    | Var of Variable.t
    | Constr of LongIdent.t * t Array.t
    | Arrow of MSet.t * t
    | Tuple of MSet.t
    | Other of Int.t

  let kind : t -> Kind.t = function
    | Var _ -> Var
    | Constr _ -> Constr
    | Arrow _ -> Arrow
    | Tuple _ -> Tuple
    | Other _ -> Other

  let kind' : t -> Kind'.t = function
    | Var _ -> Var
    | Constr (lid, _) -> Constr lid
    | Arrow _ -> Arrow
    | Tuple _ -> Tuple
    | Other _ -> Other

  let to_int t =
    Kind.to_int @@ kind t

  let rec compare t1 t2 =
    if t1 == t2 then 0
    else
      match t1, t2 with
      | Var t1, Var t2 ->
          Variable.compare t1 t2
      | Constr (lid1, params1), Constr (lid2, params2) ->
          CCOrd.(LongIdent.compare lid1 lid2
            <?> (CCArray.compare compare, params1, params2))
      | Arrow (param1, ret1), Arrow (param2, ret2) ->
          let cmp = compare in
          CCOrd.(cmp ret1 ret2
            <?> (MSet.compare, param1, param2))
      | Tuple t1, Tuple t2 ->
          MSet.compare t1 t2
      | Other t1, Other t2 ->
          CCInt.compare t1 t2
      | _ ->
          CCInt.compare (to_int t1) (to_int t2)

  let equal t1 t2 =
    compare t1 t2 = 0

  let hash = Hashtbl.hash

end

(* MSet *)

and MSet : sig

  type elt = Base.t
  type t

  val compare : t -> t -> Int.t

  val of_list : elt List.t -> t
  val of_iter : elt Iter.t -> t
  val to_iter : t -> elt Iter.t
  val as_array : t -> elt Array.t

  val empty : t
  val is_empty : t -> Bool.t
  val length : t -> Int.t
  val singleton : elt -> t
  val is_singleton : t -> elt Option.t
  val union : t -> t -> t
  val add : elt -> t -> t
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val map : (elt -> elt) -> t -> t

  val pp : elt Fmt.t -> t Fmt.t

end = struct

  type elt = Base.t
  type t = elt Array.t

  let compare = CCArray.compare Base.compare
  let sort = CCArray.sort Base.compare

  let of_list lst =
    let t = CCArray.of_list lst in
    sort t ;
    t

  let of_iter it =
    let t = Iter.to_array it in
    sort t ;
    t

  let to_iter = Iter.of_array

  let as_array = CCFun.id

  let empty = [||]
  let is_empty t = t = [||]

  let length = CCArray.length

  let singleton elt = [| elt |]
  let is_singleton = function
    | [|elt|] -> Some elt
    | _ -> None

  let union t1 t2 =
    let t = CCArray.append t1 t2 in
    sort t ;
    t

  let add elt t =
    union (singleton elt) t

  let fold fn t acc =
    CCArray.fold_left (CCFun.flip fn) acc t

  let map fn t =
    of_iter @@ Iter.map fn @@ to_iter t

  let pp pp_ty fmt = function
    | [||] ->
        Fmt.string fmt "()"
    | [| elt |] ->
        pp_ty fmt elt
    | t ->
        Fmt.pf fmt "@[<2>%a@]"
          Fmt.(array ~sep:(any " *@ ") pp_ty) t

end

include Base

module HMap = CCHashtbl.Make (Base)
module Map = CCMap.Make (Base)
module Set = CCSet.Make (Base)

(* Hashcons *)

module Hashcons = struct

  type elt = t
  type t = elt HMap.t

  let make () = HMap.create 17

  let hashcons t ty =
    match HMap.find_opt t ty with
    | Some ty -> ty
    | None -> HMap.add t ty ty ; ty

end

(* Env *)

module Env = struct

  type t = {
    var_gen : Variable.Gen.t ;
    hcons : Hashcons.t ;
  }

  let make dir = {
    var_gen = Variable.Gen.make dir ;
    hcons = Hashcons.make () ;
  }
  let from_hashcons dir hcons = {
    var_gen = Variable.Gen.make dir ;
    hcons ;
  }

end

(* smart constructors *)

let var v = Var v

let constr lid = function
  | [||] when lid = LongIdent.unit -> Tuple MSet.empty
  | params -> Constr (lid, params)

let arrow param ret =
  match param, ret with
  | Tuple elts, Arrow (params, ret) ->
      Arrow (MSet.union elts params, ret)
  | _, Arrow (params, ret) ->
      Arrow (MSet.add param params, ret)
  | Tuple elts, _ ->
      Arrow (elts, ret)
  | _, _ ->
      Arrow (MSet.singleton param, ret)

let tuple elts =
  let aux = function
    | Tuple elts -> elts
    | elt -> MSet.singleton elt
  in
  let elts = MSet.fold (fun elt -> MSet.union @@ aux elt) elts MSet.empty in
  match MSet.is_singleton elts with
  | Some elt -> elt
  | None -> Tuple elts

let other x =
  Other (CCHash.poly x)

(* importation functions *)

let of_outcometree of_outcometree var (out_ty : Outcometree.out_type) =
  match out_ty with
  | Otyp_var (_, name) ->
      var @@ Some name
  | Otyp_constr (id, params) ->
      params
      |> Iter.of_list
      |> Iter.map of_outcometree
      |> Iter.to_array
      |> constr @@ LongIdent.of_outcometree id
  | Otyp_arrow (_, param, ret) ->
      arrow (of_outcometree param) (of_outcometree ret)
  | Otyp_tuple elts ->
      elts
      |> Iter.of_list
      |> Iter.map of_outcometree
      |> MSet.of_iter
      |> tuple
  | Otyp_alias (out_ty, _) ->
      of_outcometree out_ty
  (* not handled *)
  | Otyp_object _
  | Otyp_class _
  | Otyp_variant _
  | Otyp_module _
  | Otyp_attribute _
  (* not simple types *)
  | Otyp_stuff _
  | Otyp_poly _
  | Otyp_abstract
  | Otyp_open
  | Otyp_manifest _
  | Otyp_record _
  | Otyp_sum _ ->
      other out_ty

let of_parsetree of_parsetree var (parse_ty : Parsetree.core_type) =
  match parse_ty.ptyp_desc with
  | Ptyp_any ->
      var None
  | Ptyp_var name ->
      var @@ Some name
  | Ptyp_constr (id, params) ->
      params
      |> Iter.of_list
      |> Iter.map of_parsetree
      |> Iter.to_array
      |> constr id.txt
  | Ptyp_arrow (_, param, ret) ->
      arrow (of_parsetree param) (of_parsetree ret)
  | Ptyp_tuple elts ->
      elts
      |> Iter.of_list
      |> Iter.map of_parsetree
      |> MSet.of_iter
      |> tuple
  | Ptyp_alias (parse_ty, _)
  | Ptyp_poly (_, parse_ty) ->
      of_parsetree parse_ty
  (* not handled *)
  | Ptyp_object _
  | Ptyp_class _
  | Ptyp_variant _
  | Ptyp_package _
  | Ptyp_extension _ ->
      other parse_ty

let var' vars (env : Env.t) = function
  | None ->
      var @@ Variable.Gen.gen env.var_gen
  | Some name ->
      match StringHMap.get vars name with
      | Some t -> t
      | None ->
          let v = Variable.Gen.gen env.var_gen in
          let t = var v in
          StringHMap.add vars name t ;
          t

let of_outcometree, of_parsetree =
  let wrap fn (env : Env.t) x =
    let vars = StringHMap.create 17 in
    let var = var' vars env in
    let rec fn' x =
      x
      |> fn fn' var
      |> Hashcons.hashcons env.hcons
    in
    fn' x
  in
  wrap of_outcometree,
  wrap of_parsetree

let of_lexing env lexbuf =
  lexbuf
  |> Parse.core_type
  |> of_parsetree env

let of_string env str =
  str
  |> Lexing.from_string
  |> of_lexing env

(* utility functions *)

let head = function
  | Arrow (_, ret) -> ret
  | t -> t

let tail = function
  | Arrow (params, _) -> params
  | _ -> MSet.empty

let rec substitute subst =
  let substitute t = substitute subst t in
  let substitute_set = MSet.map substitute in
  let substitute_array = CCArray.map substitute in
  fun t ->
    match t with
    | Var var ->
        begin match Variable.Map.find_opt var subst with
          | None -> t
          | Some t -> t
        end
    | Constr (lid, params) ->
        constr lid @@ substitute_array params
    | Arrow (params, ret) ->
        arrow (tuple @@ substitute_set params) (substitute ret)
    | Tuple elts ->
        tuple @@ substitute_set elts
    | Other _ ->
        t

let iter =
  let iter_subs = function
    | Constr (_, params) -> Iter.of_array params
    | Arrow (params, ret) -> Iter.snoc (MSet.to_iter params) ret
    | Tuple elts -> MSet.to_iter elts
    | _ -> Iter.empty
  in
  fun t k ->
    let rec aux t =
      k t ;
      iter_subs t aux
    in
    aux t

let rec iter_vars t k =
  match t with
  | Other _ ->
      ()
  | Var var ->
      k var
  | Constr (_, params) ->
      CCArray.iter (fun t -> iter_vars t k) params
  | Arrow (params, ret) ->
      Iter.flat_map iter_vars (MSet.to_iter params) k ;
      iter_vars ret k
  | Tuple elts ->
      Iter.flat_map iter_vars (MSet.to_iter elts) k

(* pretty printing *)

let rec pp fmt = function
  | Var var ->
      Variable.pp fmt var
  | Constr (lid, [||]) ->
      LongIdent.pp fmt lid
  | Constr (lid, params) ->
      Fmt.pf fmt "%a@ %a"
        pp_array params
        LongIdent.pp lid
  | Arrow (params, ret) ->
      Fmt.pf fmt "@[<2>%a@ ->@ %a@]"
        (MSet.pp pp_parens) params
        pp_parens ret
  | Tuple elts ->
      Fmt.pf fmt "@[<2>%a@]"
        (MSet.pp pp_parens) elts
  | Other i ->
      Fmt.pf fmt "other%i" i

and pp_parens fmt ty =
  match ty with
  | Var _ | Other _ | Constr _ ->
      pp fmt ty
  | Arrow _ ->
      Fmt.parens pp fmt ty
  | Tuple elts ->
      if MSet.length elts <= 1 then
        pp fmt ty
      else
        Fmt.parens pp fmt ty

and pp_array fmt = function
  | [||] ->
      Fmt.string fmt "()"
  | [| elt |] ->
      pp fmt elt
  | arr ->
      Fmt.pf fmt "@[<2>(%a)@]"
        Fmt.(array ~sep:(any ", ") pp) arr
