(* Kind *)

module Kind = struct
  type t = Var | Constr | Arrow | Tuple | Other

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

  let pp = Fmt.of_to_string to_string
end

(* Kind' *)

module Kind' = struct
  type t = Var | Constr of LongIdent.t | Arrow | Tuple | Other

  let to_int = function
    | Var -> 0
    | Constr _ -> 1
    | Arrow -> 2
    | Tuple -> 3
    | Other -> 4

  let to_string = function
    | Var -> "variable"
    | Constr _ -> "constructor"
    | Arrow -> "arrow"
    | Tuple -> "tuple"
    | Other -> "other"

  let compare t1 t2 =
    match (t1, t2) with
    | Var, Var | Arrow, Arrow | Tuple, Tuple | Other, Other -> 0
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
    | FrozenVar of Variable.t
    | Constr of LongIdent.t * t Array.t
    (** Represents the types of the form [(a₁,...,aₙ) p] where [p] is a [Longident.t] *)
    | Arrow of NSet.t * t
    (** Represents the types of the form [(a₁,...,aₙ) -> r] *)
    | Tuple of NSet.t
    (** Represents tuples [(a₁*...*aₙ)] *)
    | Other of Int.t

  val kind : t -> Kind.t
  val kind' : t -> Kind'.t
  val compare : t CCOrd.t
  val equal : t -> t -> Bool.t
  val hash : t CCHash.t
end = struct
  type t =
    | Var of Variable.t
    | FrozenVar of Variable.t
    | Constr of LongIdent.t * t Array.t
    | Arrow of NSet.t * t
    | Tuple of NSet.t
    | Other of Int.t

  let kind : t -> Kind.t = function
    | Var _ | FrozenVar _ -> Var
    | Constr _ -> Constr
    | Arrow _ -> Arrow
    | Tuple _ -> Tuple
    | Other _ -> Other

  let kind' : t -> Kind'.t = function
    | Var _ | FrozenVar _ -> Var
    | Constr (lid, _) -> Constr lid
    | Arrow _ -> Arrow
    | Tuple _ -> Tuple
    | Other _ -> Other

  let rec compare t1 t2 =
    if t1 == t2 then 0
    else
      let open CCOrd.Infix in
      match (t1, t2) with
      | Var var1, Var var2 -> Variable.compare var1 var2
      | FrozenVar var1, FrozenVar var2 -> Variable.compare var1 var2
      | Constr (lid1, params1), Constr (lid2, params2) ->
          LongIdent.compare lid1 lid2
          <?> (CCArray.compare compare, params1, params2)
      | Arrow (param1, ret1), Arrow (param2, ret2) ->
          let cmp = compare in
          cmp ret1 ret2 <?> (NSet.compare, param1, param2)
      | Tuple elts1, Tuple elts2 -> NSet.compare elts1 elts2
      | Other i1, Other i2 -> CCInt.compare i1 i2
      | _ -> CCOrd.poly t1 t2

  let equal t1 t2 = compare t1 t2 = 0
  let hash = Hashtbl.hash
end

(* NSet *)
and NSet : sig
  type elt = Base.t
  type t

  val compare : t CCOrd.t
  val of_list : elt List.t -> t
  val of_array : elt Array.t -> t
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
    sort t;
    t

  let of_array a =
    (* TODO: Do we really need the copy? *)
    let t = CCArray.copy a in
    sort t;
    t

  let of_iter it =
    let t = Iter.to_array it in
    sort t;
    t

  let to_iter = Iter.of_array
  let as_array = CCFun.id
  let empty = [||]
  let is_empty t = t = [||]
  let length = CCArray.length
  let singleton elt = [| elt |]
  let is_singleton = function [| elt |] -> Some elt | _ -> None

  let union t1 t2 =
    let t = CCArray.append t1 t2 in
    sort t;
    t

  let add elt t = union (singleton elt) t
  let fold fn t acc = CCArray.fold_left (CCFun.flip fn) acc t
  let map fn t = of_iter @@ Iter.map fn @@ to_iter t

  let pp pp_ty ppf = function
    | [||] -> Fmt.string ppf "unit"
    | [| elt |] -> pp_ty ppf elt
    | t -> Fmt.pf ppf "@[<2>%a@]" Fmt.(array ~sep:(any " *@ ") pp_ty) t
end

include Base
module HMap = CCHashtbl.Make (Base)
module Map = CCMap.Make (Base)
module Set = CCSet.Make (Base)
module MSet = CCMultiSet.Make (Base)

(* Hashcons *)

module Hashcons = struct
  type elt = t
  type t = elt HMap.t

  let make () = HMap.create 17

  let hashcons t ty =
    match HMap.find_opt t ty with
    | Some ty -> ty
    | None ->
        HMap.add t ty ty;
        ty
end

(* Env *)

module Env = struct
  type t = { var_gen : Variable.Gen.t; hcons : Hashcons.t }

  let make ?(hcons = Hashcons.make ()) () =
    { var_gen = Variable.Gen.make (); hcons }
  let restart env = {env with var_gen = Variable.Gen.make () }
end

let hashcons env ty = Hashcons.hashcons env.Env.hcons ty

(* smart constructors *)

let dummy = Constr (Longident.Lident "__dummy", [||])
let var env v = hashcons env @@ Var v
let frozen_var env v = hashcons env @@ FrozenVar v

let constr env lid params =
  hashcons env
  @@
  match params with
  | [||] when lid = LongIdent.unit -> Tuple NSet.empty
  | params -> Constr (lid, params)

let arrows env params ret =
  hashcons env
  @@
  match ret with
  | Arrow (params', ret) -> Arrow (NSet.union params params', ret)
  | _ -> Arrow (params, ret)

let arrow env param ret =
  hashcons env
  @@
  match (param, ret) with
  | Tuple elts, Arrow (params, ret) -> Arrow (NSet.union elts params, ret)
  | _, Arrow (params, ret) -> Arrow (NSet.add param params, ret)
  | Tuple elts, _ -> Arrow (elts, ret)
  | _, _ -> Arrow (NSet.singleton param, ret)

let tuple env elts =
  (* TODO: can elts be of size 0 or 1? Currently it is needed *)
  let aux = function Tuple elts -> elts | elt -> NSet.singleton elt in
  let elts = NSet.fold (fun elt -> NSet.union @@ aux elt) elts NSet.empty in
  hashcons env
  @@ match NSet.is_singleton elts with Some elt -> elt | None -> Tuple elts

let other env x = hashcons env @@ Other (CCHash.poly x)

(** Utility functions *)

let rec freeze_variables env (t : t) =
  match t with
  | Var v -> frozen_var env v
  | Constr (lid, t) -> constr env lid (Array.map (freeze_variables env) t)
  | Arrow (args, t) ->
      arrows env (NSet.map (freeze_variables env) args) (freeze_variables env t)
  | Tuple ts -> tuple env (NSet.map (freeze_variables env) ts)
  | Other _ | FrozenVar _ -> hashcons env t

let rec refresh_variables bdgs env (t : t) =
  match t with
  | Var v ->
    let v' =
      Variable.HMap.get_or_add bdgs ~f:(fun _ -> Variable.Gen.gen env.Env.var_gen) ~k:v
    in
    var env v'
  | Constr (lid, t) -> constr env lid (Array.map (refresh_variables bdgs env) t)
  | Arrow (args, t) ->
    arrows env (NSet.map (refresh_variables bdgs env) args) (refresh_variables bdgs env t)
  | Tuple ts -> tuple env (NSet.map (refresh_variables bdgs env) ts)
  | Other _ | FrozenVar _ -> hashcons env t
let refresh_variables env t =
  refresh_variables (Variable.HMap.create 17) env t

let is_arrow = function
  | Arrow _ -> true
  | _ -> false

(** import functions *)

let of_outcometree of_outcometree env var (out_ty : Outcometree.out_type) =
  match out_ty with
  | Otyp_var (_, name) -> var @@ Some name
  | Otyp_constr (id, params) ->
      params |> Iter.of_list |> Iter.map of_outcometree |> Iter.to_array
      |> constr env @@ LongIdent.of_outcometree id
  | Otyp_arrow (_, param, ret) ->
      arrow env (of_outcometree param) (of_outcometree ret)
  | Otyp_tuple elts ->
      elts |> Iter.of_list |> Iter.map of_outcometree |> NSet.of_iter
      |> tuple env
  | Otyp_alias { aliased ; _ } -> of_outcometree aliased
  (* not handled *)
  | Otyp_object _ | Otyp_class _ | Otyp_variant _ | Otyp_module _
  | Otyp_attribute _
  (* not simple types *)
  | Otyp_stuff _ | Otyp_poly _ | Otyp_abstract | Otyp_open | Otyp_manifest _
  | Otyp_record _ | Otyp_sum _ ->
      other env out_ty

let of_parsetree of_parsetree env var (parse_ty : Parsetree.core_type) =
  match parse_ty.ptyp_desc with
  | Ptyp_any -> var None
  | Ptyp_var name -> var @@ Some name
  | Ptyp_constr (id, params) ->
      params |> Iter.of_list |> Iter.map of_parsetree |> Iter.to_array
      |> constr env id.txt
  | Ptyp_arrow (_, param, ret) ->
      arrow env (of_parsetree param) (of_parsetree ret)
  | Ptyp_tuple elts ->
      elts |> Iter.of_list |> Iter.map of_parsetree |> NSet.of_iter |> tuple env
  | Ptyp_alias (parse_ty, _) | Ptyp_poly (_, parse_ty) -> of_parsetree parse_ty
  (* Ignore open in types *)
  | Ptyp_open (_modpath, typ) ->
    of_parsetree typ
  (* not handled *)
  | Ptyp_object _ | Ptyp_class _ | Ptyp_variant _ | Ptyp_package _
  | Ptyp_extension _ ->
      other env parse_ty

let rec parse_to_outcome (parse_ty : Parsetree.core_type) =
  match parse_ty.ptyp_desc with
  | Ptyp_var name -> Outcometree.Otyp_var (false, name)
  | Ptyp_constr (id, params) ->
      let rec make_out_id lid =
        match lid with
        | Longident.Lident s ->
            let out_name = { Outcometree.printed_name = s } in
            Outcometree.Oide_ident out_name
        | LongIdent.Ldot (id, s) -> Outcometree.Oide_dot (make_out_id id, s)
        | LongIdent.Lapply (id1, id2) ->
            Outcometree.Oide_apply (make_out_id id1, make_out_id id2)
      in
      let id = make_out_id id.txt in
      let params = params |> CCList.map parse_to_outcome in
      Outcometree.Otyp_constr (id, params)
  | Ptyp_arrow (lbl, param, ret) ->
      Outcometree.Otyp_arrow (lbl, parse_to_outcome param, parse_to_outcome ret)
  | Ptyp_tuple elts -> Outcometree.Otyp_tuple (CCList.map parse_to_outcome elts)
  | Ptyp_alias (parse_ty, alias) ->
      let aliased = parse_to_outcome parse_ty in
      Outcometree.Otyp_alias {non_gen = false; aliased; alias=alias.txt}
  | Ptyp_poly (s, parse_ty) ->
      let f (str : string Location.loc) = str.txt in
      let str = CCList.map f s in
      Outcometree.Otyp_poly (str, parse_to_outcome parse_ty)
  | Ptyp_open (_modpath, typ) ->
    parse_to_outcome typ
  | Ptyp_object _ | Ptyp_class _ | Ptyp_variant _ | Ptyp_package _
  | Ptyp_extension _ | Ptyp_any ->
      Outcometree.Otyp_abstract

let outcome_of_string (str : String.t) =
  let (parse_ty : Parsetree.core_type) =
    try Parse.core_type @@ Lexing.from_string str
    with Syntaxerr.Error _ -> invalid_arg "Type.outcome_of_string"
  in
  parse_to_outcome parse_ty

let generate_var bdgs (env : Env.t) = function
  | None -> var env @@ Variable.Gen.gen env.var_gen
  | Some name -> (
      match String.HMap.get bdgs name with
      | Some v -> var env v
      | None ->
          let v = Variable.Gen.gen env.var_gen in
          String.HMap.add bdgs name v;
          var env v)
    
let of_outcometree', of_parsetree' =
  let wrap fn (env : Env.t) x =
    let env = Env.restart env in
    let bdgs = String.HMap.create 17 in
    let var = generate_var bdgs env in
    let rec fn' x = fn fn' env var x in
    (bdgs, fn' x)
  in
  (wrap of_outcometree, wrap of_parsetree)

let of_outcometree, of_parsetree =
  let wrap fn env x = snd @@ fn env x in
  (wrap of_outcometree', wrap of_parsetree')

let of_lexing, of_lexing' =
  let wrap of_parsetree env lexbuf =
    let parse_ty =
      try Parse.core_type lexbuf
      with Syntaxerr.Error _ -> invalid_arg "Type.of_lexing"
    in
    of_parsetree env parse_ty
  in
  (wrap of_parsetree, wrap of_parsetree')

let of_string, of_string' =
  let wrap of_lexing env str = str |> Lexing.from_string |> of_lexing env in
  (wrap of_lexing, wrap of_lexing')

(* utility functions *)

let head = function Arrow (_, ret) -> ret | t -> t
let tail = function Arrow (params, _) -> params | _ -> NSet.empty

let iter =
  let iter_subs = function
    | Constr (_, params) -> Iter.of_array params
    | Arrow (params, ret) -> Iter.snoc (NSet.to_iter params) ret
    | Tuple elts -> NSet.to_iter elts
    | _ -> Iter.empty
  in
  fun t k ->
    let rec aux t =
      k t;
      iter_subs t aux
    in
    aux t

let rec iter_vars t k =
  match t with
  | Other _ | FrozenVar _ -> ()
  | Var var -> k var
  | Constr (_, params) -> CCArray.iter (fun t -> iter_vars t k) params
  | Arrow (params, ret) ->
      Iter.flat_map iter_vars (NSet.to_iter params) k;
      iter_vars ret k
  | Tuple elts -> Iter.flat_map iter_vars (NSet.to_iter elts) k

let rec iter_consts t f =
  match t with
  | Other _ | FrozenVar _ | Var _ -> ()
  | Constr (lid, params) ->
      f lid;
      CCArray.iter (fun t -> iter_consts t f) params
  | Arrow (params, ret) ->
      Iter.flat_map iter_consts (NSet.to_iter params) f;
      iter_consts ret f
  | Tuple elts -> Iter.flat_map iter_consts (NSet.to_iter elts) f

(* pretty printing *)

let rec pp ppf = function
  | Var var -> Fmt.pf ppf "'%a" Variable.pp var
  | FrozenVar var -> Fmt.pf ppf "^%a" Variable.pp var
  | Constr (lid, [||]) -> LongIdent.pp ppf lid
  | Constr (lid, params) -> Fmt.pf ppf "%a@ %a" pp_array params LongIdent.pp lid
  | Arrow (params, ret) ->
      Fmt.pf ppf "@[<2>(%a@ ->@ %a)@]" (NSet.pp pp_parens) params pp_parens ret
  | Tuple elts -> Fmt.pf ppf "@[<2>%a@]" (NSet.pp pp_parens) elts
  | Other i -> Fmt.pf ppf "other%i" i

and pp_parens ppf ty =
  match ty with
  | Var _ | FrozenVar _ | Other _ | Constr _ -> pp ppf ty
  | Arrow _ -> Fmt.parens pp ppf ty
  | Tuple elts ->
      if NSet.length elts <= 1 then pp ppf ty else Fmt.parens pp ppf ty

and pp_array ppf = function
  | [||] -> Fmt.string ppf "()"
  | [| elt |] -> pp ppf elt
  | arr -> Fmt.pf ppf "@[<2>(%a)@]" Fmt.(array ~sep:(any ", ") pp) arr
