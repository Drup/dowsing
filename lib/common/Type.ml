module Longident = struct

  type t = Longident.t

  open Longident

  let compare = compare
  let equal = (=)

  let unit = Lident "unit"

  let of_list strs =
    if strs = [] then
      invalid_arg "Type.Longident.of_list"
    else
      let root = Lident (CCList.hd strs) in
      let strs = CCList.tl strs in
      CCList.fold_left (fun id str -> Ldot (id, str)) root strs

  let rec of_outcometree =
    let open Outcometree in
    function
      | Oide_apply (oid1, oid2) ->
          Lapply (of_outcometree oid1, of_outcometree oid2)
      | Oide_dot (oid, str) ->
          Ldot (of_outcometree oid, str)
      | Oide_ident { printed_name = str } ->
          Lident str

  let rec to_iter id k =
    match id with
    | Lident str ->
        k str
    | Ldot (id, str) ->
        to_iter id k ;
        k str
    | Lapply (id1, id2) ->
        to_iter id1 k ;
        to_iter id2 k

  module Map = CCTrie.Make (struct
    type nonrec t = t
    type char_ = String.t
    let compare = CCString.compare
    let to_iter = to_iter
    let of_list = of_list
  end)

  module HMap = CCHashtbl.Make (struct
    type nonrec t = t
    let equal = equal
    let hash = CCHash.poly
  end)

  let pp fmt id = Pprintast.longident fmt id

end

(******************** Base ********************)

module rec Base : sig

  type t =
    | Var of Variable.t
    | Constr of Longident.t * t Array.t
    | Arrow of Set.t * t
    | Tuple of Set.t
    | Other of Int.t

  val compare : t -> t -> Int.t
  val equal : t -> t -> Bool.t

end = struct

  type t =
    | Var of Variable.t
    | Constr of Longident.t * t Array.t
    | Arrow of Set.t * t
    | Tuple of Set.t
    | Other of Int.t

  let to_int = function
    | Var _ -> 0
    | Constr _ -> 1
    | Arrow _ -> 2
    | Tuple _ -> 3
    | Other _ -> 4

  let rec compare lhs rhs =
      if lhs == rhs then 0
      else
        let (<?>) = CCOrd.(<?>) in
        match lhs, rhs with
        | Var lhs, Var rhs ->
            Variable.compare lhs rhs
        | Constr (id1, args1), Constr (id2, args2) ->
            Longident.compare id1 id2
            <?> (CCArray.compare compare, args1, args2)
        | Arrow (arg1, ret1), Arrow (arg2, ret2) ->
            compare ret1 ret2
            <?> (Set.compare, arg1, arg2)
        | Tuple lhs, Tuple rhs ->
            Set.compare lhs rhs
        | Other lhs, Other rhs ->
            CCInt.compare lhs rhs
        | _ ->
            CCInt.compare (to_int lhs) (to_int rhs)

  let equal lhs rhs =
    compare lhs rhs = 0

end

(******************** Set ********************)

and Set : sig

  type elt = Base.t
  type t

  val compare : t -> t -> Int.t

  val empty : t
  val singleton : elt -> t
  val union : t -> t -> t
  val add : elt -> t -> t
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val map : (elt -> elt) -> t -> t

  val is_empty : t -> bool

  val of_list : elt List.t -> t
  val of_iter : elt Iter.t -> t
  val to_iter : t -> elt Iter.t
  val as_array : t -> elt Array.t

  val pp : elt Fmt.t -> t Fmt.t

end = struct

  type elt = Base.t
  type t = elt Array.t

  let compare = CCArray.compare Base.compare
  let sort = CCArray.sort Base.compare

  let empty = [||]
  let is_empty t = t = [||]

  let singleton elt = [| elt |]

  let union lhs rhs =
    let arr = CCArray.append lhs rhs in
    sort arr ;
    arr

  let add elt set =
    union (singleton elt) set

  let fold fn set acc =
    CCArray.fold_left (CCFun.flip fn) acc set

  let of_list lst =
    let arr = CCArray.of_list lst in
    sort arr ;
    arr

  let of_iter it =
    let arr = Iter.to_array it in
    sort arr ;
    arr

  let to_iter = Iter.of_array

  let map f t = of_iter @@ Iter.map f @@ to_iter t

  let as_array = Fun.id

  let pp pp_elt fmt = function
    | [||] ->
        Fmt.string fmt "()"
    | [| elt |] ->
        pp_elt fmt elt
    | arr ->
        Fmt.pf fmt "(@ %a@ )"
          Fmt.(array ~sep:(any ", ") pp_elt) arr

end

include Base

(******************** Hashcons ********************)

module Hashcons = struct

  type elt = {
    node : t ;
    mutable tag : Int.t ;
  }

  module Set = Weak.Make (struct
    type t = elt
    let equal lhs rhs = compare lhs.node rhs.node = 0
    let hash self = CCHash.poly self.node
  end)

  type t = {
    tbl : Set.t ;
    mutable elt_cnt : Int.t ;
  }

  let make sz = {
    tbl = Set.create sz ;
    elt_cnt = 0 ;
  }

  let hashcons self ty =
    let elt = { node = ty ; tag = -1 } in
    let elt' = Set.merge self.tbl elt in
    if elt == elt' then begin
      elt.tag <- self.elt_cnt ;
      self.elt_cnt <- self.elt_cnt + 1
    end ;
    elt'.node

end

(******************** Env ********************)

module Env = struct

  type t = {
    var_gen : Variable.Gen.t ;
    var_names : String.t Variable.HMap.t ;
    hcons : Hashcons.t ;
  }

  let make () = {
    var_gen = Variable.Gen.make 10 ;
    var_names = Variable.HMap.create 17 ;
    hcons = Hashcons.make 17 ;
  }

end

(******************** importation functions ********************)

let make_var (env : Env.t) =
  let module StringHMap = CCHashtbl.Make (CCString) in
  let vars = StringHMap.create 17 in
  function
    | None ->
        Var (Variable.Gen.gen env.var_gen)
    | Some name ->
        match StringHMap.get vars name with
        | Some ty -> ty
        | None ->
            let var = Variable.Gen.gen env.var_gen in
            let ty = Var var in
            Variable.HMap.add env.var_names var name ;
            StringHMap.add vars name ty ;
            ty

let make_constr id = function
  | [||] when id = Longident.unit -> Tuple Set.empty
  | args -> Constr (id, args)

let make_arrow arg ret =
  match arg, ret with
  | Tuple tpl, _ when Set.is_empty tpl ->
      ret
  | Tuple tpl, Arrow (args, ret) ->
      Arrow (Set.union tpl args, ret)
  | _, Arrow (args, ret) ->
      Arrow (Set.add arg args, ret)
  | Tuple tpl, _ ->
      Arrow (tpl, ret)
  | _, _ ->
      Arrow (Set.singleton arg, ret)

let make_tuple elts =
  let aux = function
    | Tuple elts -> elts
    | elt -> Set.singleton elt
  in
  Tuple (Set.fold (fun elt -> Set.union @@ aux elt) elts Set.empty)

let make_other x =
  Other (CCHash.poly x)

let rec substitute sub (ty : t) = match ty with
  | Var v ->
    begin match Variable.Map.find_opt v sub with
      | None -> ty
      | Some ty -> ty
    end
  | Constr (lid, ts) ->
    make_constr lid @@ Array.map (substitute sub) ts
  | Arrow (ts, t) ->
    make_arrow (make_tuple @@ Set.map (substitute sub) ts) (substitute sub t)
  | Tuple ts ->
    make_tuple (Set.map (substitute sub) ts)
  | Other _ -> ty

let of_outcometree of_outcometree make_var (out_ty : Outcometree.out_type) =
  match out_ty with
  | Otyp_var (_, name) ->
      make_var @@ Some name
  | Otyp_constr (id, args) ->
      args
      |> Iter.of_list
      |> Iter.map of_outcometree
      |> Iter.to_array
      |> make_constr (Longident.of_outcometree id)
  | Otyp_arrow (_, arg, ret) ->
      make_arrow (of_outcometree arg) (of_outcometree ret)
  | Otyp_tuple elts ->
      elts
      |> Iter.of_list
      |> Iter.map of_outcometree
      |> Set.of_iter
      |> make_tuple
  | Otyp_alias (ty, _) ->
      of_outcometree ty
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
      make_other out_ty

let of_parsetree of_parsetree make_var (parse_ty : Parsetree.core_type) =
  match parse_ty.ptyp_desc with
  | Ptyp_any ->
      make_var None
  | Ptyp_var name ->
      make_var @@ Some name
  | Ptyp_constr (id, args) ->
      args
      |> Iter.of_list
      |> Iter.map of_parsetree
      |> Iter.to_array
      |> make_constr id.txt
  | Ptyp_arrow (_, arg, ret) ->
      make_arrow (of_parsetree arg) (of_parsetree ret)
  | Ptyp_tuple elts ->
      elts
      |> Iter.of_list
      |> Iter.map of_parsetree
      |> Set.of_iter
      |> make_tuple
  | Ptyp_alias (ty, _)
  | Ptyp_poly (_, ty) ->
      of_parsetree ty
  (* not handled *)
  | Ptyp_object _
  | Ptyp_class _
  | Ptyp_variant _
  | Ptyp_package _
  | Ptyp_extension _ ->
      make_other parse_ty

let wrap fn (env : Env.t) =
  let rec fn' x =
    x
    |> fn fn' (make_var env)
    |> Hashcons.hashcons env.hcons
  in
  fn'

let of_outcometree = wrap of_outcometree
let of_parsetree = wrap of_parsetree

let of_lexing env lexbuf =
  lexbuf
  |> Parse.core_type
  |> of_parsetree env

let of_string env str =
  str
  |> Lexing.from_string
  |> of_lexing env

(******************** utility functions ********************)

let rec vars ty k =
  match ty with
  | Other _ ->
      ()
  | Var var ->
      k var
  | Constr (_, arr) ->
      CCArray.iter (fun ty -> vars ty k) arr
  | Tuple set ->
      Iter.flat_map vars (Set.to_iter set) k
  | Arrow (set, ret) ->
      Iter.flat_map vars (Set.to_iter set) k ;
      vars ret k

let head = function
  | Arrow (_, ret) -> ret
  | ty -> ty

let pp_array pp_elt fmt = function
  | [||] ->
      Fmt.string fmt "()"
  | [| elt |] ->
      pp_elt fmt elt
  | arr ->
      Fmt.pf fmt "@[<2>(%a)@]"
        Fmt.(array ~sep:(any ", ") pp_elt) arr

let rec pp var_names =
  let pp fmt = pp var_names fmt in
  fun fmt -> function
    | Var var ->
        Variable.pp var_names fmt var
    | Constr (id, [||]) ->
        Longident.pp fmt id
    | Constr (id, args) ->
        Fmt.pf fmt "%a@ %a"
          (pp_array pp) args
          Longident.pp id
    | Arrow (args, ret) ->
        Fmt.pf fmt "@[<2>%a@ ->@ %a@]"
          (Set.pp pp) args
          pp ret
    | Tuple elts ->
        Fmt.pf fmt "@[<2>%a@]"
          (Set.pp pp) elts
    | Other _ ->
        Fmt.string fmt "_"
