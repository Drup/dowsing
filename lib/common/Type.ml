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
      let (<?>) = CCOrd.(<?>) in
      match t1, t2 with
      | Var t1, Var t2 ->
          Variable.compare t1 t2
      | Constr (lid1, args1), Constr (lid2, args2) ->
          LongIdent.compare lid1 lid2
          <?> (CCArray.compare compare, args1, args2)
      | Arrow (arg1, ret1), Arrow (arg2, ret2) ->
          compare ret1 ret2
          <?> (MSet.compare, arg1, arg2)
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
  val union : t -> t -> t
  val add : elt -> t -> t
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val map : (elt -> elt) -> t -> t
  val min_elt : t -> elt

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

  let min_elt t = t.(0)

  let pp pp_ty fmt = function
    | [||] ->
      Fmt.string fmt "()"
    | [| elt |] ->
      pp_ty fmt elt
    | arr ->
      Fmt.pf fmt "@[<2>%a@]"
        Fmt.(array ~sep:(any " *@ ") pp_ty) arr
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
    var_names : String.t Variable.HMap.t ;
    hcons : Hashcons.t ;
  }

  let make () = {
    var_gen = Variable.Gen.make 10 ;
    var_names = Variable.HMap.create 17 ;
    hcons = Hashcons.make () ;
  }

end

(* importation functions *)

let var v = Var v

let constr lid = function
  | [||] when lid = LongIdent.unit -> Tuple MSet.empty
  | args -> Constr (lid, args)

let arrow arg ret =
  match arg, ret with
  | Tuple tpl, _ when MSet.is_empty tpl ->
      ret
  | Tuple tpl, Arrow (args, ret) ->
      Arrow (MSet.union tpl args, ret)
  | _, Arrow (args, ret) ->
      Arrow (MSet.add arg args, ret)
  | Tuple tpl, _ ->
      Arrow (tpl, ret)
  | _, _ ->
      Arrow (MSet.singleton arg, ret)

let tuple elts =
  let aux = function
    | Tuple elts -> elts
    | elt -> MSet.singleton elt
  in
  let elts = MSet.fold (fun elt -> MSet.union @@ aux elt) elts MSet.empty in
  if MSet.length elts = 1 then
    MSet.min_elt elts
  else Tuple elts

let other x =
  Other (CCHash.poly x)

module StringHMap = CCHashtbl.Make (CCString)
let fresh_var vars (env : Env.t) = function
  | None ->
    var (Variable.Gen.gen env.var_gen)
  | Some name ->
    match StringHMap.get vars name with
    | Some t -> t
    | None ->
      let v = Variable.Gen.gen env.var_gen in
      let t = var v in
      Variable.HMap.add env.var_names v name ;
      StringHMap.add vars name t ;
      t

let of_outcometree of_outcometree make_var (out_ty : Outcometree.out_type) =
  match out_ty with
  | Otyp_var (_, name) ->
      make_var @@ Some name
  | Otyp_constr (id, args) ->
      args
      |> Iter.of_list
      |> Iter.map of_outcometree
      |> Iter.to_array
      |> constr (LongIdent.of_outcometree id)
  | Otyp_arrow (_, arg, ret) ->
      arrow (of_outcometree arg) (of_outcometree ret)
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
      |> constr id.txt
  | Ptyp_arrow (_, arg, ret) ->
      arrow (of_parsetree arg) (of_parsetree ret)
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

let wrap fn (env : Env.t) x =
  let vars = StringHMap.create 17 in
  let make_var = fresh_var vars env in
  let rec fn' x =
    x
    |> fn fn' make_var
    |> Hashcons.hashcons env.hcons
  in
  fn' x

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

(* utility functions *)

let head = function
  | Arrow (_, ret) -> ret
  | t -> t

let tail = function
  | Arrow (args, _) -> args
  | _ -> MSet.empty

let rec substitute sub =
  let substitute t = substitute sub t in
  let substitute_set = MSet.map substitute in
  fun t ->
    match t with
    | Var var ->
        begin match Variable.Map.find_opt var sub with
          | None -> t
          | Some t -> t
        end
    | Constr (lid, args) ->
        constr lid @@ CCArray.map substitute args
    | Arrow (args, ret) ->
        arrow (tuple @@ substitute_set args) (substitute ret)
    | Tuple elts ->
        tuple @@ substitute_set elts
    | Other _ ->
        t

let rec vars t k =
  match t with
  | Other _ ->
      ()
  | Var var ->
      k var
  | Constr (_, args) ->
      CCArray.iter (fun t -> vars t k) args
  | Tuple elts ->
      Iter.flat_map vars (MSet.to_iter elts) k
  | Arrow (args, ret) ->
      Iter.flat_map vars (MSet.to_iter args) k ;
      vars ret k

(* pretty printing *)

let rec pp var_names fmt = function
  | Var var ->
    Variable.pp var_names fmt var
  | Constr (lid, [||]) ->
    LongIdent.pp fmt lid
  | Constr (lid, args) ->
    Fmt.pf fmt "%a@ %a"
      (pp_array var_names) args
      LongIdent.pp lid
  | Arrow (args, ret) ->
    Fmt.pf fmt "@[<2>%a@ ->@ %a@]"
      (MSet.pp @@ pp var_names) args
      (pp var_names) ret
  | Tuple elts ->
    Fmt.pf fmt "@[<2>%a@]"
      (MSet.pp @@ pp var_names) elts
  | Other i ->
    Fmt.pf fmt "other%i" i
and pp_array var_names fmt = function
  | [||] ->
    Fmt.string fmt "()"
  | [| elt |] ->
    pp var_names fmt elt
  | arr ->
    Fmt.pf fmt "@[<2>(%a)@]"
      Fmt.(array ~sep:(any ", ") @@ pp var_names) arr
