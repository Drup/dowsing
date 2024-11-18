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

module type Tuple = sig
  type base_t

  module Complet : sig
    type t

    val hash : t -> int
    val unit : t
    val mk : base_t array -> t
    val mk_l : base_t list -> t
    val is_unit : t -> bool
    val size : t -> int
    val compare : t -> t -> int
    val union : t -> t -> t
    val add : t -> base_t -> t
    val singleton : base_t -> t
    val is_singleton : t -> base_t option
    val fold : (base_t -> 'a -> 'a) -> t -> 'a -> 'a
    val to_iter : t -> base_t Iter.t
    val pp : base_t Fmt.t -> t Fmt.t
  end

  module Partial : sig
    type t

    val mk : ?tuple:Complet.t -> unit -> t
    val add : t -> base_t -> unit
    val add_n : t -> base_t -> int -> unit
    val freeze : t -> Complet.t
  end

  include module type of Complet
end


module rec Base : sig
  type t =
    | Var of Variable.t
    | FrozenVar of Variable.t
    | Constr of LongIdent.t * t Array.t
    (** Represents the types of the form [(a₁,...,aₙ) p] where [p] is a [Longident.t] *)
    | Arrow of Tuple.t * t
    (** Represents the types of the form [(a₁,...,aₙ) -> r] *)
    | Tuple of Tuple.t
    (** Represents tuples [(a₁*...*aₙ)] *)
    | Other of Int.t

  val kind : t -> Kind.t
  val kind' : t -> Kind'.t
  val compare : t CCOrd.t
  val equal : t -> t -> Bool.t
  val hash : t CCHash.t
  val is_tuple : t -> bool
end = struct
  type t =
    | Var of Variable.t
    | FrozenVar of Variable.t
    | Constr of LongIdent.t * t Array.t
    | Arrow of Tuple.t * t
    | Tuple of Tuple.t
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
          cmp ret1 ret2 <?> (Tuple.compare, param1, param2)
      | Tuple elts1, Tuple elts2 -> Tuple.compare elts1 elts2
      | Other i1, Other i2 -> CCInt.compare i1 i2
      | _ -> CCOrd.poly t1 t2

  let equal t1 t2 = compare t1 t2 = 0
  let rec hash t =
    match t with
    | Var v -> CCHash.(pair int Variable.hash) (0, v)
    | FrozenVar v -> CCHash.(pair int Variable.hash) (1, v)
    | Constr (lid, params) -> CCHash.(pair LongIdent.hash (array hash)) (lid, params)
    | Arrow (args, ret) -> CCHash.(pair Tuple.hash hash) (args, ret)
    | Tuple elts -> Tuple.hash elts
    | Other i -> CCHash.int i

  let is_tuple = function
    | Tuple ts -> not (Tuple.is_unit ts && (CCOption.is_some @@ Tuple.is_singleton ts))
    | _ -> false
end

and Multiset : Tuple with type base_t = Base.t = struct
  type base_t = Base.t

  module M = CCMultiSet.Make (Base)

  module Complet = struct
    type t = M.t

    let hash m = CCHash.list Base.hash @@ M.to_list m

    let mk a =
      assert (Array.length a >= 2);
      M.of_iter (fun k ->
          CCArray.to_iter a (fun elt ->
              assert (not (Base.is_tuple elt));
              k elt))

    let mk_l l =
      assert (List.length l <> 1);
      M.of_iter (fun k ->
          CCList.to_iter l (fun elt ->
              assert (not (Base.is_tuple elt));
              k elt))

    let size m = M.fold m 0 (fun acc n _ -> acc + n)
    let is_unit = M.is_empty
    let compare = M.compare
    let unit = M.empty
    let union = M.union

    let add m elt =
      assert (not @@ M.is_empty m);
      assert (not (Base.is_tuple elt));
      M.add m elt

    let singleton elt =
      assert (not (Base.is_tuple elt));
      M.singleton elt

    let is_singleton elts =
      let exception Too_many in
      try
        let res = ref None in
        M.iter elts (fun n elt ->
            match n with
            | 0 -> ()
            | 1 ->
                if CCOption.is_some !res then raise Too_many
                else res := Some elt
            | _ -> raise Too_many);
        !res
      with Too_many -> None

    let fold f m x =
      M.fold m x (fun acc n elt ->
          let acc = ref acc in
          for _ = 1 to n do
            acc := f elt !acc
          done;
          !acc)

    let to_iter m f =
      M.iter m (fun n elt ->
          for _ = 1 to n do
            f elt
          done)

    let pp pp_x fmt m =
      if is_unit m then Format.fprintf fmt "unit"
      else
        let pp_sep fmt () = Format.fprintf fmt " * " in
        let pp_start fmt () = Format.fprintf fmt "(" in
        let pp_stop fmt () = Format.fprintf fmt ")" in
        let rec pp_mult ?(first = true) x n =
          if n = 0 then ()
          else (
            if not first then pp_sep fmt ();
            pp_x fmt x;
            pp_mult ~first:false x (n - 1))
        in
        pp_start fmt ();
        let first = ref true in
        M.iter m (fun x n ->
            if !first then first := false else pp_sep fmt ();
            pp_mult n x);
        pp_stop fmt ()
  end

  module Partial = struct
    type t = M.t ref

    let mk ?tuple () = match tuple with None -> ref M.empty | Some m -> ref m

    let add m t =
      assert (not (Base.is_tuple t));
      m := M.add !m t

    let add_n m t n =
      assert (not (Base.is_tuple t));
      m := M.add_mult !m t n

    let freeze m =
      assert (M.cardinal !m <> 1);
      !m
  end

  include Complet
end

and Array_vec : Tuple with type base_t = Base.t = struct
  type base_t = Base.t

  module Complet = struct
    type t = Base.t CCArray.t

    let hash = CCHash.array Base.hash

    let mk a =
      assert (Array.length a >= 2);
      assert (Array.for_all (fun elt -> not @@ Base.is_tuple elt) a);
      let a = Array.copy a in
      Array.fast_sort Base.compare a;
      a

    let mk_l l =
      let a = Array.of_list l in
      assert (Array.length a <> 1);
      assert (Array.for_all (fun elt -> not @@ Base.is_tuple elt) a);
      Array.fast_sort Base.compare a;
      a

    let size a = Array.length a
    let is_unit a = size a = 0
    let compare = CCArray.compare Base.compare
    let unit = [||]

    let union t1 t2 =
      let l1 = Array.length t1 in
      if l1 = 0 then t2
      else
        let l2 = Array.length t2 in
        if l2 = 0 then t1
        else
          let t = Array.make (l1 + l2) (Array.get t1 0) in
          let i1 = ref 0 and i2 = ref 0 in
          while !i1 < l1 && !i2 < l2 do
            let e1 = Array.get t1 !i1 and e2 = Array.get t2 !i2 in
            if Base.compare e1 e2 <= 0 then (
              Array.set t (!i1 + !i2) e1;
              incr i1)
            else (
              Array.set t (!i1 + !i2) e2;
              incr i2)
          done;
          if !i1 < l1 then Array.blit t1 !i1 t (!i1 + l2) (l1 - !i1);
          if !i2 < l2 then Array.blit t2 !i2 t (!i2 + l1) (l2 - !i2);
          t

    let add t elt =
      assert (Array.length t > 0);
      assert (not (Base.is_tuple elt));
      let new_t = Array.make (Array.length t + 1) elt in
      (match CCArray.bsearch ~cmp:Base.compare elt t with
      | `Just_after i | `At i ->
          Array.blit t 0 new_t 0 (i + 1);
          Array.blit t (i + 1) new_t (i + 2) (Array.length t - i - 1)
      | `All_lower -> Array.blit t 0 new_t 0 (Array.length t)
      | `All_bigger -> Array.blit t 0 new_t 1 (Array.length t)
      | `Empty -> ());
      new_t

    let singleton elt =
      assert (not (Base.is_tuple elt));
      [| elt |]

    let is_singleton a = if size a = 1 then Some a.(0) else None
    let fold f a elt = CCArray.fold (fun acc elt -> f elt acc) elt a
    let to_iter = CCArray.to_iter

    let pp pp_elt fmt a =
      if is_unit a then Format.fprintf fmt "unit"
      else CCArray.pp ~pp_sep:(CCFormat.return " * ") pp_elt fmt a
  end

  module Partial = struct
    let dummy =  (Base.Constr (Longident.Lident "__dummy", [||]))

    type t = (Base.t, CCVector.rw) CCVector.t

    let mk ?tuple () =
      match tuple with
      | None -> CCVector.create ()
      | Some a -> CCVector.of_array a

    let add v t =
      assert (not (Base.is_tuple t));
      CCVector.push v t

    let add_n v t n =
      assert (not (Base.is_tuple t));
      CCVector.ensure_with ~init:dummy v (CCVector.length v + n);
      for _ = 1 to n do
        CCVector.push v t
      done

    let freeze v =
      let a = CCVector.to_array v in
      assert (Array.length a <> 1);
      CCArray.fast_sort Base.compare a;
      a
  end

  include Complet
end

and Tuple : Tuple with type base_t = Base.t = Multiset

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

(* Test functions *)
let is_arrow = function
  | Arrow _ -> true
  | _ -> false

let is_non_arrow_var = function
  | Var v -> Variable.is_non_arrow v
  | _ -> false

let is_non_tuple_var = function
  | Var v -> Variable.is_non_tuple v
  | _ -> false

(* smart constructors *)

let dummy = Constr (Longident.Lident "__dummy", [||])
let var env v = hashcons env @@ Var v
let frozen_var env v = hashcons env @@ FrozenVar v

let constr env lid params =
  hashcons env
  @@
  match params with
  | [||] when lid = LongIdent.unit -> Tuple Tuple.unit
  | params -> Constr (lid, params)

let arrows env params ret =
  assert (not @@ is_arrow ret);
  hashcons env @@ Arrow (params, ret)

let arrows_flatten env params ret =
  hashcons env
  @@
  match ret with
  | Arrow (params', ret) -> Arrow (Tuple.union params params', ret)
  | _ -> Arrow (params, ret)

let arrow env param ret =
  assert (not @@ is_arrow ret);
  hashcons env
  @@
  match param with
  | Tuple elts -> Arrow (elts, ret)
  | _ -> Arrow (Tuple.singleton param, ret)

let tuple env elts =
  hashcons env @@ Tuple elts

let other env x = hashcons env @@ Other (CCHash.poly x)

(** Utility functions *)

let tuple_flat_map f elts =
  match
    Tuple.to_iter elts
    |> Iter.flat_map
        (fun t -> match f t with
          | Tuple elts -> Tuple.to_iter elts
          | t -> Iter.return t)
    |> Iter.to_rev_list
  with
  | [] -> Tuple.unit
  | [ t ] -> Tuple.singleton t
  | l -> Tuple.mk_l l

let tuple_map_type env f elts =
  match
    Tuple.to_iter elts
    |> Iter.flat_map
        (fun t -> match f t with
          | Tuple elts -> Tuple.to_iter elts
          | t -> Iter.return t)
    |> Iter.to_rev_list
  with
  | [] -> tuple env Tuple.unit
  | [ t ] -> t
  | l -> tuple env @@ Tuple.mk_l l

let rec freeze_variables env (t : t) =
  match t with
  | Var v -> frozen_var env v
  | Constr (lid, t) -> constr env lid (Array.map (freeze_variables env) t)
  | Arrow (args, t) ->
      arrows env (tuple_flat_map (freeze_variables env) args) (freeze_variables env t)
  | Tuple ts -> tuple_map_type env (freeze_variables env) ts
  | Other _ | FrozenVar _ -> hashcons env t

let rec refresh_variables bdgs env (t : t) =
  match t with
  | Var v ->
    let v' =
      Variable.HMap.get_or_add bdgs
        ~f:(fun _ -> Variable.(Gen.gen (get_flags v) env.Env.var_gen)) ~k:v
    in
    var env v'
  | Constr (lid, t) -> constr env lid (Array.map (refresh_variables bdgs env) t)
  | Arrow (args, t) ->
    arrows env (tuple_flat_map (refresh_variables bdgs env) args) (refresh_variables bdgs env t)
  | Tuple ts -> tuple_map_type env (refresh_variables bdgs env) ts
  | Other _ | FrozenVar _ -> hashcons env t
let refresh_variables env t =
  refresh_variables (Variable.HMap.create 17) env t

(** import functions *)

let flatten_tuple env elts =
  let rec flatten = function
    | Tuple elt -> Tuple.to_iter elt |> Iter.flat_map flatten
    | t -> Iter.return t
  in
  match
    CCList.to_iter elts |> Iter.flat_map flatten |> Iter.to_rev_list
  with
  | [ t ] -> t
  | l -> tuple env (Tuple.mk_l l)

let flatten_args_arrows env param ret =
  let params = match param with
    | Tuple elts -> Tuple.to_iter elts |> Iter.to_list
    | ty -> [ty]
  in
  let rec flatten params = function
    | Arrow (args, ret) -> flatten (Tuple.fold List.cons args params) ret
    | t -> params, t
  in
  let params, ret = flatten params ret in
  let args = match params with
    | [] -> Tuple.unit
    | [ elt ] -> Tuple.singleton elt
    | l -> Tuple.mk_l l
  in
  arrows env args ret

let of_outcometree of_outcometree env var (out_ty : Outcometree.out_type) =
  match out_ty with
  | Otyp_var (_, name) -> var @@ Some name
  | Otyp_constr (id, params) ->
      params |> Iter.of_list |> Iter.map of_outcometree |> Iter.to_array
      |> constr env @@ LongIdent.of_outcometree id
  | Otyp_arrow (_, param, ret) ->
      let param = of_outcometree param in
      let ret = of_outcometree ret in
      flatten_args_arrows env param ret
  | Otyp_tuple elts ->
      assert (match elts with [_] -> false | _ -> true);
      elts |> List.map of_outcometree |> flatten_tuple env
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
      let param = of_parsetree param in
      let ret = of_parsetree ret in
      flatten_args_arrows env param ret
  | Ptyp_tuple elts ->
      assert (match elts with [_] -> false | _ -> true);
      elts |> List.map of_parsetree |> flatten_tuple env
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
  | None -> var env @@ Variable.(Gen.gen Flags.empty env.var_gen)
  | Some name -> (
      match String.HMap.get bdgs name with
      | Some v -> var env v
      | None ->
          let v = Variable.(Gen.gen Flags.empty env.var_gen) in
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
let tail = function Arrow (params, _) -> params | _ -> Tuple.unit

let iter =
  let iter_subs = function
    | Constr (_, params) -> Iter.of_array params
    | Arrow (params, ret) -> Iter.snoc (Tuple.to_iter params) ret
    | Tuple elts -> Tuple.to_iter elts
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
      Iter.flat_map iter_vars (Tuple.to_iter params) k;
      iter_vars ret k
  | Tuple elts -> Iter.flat_map iter_vars (Tuple.to_iter elts) k

let rec iter_consts t f =
  match t with
  | Other _ | FrozenVar _ | Var _ -> ()
  | Constr (lid, params) ->
      f lid;
      CCArray.iter (fun t -> iter_consts t f) params
  | Arrow (params, ret) ->
      Iter.flat_map iter_consts (Tuple.to_iter params) f;
      iter_consts ret f
  | Tuple elts -> Iter.flat_map iter_consts (Tuple.to_iter elts) f

let variable_clash v = function
  | Var v' -> Variable.rel v v' <> Variable.Smaller
  | Arrow _ -> Variable.is_non_arrow v
  | Tuple _ -> Variable.is_non_tuple v
  | _ -> false


(* pretty printing *)

let rec pp ppf = function
  | Var var -> Fmt.pf ppf "'%a" Variable.pp var
  | FrozenVar var -> Fmt.pf ppf "^%a" Variable.pp var
  | Constr (lid, [||]) -> LongIdent.pp ppf lid
  | Constr (lid, params) -> Fmt.pf ppf "%a@ %a" pp_array params LongIdent.pp lid
  | Arrow (params, ret) ->
      Fmt.pf ppf "@[<2>(%a@ ->@ %a)@]" (Tuple.pp pp_parens) params pp_parens ret
  | Tuple elts -> Fmt.pf ppf "@[<2>%a@]" (Tuple.pp pp_parens) elts
  | Other i -> Fmt.pf ppf "other%i" i

and pp_parens ppf ty =
  match ty with
  | Var _ | FrozenVar _ | Other _ | Constr _ -> pp ppf ty
  | Arrow _ -> Fmt.parens pp ppf ty
  | Tuple elts ->
      if CCOption.is_some @@ Tuple.is_singleton elts then pp ppf ty else Fmt.parens pp ppf ty

and pp_array ppf = function
  | [||] -> Fmt.string ppf "()"
  | [| elt |] -> pp ppf elt
  | arr -> Fmt.pf ppf "@[<2>(%a)@]" Fmt.(array ~sep:(any ", ") pp) arr
