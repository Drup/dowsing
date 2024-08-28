module type S = sig
  type 'a partition = {
    variable : 'a;
    non_arrow_var : 'a;
    shapes : 'a list;
    arrows : 'a;
  }

  val partition : Type.Set.t -> (Type.t -> bool )partition
  (** Partition a set of type into the variables in one side and the partition of
      the type with a shape on the other. Here the notion of variable is broad,
      it means anything that we treat as variable. In the case of Const it is
      everything thing except constant and frozen variables. *)

  val simplify : Env.t -> Type.t -> Type.t array
end

module Kind : S = struct
  type t =
    | Other of int
    | FrozenVar of Variable.t  (** Constants *)
    | Constr of LongIdent.t
    | Arrow

  type 'a partition = {
    variable : 'a;
    non_arrow_var : 'a;
    shapes : 'a list;
    arrows : 'a;
  }

  let to_int = function
    | Other _ -> 0
    | FrozenVar _ -> 1
    | Constr _ -> 2
    | Arrow -> 3

  let compare s1 s2 =
    match (s1, s2) with
    | Other id1, Other id2 -> Int.compare id1 id2
    | FrozenVar v1, FrozenVar v2 -> Variable.compare v1 v2
    | Constr c1, Constr c2 -> LongIdent.compare c1 c2
    | _ -> CCInt.compare (to_int s1) (to_int s2)

  let of_type = function
    (* TODO: NonArrowVar should not be match with an arrow *)
    | Type.Var _ | Tuple _ | NonArrowVar _ ->
        raise (Invalid_argument "Shape.of_type")
    | Type.FrozenVar v -> FrozenVar v
    | Type.Constr (c, _) -> Constr c
    | Type.Arrow (_, _) -> Arrow
    | Type.Other id -> Other id

  module Map = CCMap.Make (struct
    type nonrec t = t

    let compare = compare
  end)

  let partition types =
    let variable = function Type.Var _ -> true | _ -> false in
    let non_arrow_var = function Type.NonArrowVar _ -> true | _ -> false in
    let shapes =
      Type.Set.fold
        (fun t m ->
          match t with
          | Type.Var _ | NonArrowVar _ | Arrow _ -> m
          | _ ->
              Map.update (of_type t)
                (function
                  | None -> Some (Type.Set.singleton t)
                  | Some s -> Some (Type.Set.add t s))
                m)
        types Map.empty
      |> Map.to_list
      |> List.map (fun (_, s) t -> Type.Set.mem t s)
    in
    { variable; non_arrow_var; shapes; arrows = Type.is_arrow }

    let simplify env (t: Type.t) = match t with
      | Type.FrozenVar _ | Type.Constr (_, _) | Type.Arrow (_, _)
      | Type.Other _ -> [|t|]
      | Type.Tuple t  -> Type.NSet.as_array t
      | Type.Var v -> (
        match Env.representative ~non_arrow:false env v with
        | V v' -> [| Type.var (Env.tyenv env) v'|]
        | NAR v' -> [| Type.non_arrow_var (Env.tyenv env) v'|]
        | E (_, Tuple t) -> Type.NSet.as_array t
        | E (_, t) -> [|t|]
      )
      | Type.NonArrowVar v -> (
        match Env.representative ~non_arrow:true env v with
        | V _ -> failwith "Impossible"
        | NAR v' -> [| Type.non_arrow_var (Env.tyenv env) v'|]
        | E (_, Tuple t) -> Type.NSet.as_array t
        | E (_, t) -> [|t|]
      )
end

module Const : S = struct
  type t = Constant of LongIdent.t | FrozenVar of Variable.t  (** Constants *)

  type 'a partition = {
    variable : 'a;
    non_arrow_var : 'a;
    shapes : 'a list;
    arrows : 'a;
  }

  let to_int = function Constant _ -> 0 | FrozenVar _ -> 1

  let compare s1 s2 =
    match (s1, s2) with
    | Constant c1, Constant c2 -> LongIdent.compare c1 c2
    | FrozenVar v1, FrozenVar v2 -> Variable.compare v1 v2
    | _ -> CCInt.compare (to_int s1) (to_int s2)

  module Map = CCMap.Make (struct
    type nonrec t = t

    let compare = compare
  end)

  let of_type = function
    | Type.Constr (n, [||]) -> Constant n
    | FrozenVar v -> FrozenVar v
    | _ -> raise (Invalid_argument "This is a variable")

  let partition types =
    let is_var = function
      | Type.FrozenVar _ | Constr (_, [||]) -> false
      | _ -> true
    in
    let shape_map =
      Type.Set.fold
        (fun t m ->
          match t with
          | Type.FrozenVar _ | Constr (_ , [||]) ->
              Map.update (of_type t)
                (function
                  | None -> Some (Type.Set.singleton t)
                  | Some s -> Some (Type.Set.add t s))
                m
          | _ -> m)
        types Map.empty
    in
    {
      variable = is_var;
      non_arrow_var = (fun _ -> false);
      shapes =
        Map.to_list shape_map |> List.map (fun (_, s) t -> Type.Set.mem t s);
      arrows = (fun _ -> false);
    }

    let simplify env (t: Type.t) = match t with
      | Type.FrozenVar _ | Type.Constr (_, [||]) -> [| t |]
      | Type.Var v -> (
        match Env.representative ~non_arrow:false env v with
        | V v' -> [|Type.var (Env.tyenv env) v'|]
        | NAR v' -> [| Type.non_arrow_var (Env.tyenv env) v'|]
        | E (_, (FrozenVar _ | Constr (_, [||]) as t)) -> [| t |]
        | E (v', _) -> [| Type.var (Env.tyenv env) v' |])
      | Type.NonArrowVar v -> (
        match Env.representative ~non_arrow:true env v with
        | V _ -> failwith "Impossible"
        | NAR v' -> [| Type.non_arrow_var (Env.tyenv env) v'|]
        | E (_, (FrozenVar _ | Constr (_, [||]) as t)) -> [| t |]
        | E (v', _) -> [| Type.var (Env.tyenv env) v' |])
      | _ ->
          let new_v = Env.gen env in
          match Syntactic.attach false env new_v t with
          | Syntactic.Done -> [| Type.var (Env.tyenv env) new_v |]
          | Syntactic.FailUnif (_, _) | Syntactic.FailedOccurCheck _ -> failwith "Impossible"
end
