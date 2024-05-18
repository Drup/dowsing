module type S = sig
  type t

  val equal : t -> t -> bool
  val shape_or_var : Env.t -> Pure.t -> (t, Variable.t) Either.t

  module Map : CCMap.S with type key = t
end

module Kind : S = struct
  type t =
    | Constant of LongIdent.t
    | FrozenVar of Variable.t  (** Constants *)
    | Free of Type.Kind'.t

  let to_int = function Constant _ -> 0 | FrozenVar _ -> 1 | Free _ -> 3

  let compare s1 s2 =
    match (s1, s2) with
    | Constant c1, Constant c2 -> LongIdent.compare c1 c2
    | FrozenVar v1, FrozenVar v2 -> Variable.compare v1 v2
    | Free k1, Free k2 -> Type.Kind'.compare k1 k2
    | _ -> CCInt.compare (to_int s1) (to_int s2)

  let equal s1 s2 = compare s1 s2 = 0

  let shape_or_var env = function
    | Pure.Var v -> (
        match Env.representative env v with
        | V v -> Either.Right v
        | E (_, FrozenVar _) | E (_, Constr (_, [||])) | E (_, Var _) ->
            failwith "Error with simplification of problem"
        | E (v, (Tuple _ | Other _)) ->
            Logs.warn (fun m -> m "Wrong assumption on pure AC system.");
            Either.Right v
        | E (_v, ((Constr _ | Arrow _) as t)) ->
            Either.Left (Free (Type.kind' t)))
    | Constant name -> Either.Left (Constant name)
    | FrozenVar v -> Either.Left (FrozenVar v)

  module Map = CCMap.Make (struct
    type nonrec t = t

    let compare = compare
  end)
end

module Const : S = struct
  type t =
    | Constant of LongIdent.t
    | FrozenVar of Variable.t  (** Constants *)

  let to_int = function Constant _ -> 0 | FrozenVar _ -> 1

  let compare s1 s2 =
    match (s1, s2) with
    | Constant c1, Constant c2 -> LongIdent.compare c1 c2
    | FrozenVar v1, FrozenVar v2 -> Variable.compare v1 v2
    | _ -> CCInt.compare (to_int s1) (to_int s2)

  let equal s1 s2 = compare s1 s2 = 0

  let shape_or_var env = function
    | Pure.Var v -> (
        match Env.representative env v with
        | V v -> Either.Right v
        | E (_, FrozenVar _) | E (_, Constr (_, [||])) | E (_, Var _) ->
            failwith "Error with simplification of problem"
        | E (v, (Tuple _ | Other _)) ->
            Logs.warn (fun m -> m "Wrong assumption on pure AC system.");
            Either.Right v
        | E (v, (Constr _ | Arrow _)) ->
            Either.Right v)
    | Constant name -> Either.Left (Constant name)
    | FrozenVar v -> Either.Left (FrozenVar v)

  module Map = CCMap.Make (struct
    type nonrec t = t

    let compare = compare
  end)
end
