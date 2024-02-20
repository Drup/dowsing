(** Pure subterms: Variables or constants *)

type t =
  | Var of Variable.t
  | FrozenVar of Variable.t
  | Constant of LongIdent.t

let equal x y = match x, y with
  | Var x, Var y -> Variable.equal x y
  | FrozenVar x, FrozenVar y -> Variable.equal x y
  | Constant x, Constant y -> LongIdent.equal x y
  | _, _ -> false

let compare x y = match x,y with
  | Var x, Var y
  | FrozenVar x, FrozenVar y -> Variable.compare x y
  | Constant x, Constant y -> LongIdent.compare x y
  | Var _, (FrozenVar _ | Constant _) -> -1
  | (FrozenVar _ | Constant _), Var _ -> 1
  | FrozenVar _, Constant _ -> -1
  | Constant _, FrozenVar _ -> 1

let dummy = Constant (Longident.Lident "dummy")
let var x = Var x
let frozen x = FrozenVar x
let constant p = Constant p

let pp ppf = function
  | Var i -> Variable.pp ppf i
  | FrozenVar i -> Variable.pp ppf i
  | Constant p -> LongIdent.pp ppf p

let as_typexpr (env : Type.Env.t) = function
  | Var v -> Type.var env v
  | FrozenVar v -> Type.frozen_var env v
  | Constant c -> Type.constr env c [||]

module HMap = Hashtbl.Make(struct
    type nonrec t = t
    let hash = Hashtbl.hash
    let equal = equal
  end)

module Set = CCSet.Make(struct
  type nonrec t = t
  let compare = compare
end)
