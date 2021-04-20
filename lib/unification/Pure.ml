(** Pure subterms: Variables or constants *)

type t =
  | Var of Variable.t
  | Constant of LongIdent.t

let dummy = Constant (Longident.Lident "dummy")
let var x = Var x
let constant p = Constant p

let pp namefmt fmt = function
  | Var i -> Variable.pp namefmt fmt i
  | Constant p -> LongIdent.pp fmt p

let as_typexpr = function
  | Var v -> Type.var v
  | Constant c -> Type.constr c [||]
