(** Terms and Problems for AC symbols *)

type t = Type.Tuple.t

let make (p : t) = p

type 'a problem = {left : 'a ; right : 'a }

let make_problem left right = {left;right}

let pp ppf t =
  match t with
  | [|x|] -> Type.pp ppf x
  | t ->
    Fmt.pf ppf "@[<h>(%a)@]" Fmt.(array ~sep:(any ",@ ") Type.pp) t

let pp_problem ppf {left ; right} =
  Fmt.pf ppf "%a = %a"
    (Type.Tuple.pp Type.pp) left
    (Type.Tuple.pp Type.pp) right
