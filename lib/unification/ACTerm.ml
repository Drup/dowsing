(** Terms and Problems for AC symbols *)

type t = Pure.t array

let add (t: t) p = Array.append [|p|] t

let make (p : t) = p

type problem = {left : t ; right : t }

let make_problem left right = {left;right}

let as_tuple env t : Type.t =
  match t with
  | [|x|] -> Pure.as_typexpr env x
  | t ->
    Type.tuple env
      (t |> Iter.of_array |> Iter.map (Pure.as_typexpr env) |> Type.NSet.of_iter)

let pp ppf t =
  match t with
  | [|x|] -> Pure.pp ppf x
  | t ->
    Fmt.pf ppf "@[<h>(%a)@]" Fmt.(array ~sep:(any ",@ ") Pure.pp) t

let pp_problem ppf {left ; right} =
  Fmt.pf ppf "%a = %a"
    Fmt.(array ~sep:(any ",") Pure.pp) left
    Fmt.(array ~sep:(any ",") Pure.pp) right
