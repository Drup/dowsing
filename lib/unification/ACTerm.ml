(** Terms and Problems for AC symbols *)

type t = Type.t array

let add (t: t) p = Array.append [|p|] t

let make (p : t) = p

type problem = {left : t ; right : t }

let make_problem left right = {left;right}

let as_tuple env t : Type.t =
  match t with
  | [|x|] -> x
  | t -> Type.tuple env @@ Type.NSet.of_array t

let pp ppf t =
  match t with
  | [|x|] -> Type.pp ppf x
  | t ->
    Fmt.pf ppf "@[<h>(%a)@]" Fmt.(array ~sep:(any ",@ ") Type.pp) t

let pp_problem ppf {left ; right} =
  Fmt.pf ppf "%a = %a"
    Fmt.(array ~sep:(any ",") Type.pp) left
    Fmt.(array ~sep:(any ",") Type.pp) right
