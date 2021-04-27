
type t = Type.t Variable.Map.t

let simplify _vars unif = unif
  (* let named_vars, anonymous_vars =
   *   Variable.Map.partition (fun v _ -> Variable.Set.mem v vars) unif
   * in
   * Variable.Map.map (Type.substitute anonymous_vars) named_vars *)

let size = Variable.Map.cardinal

let compare t1 t2 =
  compare (size t1) (size t2)

let lt t1 t2 =
  compare t1 t2 < 0

let pp ppf (unif : t) =
  let pp_pair ppf (v,t) =
    Fmt.pf ppf "@[%a  →  %a@]" Variable.pp v Type.pp_parens t in
  Fmt.pf ppf "@[<v>%a@]"
    (Fmt.iter_bindings ~sep:(Fmt.unit ";@ ")
       Variable.Map.iter pp_pair)
    unif
