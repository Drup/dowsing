type t = {
  tyenv : Type.Env.t ;
  vars : Type.t Variable.HMap.t ;
  mutable tuples : ACTerm.problem list ;
  mutable arrows : ArrowTerm.problem list ;
}

let make (tyenv : Type.Env.t) = {
  tyenv ;
  vars = Variable.HMap.create 17 ;
  tuples = [] ;
  arrows = [] ;
}

let copy { tyenv ; vars ; tuples ; arrows } =
  { tyenv ; vars ; tuples ; arrows }

let vars e = e.vars
let gen e = Variable.Gen.gen e.tyenv.var_gen
let add e = Variable.HMap.add e.vars

let push_tuple e left right =
  e.tuples <- ACTerm.make_problem left right :: e.tuples
let push_arrow e left right =
  e.arrows <- ArrowTerm.make_problem left right :: e.arrows

let pop_tuple e =
  match e.tuples with
  | [] -> None
  | pb :: tl -> e.tuples <- tl; Some pb

let pop_arrow e =
  match e.arrows with
  | [] -> None
  | pb :: tl -> e.arrows <- tl; Some pb


type representative =
  | V of Variable.t
  | E of Variable.t * Type.t

let rec representative_rec m x =
  match Variable.HMap.get m x with
  | None -> V x
  | Some (Type.Var x') -> representative_rec m x'
  | Some t -> E (x, t)
let representative e x = representative_rec e.vars x

let pp_binding ppf (x,t) =
  Fmt.pf ppf "@[%a = %a@]"  Variable.pp x Type.pp t

let is_solved env =
  if CCList.is_empty env.tuples
  && CCList.is_empty env.arrows
  then
    Some (Subst.simplify Variable.Set.empty env.vars)
  else
    None

let pp ppf { vars ; tuples ; arrows; _ } =
  Fmt.pf ppf "@[<v>@[<v2>Quasi:@ %a@]@,@[<v2>Tuple:@ %a@]@,@[<v2>Arrows:@ %a@]@]"
    Fmt.(iter_bindings ~sep:cut Variable.HMap.iter pp_binding) vars
    Fmt.(list ~sep:cut ACTerm.pp_problem) tuples
    Fmt.(list ~sep:cut ArrowTerm.pp_problem) arrows
