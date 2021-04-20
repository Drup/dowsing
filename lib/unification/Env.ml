type t = {
  tyenv : Type.Env.t ;
  mutable vars : Type.t Variable.Map.t ;
  mutable tuples : ACTerm.problem list ;
  mutable arrows : ArrowTerm.problem list ;
}

let make (tyenv : Type.Env.t) = {
  tyenv ;
  vars = Variable.Map.empty ;
  tuples = [] ;
  arrows = [] ;
}

let copy { tyenv ; vars ; tuples ; arrows } =
  { tyenv ; vars ; tuples ; arrows }

let vars e = e.vars
let gen e = Variable.Gen.gen e.tyenv.var_gen
let var_names e = e.tyenv.var_names
let add e v t =
  e.vars <- Variable.Map.add v t e.vars

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
  match Variable.Map.get x m with
  | None -> V x
  | Some (Type.Var x') -> representative_rec m x'
  | Some t -> E (x, t)
let representative e x = representative_rec e.vars x

let pp_binding namefmt fmt (x,t) =
  Fmt.pf fmt "@[%a = %a@]"  (Variable.pp namefmt) x (Type.pp namefmt) t

let is_solved env =
  if CCList.is_empty env.tuples
  && CCList.is_empty env.arrows
  then
    Some (Subst.simplify env.tyenv.var_names env.vars)
  else
    None

let pp fmt { vars ; tuples ; arrows; tyenv } =
  let {Type.Env. var_names ; _ } = tyenv in
  Fmt.pf fmt "@[<v>@[<v2>Quasi:@ %a@]@,@[<v2>Tuple:@ %a@]@,@[<v2>Arrows:@ %a@]@]"
    Fmt.(iter_bindings ~sep:cut Variable.Map.iter @@ pp_binding var_names) vars
    Fmt.(list ~sep:cut @@ ACTerm.pp_problem var_names) tuples
    Fmt.(list ~sep:cut @@ ArrowTerm.pp_problem var_names) arrows
