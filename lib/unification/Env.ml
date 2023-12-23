type t = {
  tyenv : Type.Env.t ;
  mutable vars : Subst.t (* TODO: maybe a simple array could do it because variable might have a continous range *) ;
  mutable tuples : ACTerm.problem list ;
  mutable arrows : ArrowTerm.problem list ;
  orig_vars : Variable.Set.t ;
}


let make ~(tyenv : Type.Env.t) ~orig_vars = {
  tyenv ;
  vars = Subst.empty ;
  tuples = [] ;
  arrows = [] ;
  orig_vars ;
}

let copy { tyenv ; vars ; tuples ; arrows ; orig_vars } =
  { tyenv ; vars ; tuples ; arrows ; orig_vars }

let vars e = e.vars
let tyenv t = t.tyenv
let gen e : Variable.t =
  Obj.magic (1000 + Obj.magic (Variable.Gen.gen e.tyenv.var_gen))
    
let add e v ty = e.vars <- Subst.add v ty e.vars

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

let pp_binding ppf (x,t) =
  Fmt.pf ppf "@[%a = %a@]"  Variable.pp x Type.pp t

let is_solved env =
  if CCList.is_empty env.tuples
  && CCList.is_empty env.arrows
  then
    Some (Subst.simplify env.tyenv env.orig_vars env.vars)
  else
    None

let pp ppf { vars ; tuples ; arrows; _ } =
  Fmt.pf ppf "@[<v>@[<v2>Quasi:@ %a@]@,@[<v2>Tuple:@ %a@]@,@[<v2>Arrows:@ %a@]@]"
    Fmt.(iter_bindings ~sep:cut Variable.Map.iter pp_binding) vars
    Fmt.(list ~sep:cut ACTerm.pp_problem) tuples
    Fmt.(list ~sep:cut ArrowTerm.pp_problem) arrows
