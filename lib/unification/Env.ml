module Partial = Type.Tuple.Partial

type t = {
  tyenv : Type.Env.t ;
  mutable vars : Subst.t (* TODO: maybe a simple array could do it because variable have a continous range *) ;
  mutable tuples : ACTerm.t ACTerm.problem list ;
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
let gen flags e : Variable.t = Variable.Gen.gen flags e.tyenv.var_gen
    
let add e v ty =
  assert (not @@ Type.variable_clash v ty);
  e.vars <- Subst.add v ty e.vars

let remove e v = e.vars <- Subst.remove v e.vars

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

exception FlagsClash of Variable.t * Type.t

let rec representative_rec m x =
  match Variable.Map.get x m with
  | None -> V x
  | Some (Type.Var x') ->
      assert (Variable.are_flags_included x x');
      representative_rec m x'
  | Some t ->
      if Type.variable_clash x t then
        (* failwith "Env.representative: NonArrowVar followed by Arrow" *)
        raise (FlagsClash (x, t)) (* TODO: change this once the AC is fixed *)
      else E (x, t)
let representative e x = representative_rec e.vars x

(* TODO: During the merge, it could be that there is a contradiction between
   partials and vars but for now we will not see it. *)
let merge e1 e2 =
  assert (e1.tyenv == e2.tyenv);
  assert (e1.orig_vars == e2.orig_vars);
  let stack = ref [] in
  (* TODO: we should use a Unification.Stack.t for stack but we would get a module cycle *)
  let vars = Variable.Map.merge  (* TODO: Test physical equality to skip merging vars, not sure this will help, we need to think about a way to do stuff faster. *)
    (fun _v t1 t2 ->
      match t1, t2 with
      | None, None -> None
      | Some t, None | None, Some t -> Some t
      | Some t1, Some t2 ->
        (* TODO: Should we do something more clever here? Like look for the repr or
           we let the insert do that? For now, we will let the insert function do the job *)
        if t1 != t2 then
          stack := (t1, t2) ::!stack;
        Some t1)
    e1.vars
    e2.vars
  in
  ({ tyenv = e1.tyenv;
    vars;
    tuples = e1.tuples @ e2.tuples;
    arrows = e1.arrows @ e2.arrows;
    orig_vars = e1.orig_vars;
  }, !stack)

let commit e partials =
  let stack = ref [] in
  let vars = Variable.Map.merge
    (fun v t1 t2 ->
      match t1, t2 with
      | None, None -> None
      | Some t, None -> Some t
      | None, Some p -> begin
        match Type.Tuple.Partial.is_singleton p with
        | Some t ->
          begin match t with
            | Type.Var _ | Arrow _ | Tuple _ ->
              stack := (Type.var (tyenv e) v, t) :: !stack;
              None
            | _ -> Some t
          end
        | None -> Some (Type.tuple (tyenv e) (Type.Tuple.Partial.freeze p))
        end
      | Some t1, Some p ->
        (* TODO: Should we do something more clever here? Like look for the repr or
           we let the insert do that? For now, we will let the insert function do the job *)
        let t2 =
          match Type.Tuple.Partial.is_singleton p with
          | Some t -> t
          | None -> Type.tuple (tyenv e) (Type.Tuple.Partial.freeze p) in
        if t1 != t2 then
          stack := (t1, t2) ::!stack;
        Some t1)
    e.vars
    partials
  in
  {e with vars}, !stack

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
