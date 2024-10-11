type t = {
  tyenv : Type.Env.t ;
  mutable vars : Subst.t (* TODO: maybe a simple array could do it because variable have a continous range *) ;
  mutable tuples : ACTerm.problem list ;
  mutable arrows : ArrowTerm.problem list ;
  mutable partials : (Type.t list) Variable.Map.t;
  (** Store partial assignement, this represents the partial solution obtain
      when solving an AC problem. *)
  (* TODO: When we will have unit as the neutral element for *, we need to initialise this for each variables in the system to the empty list. If it stays empty that's mean it unifies with unit *)

  orig_vars : Variable.Set.t ;
}


let make ~(tyenv : Type.Env.t) ~orig_vars = {
  tyenv ;
  vars = Subst.empty ;
  tuples = [] ;
  arrows = [] ;
  partials = Variable.Map.empty;
  orig_vars ;
}

let copy { tyenv ; vars ; tuples ; arrows; partials ; orig_vars } =
  { tyenv ; vars ; tuples ; arrows ; partials ; orig_vars }

let vars e = e.vars
let tyenv t = t.tyenv
let gen flags e : Variable.t = Variable.Gen.gen flags e.tyenv.var_gen
    
let add e v ty =
  assert (not @@ Type.variable_clash v ty);
  e.vars <- Subst.add v ty e.vars

let remove e v = e.vars <- Subst.remove v e.vars

let init_partial e v =
  e.partials <- Variable.Map.update v
    (function
      | None -> Some []
      | Some _ -> failwith "Init on a non empty partials map of variable")
    e.partials

let extend_partial ?(by = 1) e v ty =
  let rec extend by l =
    assert (by >= 0);
    if by = 0 then l else extend (by - 1) (ty::l) in
  e.partials <-
    Variable.Map.update v
      (function
        | None -> Some (extend by [])
        | Some l -> Some (extend by l))
    e.partials

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
  let partials = Variable.Map.merge
    (fun _v l1 l2 ->
      let l1 = CCOption.get_or ~default:[] l1 in
      let l2 = CCOption.get_or ~default:[] l2 in
      Some (l1 @ l2))
    e1.partials e2.partials
  in
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
    partials;
    orig_vars = e1.orig_vars;
  }, !stack)

let commit e =
  let stack = ref [] in
  let vars = Variable.Map.merge
    (fun v t1 t2 ->
      match t1, t2 with
      | None, None -> None
      | Some t, None -> Some t
      | None, Some [t] -> (
        match t with
          | Type.Var _ | Arrow _ | Tuple _ ->
            stack := (Type.var (tyenv e) v, t) :: !stack; (* TODO: we will create a new variable because of this. We need to work to create less variables in general. *)
            None
          | _ -> Some t)
      | None, Some l -> Some (Type.tuple (tyenv e) (Type.NSet.of_list l))
      | Some t1, Some l ->
        (* TODO: Should we do something more clever here? Like look for the repr or
           we let the insert do that? For now, we will let the insert function do the job *)
        let t2 = match l with [t] -> t | l -> Type.tuple (tyenv e) (Type.NSet.of_list l) in
        if t1 != t2 then
          stack := (t1, t2) ::!stack;
        Some t1)
    e.vars
    e.partials
  in
  {e with vars; partials = Variable.Map.empty}, !stack

let pp_binding ppf (x,t) =
  Fmt.pf ppf "@[%a = %a@]"  Variable.pp x Type.pp t

let is_solved env =
  assert (Variable.Map.is_empty env.partials);
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
