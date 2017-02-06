(** AC Unification

    Follow the algorithm in
    Competing for the AC-unification Race by Boudet (1993)
*)


module T = Typexpr
module Var = Variables

module Pure = struct
  type t =
    | Var of Var.t
    | Constant of T.P.t

  type term =
    | Pure of t
    | Tuple of t array

  let var x = Var x
  let constant p = Constant p
  let tuple p = Tuple p
  let pure p = Pure p

  type problem =
    | Problem of t array * t array [@ocaml.unboxed]

  let pp fmt = function
    | Var i -> Var.pp fmt i
    | Constant p -> T.P.pp fmt p

  let pp_problem fmt (Problem (a,b)) =
    Fmt.pf fmt "%a = %a"
      Fmt.(array ~sep:(unit ",") pp) a
      Fmt.(array ~sep:(unit ",") pp) b
end

type problem =
  | Var of Var.t * Typexpr.t
  | Expr of Typexpr.t * Typexpr.t

type representative = V of Var.t | E of Var.t * Typexpr.t

module Stack = struct
  type t = problem list
  let pop = function
    | [] -> None
    | h :: t -> Some (h, t)
  let push l t1 t2 = Expr (t1, t2) :: l
  let push_quasi_solved l v t = Var (v, t) :: l
  let push_array2 a1 a2 stack =
    CCArray.fold2 push stack a1 a2

  let pp_problem fmt = function
    | Var (v, t) -> Fmt.pf fmt "%a = %a" Var.pp v Typexpr.pp t
    | Expr (t1, t2) -> Fmt.pf fmt "%a = %a" Typexpr.pp t1 Typexpr.pp t2

  let pp = Fmt.(vbox (list ~sep:cut pp_problem))
end

(* The [F] unification problem. *)
exception FailUnif
let fail () = raise FailUnif

module Env = struct
  type env = {
    gen : Var.gen ;
    mutable vars : Typexpr.t Var.Map.t ;
    mutable pure_problems : Pure.problem list ;
  }

  let make ?(gen=Var.init 0) () = {
    gen ;
    vars = Var.Map.empty ;
    pure_problems = []
  }

  let gen e = Var.gen e.gen
  let get e x = Var.Map.get x e.vars
  let attach e v t =
    e.vars <- Var.Map.add v t e.vars

  let push_pure e a b =
    e.pure_problems <- (Pure.Problem (a,b)) :: e.pure_problems

  let rec representative_rec m x =
    match Var.Map.get x m with
    | None -> V x
    | Some (T.Var x') -> representative_rec m x'
    | Some t -> E (x, t)
  let representative e x = representative_rec e.vars x

  let pp_binding fmt (x,t) =
    Fmt.pf fmt "%a = %a"  Var.pp x  Typexpr.pp t

  let pp fmt { vars ; pure_problems } =
    Fmt.pf fmt "@[<v2>Quasi:@ %a@]@.@[<v2>Pure:@ %a@]@."
      Fmt.(iter_bindings ~sep:cut Var.Map.iter pp_binding) vars
      Fmt.(list ~sep:cut Pure.pp_problem) pure_problems
end

type d = Done

let rec process env stack =
  match stack with
  | Expr (t1, t2) :: stack -> insert env stack t1 t2
  | Var (v, t) :: stack -> insert_var env stack v t
  | [] -> Done

and insert env stack (t1 : T.t) (t2 : T.t) =
  match t1, t2 with
  (* Decomposition rule
     (s₁,...,sₙ) p ≡ (t₁,...,tₙ) p --> ∀i, sᵢ ≡ tᵢ
     when p is a type constructor.
  *)
  | Constr (p1, args1), Constr (p2, args2)
    when T.P.compare p1 p2 = 0 ->
    let stack = Stack.push_array2 args1 args2 stack in
    process env stack

  | Arrow (arg1, ret1), Arrow (arg2, ret2) ->
    (* TODO: Wrong, should fork here *)
    let stack = Stack.push stack (T.Tuple arg1) (T.Tuple arg2) in
    insert env stack ret1 ret2

  (* Two tuples, we apply VA repeatedly
     (s₁,...,sₙ) ≡ (t₁,...,tₙ) -> an equivalent pure problem
  *)
  | Tuple s, Tuple t ->
    let stack, pure_s = variable_abstraction_all env stack s in
    let stack, pure_t = variable_abstraction_all env stack t in
    Env.push_pure env pure_s pure_t ;
    process env stack

  | Var v, t | t, Var v -> insert_var env stack v t

  (* Clash rule
     Terms are incompatible
  *)
  | Constr _, Constr _  (* if same constructor, already checked above *)
  | (Constr _ | Tuple _ | Arrow _ | Unknown _),
    (Constr _ | Tuple _ | Arrow _ | Unknown _)
    -> fail ()

  | _ -> assert false

(* Repeated application of VA on an array of subexpressions. *)
and variable_abstraction_all env stack a =
  let r = ref stack in
  let f x =
    let stack, x = variable_abstraction env stack x in
    r := stack ;
    x
  in
  !r, Array.map f @@ T.NSet.as_array a

(* rule VA/Variable Abstraction
   Given a tuple, assign a variable to each subexpressions that is foreign
   and push them on stack.

   Returns the new state of the stack and the substituted expression.
*)
and variable_abstraction env stack t =
  match t with
  | T.Tuple _ -> assert false (* No nested tuple *)

  (* Not a foreign subterm *)
  | Var i -> stack, Pure.var i
  | Unit -> stack, Pure.constant T.P.unit
  | Constr (p, [||]) -> stack, Pure.constant p

  (* It's a foreign subterm *)
  | Arrow _ | Constr (_, _) | Unknown _ ->
    let var = Env.gen env in
    let stack = Stack.push_quasi_solved stack var t in
    stack, Pure.Var var

and insert_var env stack x s = match s with
  | T.Unit | T.Constr (_, [||])
  | T.Tuple _ | T.Constr _ | T.Arrow _ | T.Unknown _ ->
    quasi_solved env stack x s
  | T.Var y ->
    non_proper env stack x y

(* Quasi solved equation
   'x = (s₁,...sₙ) or
   'x = (s₁,...sₙ) p
 *)
and quasi_solved env stack x s =
  match Env.get env x with
  | None ->
    Env.attach env x s ;
    process env stack ;

  (* Rule representative *)
  | Some (T.Var y) ->
    Env.attach env y s ;
    process env stack

  (* Rule AC-Merge *)
  | Some t ->
    (* TODO: use size of terms *)
    insert env stack t s

(* Non proper equations
   x ≡ y
*)
and non_proper env stack x y =
  let xr = Env.representative env x
  and yr = Env.representative env y
  in
  match xr, yr with
  | V x', V y' when Var.equal x' y' ->
    process env stack
  | V x', (E (y',_) | V y')
  | E (y',_), V x' ->
    Env.attach env x' (T.Var y') ;
    process env stack
  | E (_, t), E (_, s) ->
    (* TODO: use size of terms *)
    insert env stack t s
