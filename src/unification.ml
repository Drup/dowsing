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

  type problem = {left : t array ; right : t array }

  let pp fmt = function
    | Var i -> Var.pp fmt i
    | Constant p -> T.P.pp fmt p

  let pp_problem fmt {left ; right} =
    Fmt.pf fmt "%a = %a"
      Fmt.(array ~sep:(unit ",") pp) left
      Fmt.(array ~sep:(unit ",") pp) right
end

module Arrow = struct

  type problem = {
    arg1 : Pure.t array ; ret1 : Pure.t ;
    arg2 : Pure.t array ; ret2 : Pure.t ;
  }

  let pp_problem fmt {arg1; ret1; arg2; ret2} =
    Fmt.pf fmt "%a -> %a = %a -> %a"
      Fmt.(array ~sep:(unit ",") Pure.pp) arg1
      Pure.pp ret1
      Fmt.(array ~sep:(unit ",") Pure.pp) arg2
      Pure.pp ret2
end

type representative = V of Var.t | E of Var.t * Typexpr.t

module Stack = struct

  type elt =
    | Var of Var.t * Typexpr.t
    | Expr of Typexpr.t * Typexpr.t

  type t = elt list
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
    mutable arrows : Arrow.problem list ;
  }

  let make ?(gen=Var.init 0) () = {
    gen ;
    vars = Var.Map.empty ;
    pure_problems = [] ;
    arrows = [] ;
  }

  let copy { gen ; vars ; pure_problems ; arrows } =
    { gen ; vars ; pure_problems ; arrows }

  let gen e = Var.gen e.gen
  let get e x = Var.Map.get x e.vars
  let attach e v t =
    e.vars <- Var.Map.add v t e.vars

  let push_pure e left right =
    e.pure_problems <- {left;right} :: e.pure_problems

  let push_arrow e (arg1, ret1) (arg2, ret2) =
    e.arrows <- {arg1;ret1;arg2;ret2} :: e.arrows

  let rec representative_rec m x =
    match Var.Map.get x m with
    | None -> V x
    | Some (T.Var x') -> representative_rec m x'
    | Some t -> E (x, t)
  let representative e x = representative_rec e.vars x

  let pp_binding fmt (x,t) =
    Fmt.pf fmt "%a = %a"  Var.pp x  Typexpr.pp t

  let pp fmt { vars ; pure_problems ; arrows } =
    Fmt.pf fmt "@[<v2>Quasi:@ %a@]@.@[<v2>Pure:@ %a@]@.@[<v2>Arrows:@ %a@]@."
      Fmt.(iter_bindings ~sep:cut Var.Map.iter pp_binding) vars
      Fmt.(list ~sep:cut Pure.pp_problem) pure_problems
      Fmt.(list ~sep:cut Arrow.pp_problem) arrows
end

type d = Done

let rec process env stack =
  match stack with
  | Stack.Expr (t1, t2) :: stack -> insert env stack t1 t2
  | Var (v, t) :: stack -> insert_var env stack v t
  | [] -> Done

and insert env stack (t1 : T.t) (t2 : T.t) =
  match t1, t2 with
  (* Decomposition rule
     (s₁,...,sₙ) p ≡ (t₁,...,tₙ) p  --> ∀i, sᵢ ≡ tᵢ
     when p is a type constructor.
  *)
  | Constr (p1, args1), Constr (p2, args2)
    when T.P.compare p1 p2 = 0 ->
    let stack = Stack.push_array2 args1 args2 stack in
    process env stack

  (* Two arrows, we apply VA repeatedly
     (a₁,...,aₙ) -> r ≡ (a'₁,...,a'ₙ) -> r'  -->  an equivalent arrow problem
  *)
  | Arrow (arg1, ret1), Arrow (arg2, ret2) ->
    let stack, pure_arg1 = variable_abstraction_all env stack arg1 in
    let stack, pure_ret1 = variable_abstraction env stack ret1 in
    let stack, pure_arg2 = variable_abstraction_all env stack arg2 in
    let stack, pure_ret2 = variable_abstraction env stack ret2 in
    Env.push_arrow env (pure_arg1, pure_ret1) (pure_arg2, pure_ret2) ;
    process env stack

  (* Two tuples, we apply VA repeatedly
     (s₁,...,sₙ) ≡ (t₁,...,tₙ) --> an equivalent pure problem
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
   'x = (s₁,...sₙ)
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
   'x ≡ 'y
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


(** Checking for cycles *)

(* Check for cycles *)
let occur_check env =
  let nb_preds = Var.HMap.create 17 in
  let succs = Var.HMap.create 17 in
  let nb_representatives = Var.HMap.length nb_preds in

  let fill_nb_preds x ty =
    let aux v =
      Var.HMap.incr nb_preds v ;
      Var.HMap.add_list succs x v ;
    in
    T.vars ty |> Sequence.iter aux
  in
  Var.Map.iter fill_nb_preds env.Env.vars ;

  let rec loop n q = match q with
    | _ when n = nb_representatives -> true
    (* We eliminated all the variables: there are no cycles *)
    | [] -> false (* there is a cycle *)
    | x :: q ->
      let aux l v =
        Var.HMap.decr nb_preds v ;
        let n = Var.HMap.find nb_preds v in
        if n = 0 then v :: l
        else l
      in
      let q = List.fold_left aux q (Var.HMap.find succs x) in
      loop (n+1) q
  in

  let no_preds =
    Var.HMap.fold (fun x p l -> if p = 0 then x :: l else l) nb_preds []
  in
  loop 0 no_preds


(** Elementary AC-Unif *)

module System = struct

  type t = {
    get : int -> Pure.t ;
    is_const : int -> bool ;
    system : int array array ;
  }

  let pp ppf {system} =
    Fmt.(vbox (array ~sep:cut @@ array ~sep:(unit ", ") int)) ppf system

  (* Replace variables by their representative/a constant *)
  let simplify_problem env {Pure. left ; right} =
    let f x = match x with
      | Pure.Constant _ -> x
      | Pure.Var v ->
        match Env.representative env v with
        | V v' -> Var v'
        | E (_,Constr (p,[||])) -> Constant p
        | E _ -> x
    in
    {Pure. left = Array.map f left ; right = Array.map f right }

  let add_problem get_index size {Pure. left; right} =
    let equation = Array.make size 0 in
    let add dir r =
      let i = get_index r in
      equation.(i) <- equation.(i) + dir
    in
    Array.iter (add 1) left ;
    Array.iter (add (-1)) right ;
    equation

  (* The number of constants and variables in a system *)
  let make_mapping problems =
    let vars = Var.HMap.create 4 in
    let nb_vars = ref 0 in
    let consts = T.P.HMap.create 4 in
    let nb_consts = ref 0 in
    let f = function
      | Pure.Var v ->
        if Var.HMap.mem vars v then () else
          Var.HMap.add vars v @@ CCRef.get_then_incr nb_vars
      | Constant p ->
        if T.P.HMap.mem consts p then () else
          T.P.HMap.add consts p @@ CCRef.get_then_incr nb_consts
    in
    let aux {Pure. left ; right} =
      Array.iter f left ; Array.iter f right
    in
    List.iter aux problems ;
    vars, !nb_vars, consts, !nb_consts

  let array_of_map size neutral iter tbl =
    let a = Array.make size neutral in
    let f k i = a.(i) <- k in
    iter f tbl

  let make problems =
    let vars, nb_vars, consts, nb_consts = make_mapping problems in
    let get_index = function
      | Pure.Constant p -> T.P.HMap.find consts p
      | Pure.Var v -> Var.HMap.find vars v + nb_consts
    in
    let size = nb_vars + nb_consts in

    let get =
      let a = Array.make size (Pure.var @@ Var.inject 0) in
      T.P.HMap.iter (fun k i -> a.(i) <- Pure.constant k) consts ;
      Var.HMap.iter (fun k i -> a.(i+nb_consts) <- Pure.var k) vars ;
      Array.get a
    in
    let is_const i = i < nb_consts in
    let system =
      Sequence.of_list problems
      |> Sequence.map (add_problem get_index size)
      |> Sequence.to_array
    in
    { get ; is_const ; system }

end
