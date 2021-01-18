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

  type term = t array

  let dummy = Constant (Longident.Lident "dummy")
  let var x = Var x
  let constant p = Constant p
  let tuple p = p
  let pure p = [|p|]

  type problem = {left : t array ; right : t array }

  let pp fmt = function
    | Var i -> Var.pp fmt i
    | Constant p -> T.P.pp fmt p
  let pp_term fmt = function
    | [|t|] -> pp fmt t
    | t -> Fmt.pf fmt "@[<h>(%a)@]" Fmt.(array ~sep:(unit ",@ ") pp) t

  let pp_problem fmt {left ; right} =
    Fmt.pf fmt "%a = %a"
      Fmt.(array ~sep:(unit ",") pp) left
      Fmt.(array ~sep:(unit ",") pp) right

  let as_typexpr p : Typexpr.t =
    let a =
      p
      |> Iter.of_array
      |> Iter.map (function Var v -> Typexpr.Var v | Constant c -> Typexpr.Constr (c, [||]))
      |> Typexpr.NSet.of_seq
    in
    Typexpr.Tuple a
end

module Arrow : sig
  type t = {
    args: Pure.t array;
    ret: Pure.t;
  }

  type problem = {
    left: t;
    right: t;
  }

  val make : Pure.t array -> Pure.t -> t
  val make_problem : t -> t -> problem
  val pp_problem : problem Fmt.t
end = struct
  type t = {
    args: Pure.t array;
    ret: Pure.t;
  }

  type problem = {
    left: t;
    right: t;
  }

  let make args ret : t = {args; ret}
  let make_problem left right = {left; right}

  let pp_problem fmt self =
    Fmt.pf fmt "%a -> %a = %a -> %a"
      Fmt.(array ~sep:(unit ",") Pure.pp) self.left.args
      Pure.pp self.left.ret
      Fmt.(array ~sep:(unit ",") Pure.pp) self.right.args
      Pure.pp self.right.ret

end

type representative = V of Var.t | E of Var.t * Typexpr.t

(** {2 A stack of unification pairs} *)
module Stack : sig
  type elt =
    | Var of Var.t * Typexpr.t
    | Expr of Typexpr.t * Typexpr.t
    | Subst of Var.t * Pure.term

  type t

  val empty : t
  val pop : t -> (elt * t) option
  val push : t -> Typexpr.t -> Typexpr.t -> t
  val push_quasi_solved : t -> Var.t -> Typexpr.t -> t
  val push_array2 : Typexpr.t array -> Typexpr.t array -> t -> t

  val pp : t Fmt.t
end = struct
  type elt =
    | Var of Var.t * Typexpr.t
    | Expr of Typexpr.t * Typexpr.t
    | Subst of Var.t * Pure.term

  type t = elt list
  let empty : t = []

  let[@inline] pop = function
    | [] -> None
    | h :: t -> Some (h, t)

  (* TODO: introduce some priorities:
     subst first, then quasi solved, then expr. ===> Use a form of deque. *)

  let push l t1 t2 = Expr (t1, t2) :: l
  let push_quasi_solved l v t = Var (v, t) :: l
  let push_array2 a1 a2 stack =
    CCArray.fold2 push stack a1 a2

  let pp_elt fmt = function
    | Var (v, t) -> Fmt.pf fmt "%a = %a" Var.pp v Typexpr.pp t
    | Expr (t1, t2) -> Fmt.pf fmt "%a = %a" Typexpr.pp t1 Typexpr.pp t2
    | Subst (v, p) -> Fmt.pf fmt "%a = %a" Var.pp v Pure.pp_term p

  let pp = Fmt.(vbox (list ~sep:cut pp_elt))
end

(* The [F] unification problem. *)
exception FailUnif
let fail () = raise FailUnif

module Env : sig
  type t

  val make : ?gen:Var.gen -> unit -> t

  val copy : t -> t
  val gen : t -> Var.t
  val get_gen : t -> Var.gen
  val get : t -> Var.t -> Typexpr.t option
  val vars : t -> Typexpr.t Var.Map.t
  val representative : t -> Var.t -> representative

  val push_pure : t -> Pure.problem -> unit
  val push_pure' : t -> Pure.t array -> Pure.t array -> unit
  val push_arrow : t -> Arrow.problem -> unit
  val push_arrow' : t -> Arrow.t -> Arrow.t -> unit
  val attach : t -> Var.t -> Typexpr.t -> unit

  val pop_pure : t -> Pure.problem option
  val pop_arrow : t -> Arrow.problem option

  val pp : t Fmt.t
end = struct
  type t = {
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

  let vars e = e.vars
  let gen e = Var.gen e.gen
  let get_gen e = e.gen
  let get e x = Var.Map.get x e.vars
  let attach e v t =
    e.vars <- Var.Map.add v t e.vars

  let push_pure e pb = e.pure_problems <- pb :: e.pure_problems
  let push_pure' e left right = push_pure e {Pure.left;right}
  let push_arrow e pb = e.arrows <- pb :: e.arrows
  let push_arrow' e left right = push_arrow e {Arrow.left;right}

  let pop_pure e =
    match e.pure_problems with
    | [] -> None
    | pb :: tl -> e.pure_problems <- tl; Some pb

  let pop_arrow e =
    match e.arrows with
    | [] -> None
    | pb :: tl -> e.arrows <- tl; Some pb

  let rec representative_rec m x =
    match Var.Map.get x m with
    | None -> V x
    | Some (T.Var x') -> representative_rec m x'
    | Some t -> E (x, t)
  let representative e x = representative_rec e.vars x

  let pp_binding fmt (x,t) =
    Fmt.pf fmt "@[%a = %a@]"  Var.pp x  Typexpr.pp t

  let pp fmt { vars ; pure_problems ; arrows; gen=_ } =
    Fmt.pf fmt "@[<v2>Quasi:@ %a@]@.@[<v2>Pure:@ %a@]@.@[<v2>Arrows:@ %a@]@."
      Fmt.(iter_bindings ~sep:cut Var.Map.iter pp_binding) vars
      Fmt.(list ~sep:cut Pure.pp_problem) pure_problems
      Fmt.(list ~sep:cut Arrow.pp_problem) arrows
end

(** Elementary AC-Unif *)
module System : sig
  type t = {
    nb_atom : int ; (* Number of atoms in each equation *)
    assoc_pure : Pure.t array ; (* Map from indices to the associated pure terms *)
    first_var : int ; (* Constants are at the beginning of the array, Variables at the end. *)
    system : int array array ;
  }

  val pp : t Fmt.t

  val simplify_problem : Env.t -> Pure.problem -> Pure.problem

  val make : Pure.problem list -> t

  type dioph_solution = private int array

  val solve : t -> dioph_solution Iter.t
end = struct
  module Dioph = Diophantine.Make()
  module Solver = Dioph.Homogeneous_system

  type t = {
    nb_atom : int ; (* Number of atoms in each equation *)
    assoc_pure : Pure.t array ; (* Map from indices to the associated pure terms *)
    first_var : int ; (* Constants are at the beginning of the array, Variables at the end. *)
    system : int array array ;
  }

  let pp ppf {system; _} =
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

  let add_problem get_index nb_atom {Pure. left; right} =
    let equation = Array.make nb_atom 0 in
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

  let make problems : t =
    let vars, nb_vars, consts, nb_consts = make_mapping problems in
    let get_index = function
      | Pure.Constant p -> T.P.HMap.find consts p
      | Pure.Var v -> Var.HMap.find vars v + nb_consts
    in
    let nb_atom = nb_vars + nb_consts in

    let assoc_pure = Array.make nb_atom Pure.dummy in
    T.P.HMap.iter (fun k i -> assoc_pure.(i) <- Pure.constant k) consts ;
    Var.HMap.iter (fun k i -> assoc_pure.(i+nb_consts) <- Pure.var k) vars ;

    let first_var = nb_consts in
    let system =
      Iter.of_list problems
      |> Iter.map (add_problem get_index nb_atom)
      |> Iter.to_array (* TO OPTIM *)
    in
    { nb_atom ; assoc_pure ; first_var ; system }

  type dioph_solution = int array

  let solve { first_var ; system ; _ } : dioph_solution Iter.t =
    let rec cut_aux i stop sum solution =
      if i < stop then
        let v = solution.(i) in
        v > 1 || cut_aux (i+1) stop (sum+v) solution
      else
        sum > 1
    in
    let cut x = cut_aux 0 first_var 0 x in
    Solver.solve ~cut @@ Solver.make system
end

module Unifier : sig
  type t = Bitv.t * Pure.t array Var.HMap.t

  val pp : t Fmt.t
end = struct
  type t = Bitv.t * Pure.t array Var.HMap.t

  let pp ppf (subset, unif : t) =
    let pp_pair ppf (v,t) =
      Fmt.pf ppf "(@[%a := %a@])" Var.pp v Pure.pp_term t in
    Fmt.pf ppf "@[%a: { %a }@]"
      Bitv.pp subset
      (Fmt.iter_bindings ~sep:(Fmt.unit "|@ ")
         Var.HMap.iter pp_pair)
      unif
end

(** See section 6.2 and 6.3 *)
module Dioph2Sol : sig
  val get_unifiers :
    Var.gen -> System.t -> System.dioph_solution Iter.t ->
    Unifier.t Iter.t
end = struct
  (** In the following, [i] is always the row/solution index and [j] is always
      the column/variable index. *)

  (** Construction of the hullot tree to iterate through subsets. *)

  let rec for_all2_range f a k stop : bool =
    k = stop || f a.(k) && for_all2_range f a (k+1) stop

  let large_enough bitvars subset =
    let f col = Bitv.is_subset col subset in
    for_all2_range f bitvars 0 @@ Array.length bitvars

  let small_enough first_var bitvars bitset =
    let f col = Bitv.(is_singleton (bitset && col)) in
    for_all2_range f bitvars 0 first_var

  let iterate_subsets len system bitvars =
    Hullot.Default.iter ~len
      ~small:(small_enough system.System.first_var bitvars)
      ~large:(large_enough bitvars)

  (** Constructions of the mapping from solutions to variable/constant *)
  let symbol_of_solution gen {System. first_var ; assoc_pure; _ } sol =
    (* By invariant, we know that solutions have at most one non-null
       factor associated with a constant, so we scan them linearly, and if
       non is found, we create a fresh variable. *)
    assert (Array.length sol >= first_var) ;
    let rec aux j =
      if j >= first_var then Pure.var (Var.gen gen)
      else if sol.(j) <> 0 then
        assoc_pure.(j)
      else aux (j+1)
    in
    aux 0

  let symbols_of_solutions gen system solutions =
    let pures = Array.make (CCVector.length solutions) Pure.dummy in
    let f i sol = pures.(i) <- symbol_of_solution gen system sol in
    CCVector.iteri f solutions;
    pures

  let extract_solutions stack nb_atom
      (seq_solutions:System.dioph_solution Iter.t) : Bitv.t array =
    let nb_columns = nb_atom in
    let bitvars = Array.make nb_columns Bitv.empty in
    let counter = ref 0 in
    seq_solutions begin fun sol ->
      let sol = (sol : System.dioph_solution :> int array) in
      CCVector.push stack sol ;
      let i = CCRef.get_then_incr counter in
      for j = 0 to nb_columns - 1 do
        if sol.(j) <> 0 then
          bitvars.(j) <- Bitv.add bitvars.(j) i
        else ()
      done;
    end;
    assert (!counter < Bitv.capacity) ; (* Ensure we are not doing something silly with bitsets. *)
    bitvars

  (* TO OPTIM *)
  let make_term buffer l : Pure.t array =
    CCVector.clear buffer;
    let f (n, symb) =
      for _ = 1 to n do CCVector.push buffer symb done
    in
    List.iter f l;
    Pure.tuple @@ CCVector.to_array buffer

  let unifier_of_subset
      vars solutions symbols subset : Unifier.t =
    assert (CCVector.length solutions = Array.length symbols);
    let unifiers = Var.HMap.create (Array.length vars) in
    let solutions = CCVector.unsafe_get_array solutions in
    for i = 0 to Array.length symbols - 1 do
      if Bitv.mem i subset then
        let sol = solutions.(i) in
        assert (Array.length sol = Array.length vars) ;
        let symb = symbols.(i) in
        (* Fmt.epr "Checking %i:%a for subset %a@." i Pure.pp symb Bitv.pp subset; *)
        for j = 0 to Array.length vars - 1 do
          match vars.(j) with
          | Pure.Constant _ -> ()
          | Pure.Var var ->
            let multiplicity = sol.(j) in
            Var.HMap.add_list unifiers var (multiplicity, symb)
        done;
    done;
    (* Fmt.epr "Unif: %a@." Fmt.(iter_bindings ~sep:(unit" | ") Var.HMap.iter @@
       pair ~sep:(unit" -> ") Var.pp @@ list ~sep:(unit",@ ") @@ pair int Pure.pp ) unifiers ; *)
    let buffer = CCVector.create_with ~capacity:10 Pure.dummy in
    let tbl = Var.HMap.create (Var.HMap.length unifiers) in
    Var.HMap.iter
      (fun k l ->
         let pure_term = make_term buffer l in
         Var.HMap.add tbl k pure_term)
      unifiers ;
    subset, tbl

  (** Combine everything *)
  let get_unifiers gen
      ({System. nb_atom; assoc_pure;_} as system)
      (seq_solutions:System.dioph_solution Iter.t) : Unifier.t Iter.t =
    let stack_solutions = CCVector.create_with ~capacity:5 [||] in
    let bitvars = extract_solutions stack_solutions nb_atom seq_solutions in
    Fmt.epr "@[Bitvars: %a@]@." (Fmt.Dump.array Bitv.pp) bitvars;
    (* Fmt.epr "@[<v2>Sol stack:@ %a@]@." (CCVector.pp System.Solver.pp_sol) stack_solutions; *)
    let symbols = symbols_of_solutions gen system stack_solutions in
    Fmt.epr "@[Symbols: %a@]@." (Fmt.Dump.array Pure.pp) symbols;
    let subsets = iterate_subsets (Array.length symbols) system bitvars in
    Iter.map
      (unifier_of_subset assoc_pure stack_solutions symbols)
      subsets
end

(** Main process *)

let rec process_stack env (stack:Stack.t) : unit =
  match Stack.pop stack with
  | Some (Stack.Expr (t1, t2), stack) -> insert_rec env stack t1 t2
  | Some (Var (v, t), stack) -> insert_var env stack v t
  | Some (Subst (v, p), stack) -> insert_subst env stack v p
  | None -> ()

and insert_rec env stack (t1 : T.t) (t2 : T.t) : unit =
  match t1, t2 with
  (* Decomposition rule
     (s₁,...,sₙ) p ≡ (t₁,...,tₙ) p  --> ∀i, sᵢ ≡ tᵢ
     when p is a type constructor.
  *)
  | T.Constr (p1, args1), T.Constr (p2, args2)
    when T.P.compare p1 p2 = 0 ->
    let stack = Stack.push_array2 args1 args2 stack in
    process_stack env stack

  (* Two arrows, we apply VA repeatedly
     (a₁,...,aₙ) -> r ≡ (a'₁,...,a'ₙ) -> r'  -->  an equivalent arrow problem
  *)
  | T.Arrow (arg1, ret1), T.Arrow (arg2, ret2)->
    let stack, pure_arg1 = variable_abstraction_all env stack arg1 in
    let stack, pure_ret1 = variable_abstraction env stack ret1 in
    let stack, pure_arg2 = variable_abstraction_all env stack arg2 in
    let stack, pure_ret2 = variable_abstraction env stack ret2 in
    Env.push_arrow' env (Arrow.make pure_arg1 pure_ret1) (Arrow.make pure_arg2 pure_ret2) ;
    process_stack env stack

  (* Two tuples, we apply VA repeatedly
     (s₁,...,sₙ) ≡ (t₁,...,tₙ) --> an equivalent pure problem
  *)
  | Tuple s, Tuple t ->
    let stack, pure_s = variable_abstraction_all env stack s in
    let stack, pure_t = variable_abstraction_all env stack t in
    Env.push_pure' env pure_s pure_t ;
    process_stack env stack

  | Var v, t | t, Var v ->
    insert_var env stack v t;
    process_stack env stack

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
    stack, Pure.var var

and insert_var env stack x s =
  match Env.representative env x with
  | V x ->
    begin match s with
      | T.Unit | T.Constr (_, [||])
      | T.Tuple _ | T.Constr _ | T.Arrow _ | T.Unknown _ ->
        quasi_solved env stack x s
      | T.Var y ->
        non_proper env stack x y
    end
  | E (_,u) ->
    (* variable was already bound *)
    insert_rec env stack u s

and insert_subst env stack x p = match p with
  | [| Pure.Var v |] -> non_proper env stack x v
  | _ -> quasi_solved env stack x (Pure.as_typexpr p) (* TO OPTIM *)

(* Quasi solved equation
   'x = (s₁,...sₙ)
   'x = (s₁,...sₙ) p
*)
and quasi_solved env stack x s : unit =
  match Env.get env x with
  | None ->
    Env.attach env x s ;
    process_stack env stack ;

    (* Rule representative *)
  | Some (T.Var y) ->
    Env.attach env y s ;
    process_stack env stack

  (* Rule AC-Merge *)
  | Some t ->
    (* TODO: use size of terms *)
    insert_rec env stack t s

(* Non proper equations
   'x ≡ 'y
*)
and non_proper env stack (x:Var.t) (y:Var.t) =
  let xr = Env.representative env x
  and yr = Env.representative env y
  in
  match xr, yr with
  | V x', V y' when Var.equal x' y' ->
    process_stack env stack
  | V x', (E (y',_) | V y')
  | E (y',_), V x' ->
    Env.attach env x' (T.Var y') ;
    process_stack env stack
  | E (_, t), E (_, s) ->
    (* TODO: use size of terms *)
    insert_rec env stack t s

let insert env t u : unit = insert_rec env Stack.empty t u

let process_pure_problems env : System.t =
  let rec iter acc =
    match Env.pop_pure env with
    | None -> acc
    | Some pb ->
      let pb = System.simplify_problem env pb in
      iter (pb::acc)
  in
  let l = iter [] in
  let sys = System.make l in
  sys

let rec process_arrow_problems env : System.t =
  match Env.pop_arrow env with
  | None -> process_pure_problems env
  | Some {Arrow. left; right } ->
    Env.push_pure env {Pure.left = [|left.ret|]; right = [|right.ret|]};
    Env.push_pure env {Pure.left = left.args; right = right.args};
    process_arrow_problems env

let get_system env = process_arrow_problems env

(** Checking for cycles *)
(* TO OPTIM/MEASURE *)
let occur_check env : bool =
  let nb_preds = Var.HMap.create 17 in
  let succs = Var.HMap.create 17 in
  let nb_representatives = Var.HMap.length nb_preds in

  let fill_nb_preds x ty =
    let aux v =
      Var.HMap.incr nb_preds v ;
      Var.HMap.add_list succs x v ;
    in
    T.vars ty |> Iter.iter aux
  in
  Var.Map.iter fill_nb_preds (Env.vars env);

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

let occur_check_or_fail env =
  if not (occur_check env) then fail()

let solve_system env system : _ Iter.t =
  let dioph_sols = System.solve system in
  Dioph2Sol.get_unifiers (Env.get_gen env) system dioph_sols

let unify ?gen (pairs: _ list) : Unifier.t Iter.t =
  let env = Env.make ?gen () in
  List.iter (fun (t1,t2) -> insert_rec env Stack.empty t1 t2) pairs;
  if not (occur_check env) then (
    raise FailUnif;
  );
  let system = process_arrow_problems env in
  solve_system env system
