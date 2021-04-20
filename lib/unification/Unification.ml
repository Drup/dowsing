(** AC Unification

    Follow the algorithm in
    Competing for the AC-unification Race by Boudet (1993)
*)

module Logs = (val Logs.(src_log @@ Src.create __MODULE__))
let log = Logs.debug

module Pure = struct

  type t =
    | Var of Variable.t
    | Constant of LongIdent.t

  let dummy = Constant (Longident.Lident "dummy")
  let var x = Var x
  let constant p = Constant p

  let pp namefmt fmt = function
    | Var i -> Variable.pp namefmt fmt i
    | Constant p -> LongIdent.pp fmt p

  let as_typexpr = function
    | Var v -> Type.var v
    | Constant c -> Type.constr c [||]

end

module Tuple = struct

  type t = Pure.t array

  let make p = p

  type problem = {left : t ; right : t }

  let make_problem left right = {left;right}

  let as_typexpr t : Type.t =
    match t with
    | [|x|] -> Pure.as_typexpr x
    | t ->
      Type.tuple
        (t |> Iter.of_array |> Iter.map Pure.as_typexpr |> Type.MSet.of_iter)

  let pp namefmt fmt t =
      Fmt.pf fmt "@[<h>(%a)@]" Fmt.(array ~sep:(unit ",@ ") (Pure.pp namefmt)) t

  let pp_problem namefmt fmt {left ; right} =
    Fmt.pf fmt "%a = %a"
      Fmt.(array ~sep:(unit ",") @@ Pure.pp namefmt) left
      Fmt.(array ~sep:(unit ",") @@ Pure.pp namefmt) right

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
  val pp_problem : string Variable.HMap.t -> problem Fmt.t

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

  let pp_problem namefmt fmt self =
    Fmt.pf fmt "%a -> %a = %a -> %a"
      Fmt.(array ~sep:(unit ",") @@ Pure.pp namefmt) self.left.args
      (Pure.pp namefmt) self.left.ret
      Fmt.(array ~sep:(unit ",") @@ Pure.pp namefmt) self.right.args
      (Pure.pp namefmt) self.right.ret

end

type representative =
  | V of Variable.t
  | E of Variable.t * Type.t

(** {2 A stack of unification pairs} *)
module Stack : sig

  type elt =
    | Var of Variable.t * Type.t
    | Expr of Type.t * Type.t

  type t

  val empty : t
  val pop : t -> (elt * t) option
  val push_quasi_solved : t -> Variable.t -> Type.t -> t
  val push_array2 : Type.t array -> Type.t array -> t -> t

  val[@warning "-32"] pp : string Variable.HMap.t -> t Fmt.t

end = struct

  type elt =
    | Var of Variable.t * Type.t
    | Expr of Type.t * Type.t

  type t = elt list

  let empty : t = []

  let [@inline] pop = function
    | [] -> None
    | h :: t -> Some (h, t)

  (* TODO: introduce some priorities:
     subst first, then quasi solved, then expr. ===> Use a form of deque. *)

  let push l t1 t2 = Expr (t1, t2) :: l
  let push_quasi_solved l v t = Var (v, t) :: l
  let push_array2 a1 a2 stack =
    CCArray.fold2 push stack a1 a2

  let pp_elt namefmt fmt = function
    | Var (v, t) -> Fmt.pf fmt "%a = %a" (Variable.pp namefmt) v (Type.pp namefmt) t
    | Expr (t1, t2) -> Fmt.pf fmt "%a = %a" (Type.pp namefmt) t1 (Type.pp namefmt) t2

  let pp namefmt =
    Fmt.(vbox (list ~sep:cut @@ pp_elt namefmt))

end

(* The [F] unification problem. *)

exception FailUnif of Type.t * Type.t
let fail t1 t2 = raise (FailUnif (t1, t2))

module Unifier : sig

  type t = Type.t Variable.Map.t

  val simplify : string Variable.HMap.t -> t -> t

  val size : t -> int
  val compare : t -> t -> int
  val lt : t -> t -> bool

  val pp : string Variable.HMap.t -> t Fmt.t

end = struct

  type t = Type.t Variable.Map.t

  let simplify namefmt unif =
    let named_vars, anonymous_vars =
      Variable.Map.partition (fun v _ -> Variable.HMap.mem namefmt v) unif
    in
    Variable.Map.map (Type.substitute anonymous_vars) named_vars

  let size = Variable.Map.cardinal

  let compare t1 t2 =
    compare (size t1) (size t2)

  let lt t1 t2 =
    compare t1 t2 < 0

  let pp namefmt ppf (unif : t) =
    let pp_pair ppf (v,t) =
      Fmt.pf ppf "@[%a → %a@]" (Variable.pp namefmt) v (Type.pp namefmt) t in
    Fmt.pf ppf "@[%a@]"
      (Fmt.iter_bindings ~sep:(Fmt.unit ";@ ")
         Variable.Map.iter pp_pair)
      unif

end

module Env : sig

  type t

  val make : Type.Env.t -> t
  val copy : t -> t
  val gen : t -> Variable.t
  val get : t -> Variable.t -> Type.t option
  val vars : t -> Type.t Variable.Map.t
  val var_names : t -> string Variable.HMap.t
  val representative : t -> Variable.t -> representative

  val push_tuple : t -> Tuple.t -> Tuple.t -> unit
  val push_arrow : t -> Arrow.t -> Arrow.t -> unit
  val attach : t -> Variable.t -> Type.t -> unit

  val pop_tuple : t -> Tuple.problem option
  val pop_arrow : t -> Arrow.problem option

  val is_solved : t -> Unifier.t option

  val pp : t Fmt.t

end = struct

  type t = {
    tyenv : Type.Env.t ;
    mutable vars : Type.t Variable.Map.t ;
    mutable tuples : Tuple.problem list ;
    mutable arrows : Arrow.problem list ;
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
  let get e x = Variable.Map.get x e.vars
  let var_names e = e.tyenv.var_names
  let attach e v t =
    e.vars <- Variable.Map.add v t e.vars

  let push_tuple e left right =
    e.tuples <- Tuple.make_problem left right :: e.tuples
  let push_arrow e left right =
    e.arrows <- Arrow.make_problem left right :: e.arrows

  let pop_tuple e =
    match e.tuples with
    | [] -> None
    | pb :: tl -> e.tuples <- tl; Some pb

  let pop_arrow e =
    match e.arrows with
    | [] -> None
    | pb :: tl -> e.arrows <- tl; Some pb

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
      Some (Unifier.simplify env.tyenv.var_names env.vars)
    else
      None

  let pp fmt { vars ; tuples ; arrows; tyenv } =
    let {Type.Env. var_names ; _ } = tyenv in
    Fmt.pf fmt "@[<v2>Quasi:@ %a@]@,@[<v2>Pure:@ %a@]@,@[<v2>Arrows:@ %a@]"
      Fmt.(iter_bindings ~sep:cut Variable.Map.iter @@ pp_binding var_names) vars
      Fmt.(list ~sep:cut @@ Tuple.pp_problem var_names) tuples
      Fmt.(list ~sep:cut @@ Arrow.pp_problem var_names) arrows

end

(** Elementary AC-Unif *)
module System : sig

  type t = {
    nb_atom : int ; (* Number of atoms in each equation *)
    assoc_pure : Pure.t array ; (* Map from indices to the associated pure terms *)
    first_var : int ; (* Constants are at the beginning of the array, Variables at the end. *)
    system : int array array ;
  }

  val[@warning "-32"] pp : t Fmt.t

  val simplify_problem : Env.t -> Tuple.problem -> Tuple.problem

  val make : Tuple.problem list -> t

  type dioph_solution
  val get_solution : dioph_solution -> int -> int

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
  let simplify_problem env {Tuple. left ; right} =
    let f x = match x with
      | Pure.Constant _ -> x
      | Pure.Var v ->
        match Env.representative env v with
        | V v' -> Var v'
        | E (_,Constr (p,[||])) -> Constant p
        | E _ -> x
    in
    {Tuple. left = Array.map f left ; right = Array.map f right }

  let add_problem get_index nb_atom {Tuple. left; right} =
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
    let vars = Variable.HMap.create 4 in
    let nb_vars = ref 0 in
    let consts = LongIdent.HMap.create 4 in
    let nb_consts = ref 0 in
    let f = function
      | Pure.Var v ->
        if Variable.HMap.mem vars v then () else
          Variable.HMap.add vars v @@ CCRef.get_then_incr nb_vars
      | Constant p ->
        if LongIdent.HMap.mem consts p then () else
          LongIdent.HMap.add consts p @@ CCRef.get_then_incr nb_consts
    in
    let aux {Tuple. left ; right} =
      Array.iter f left ; Array.iter f right
    in
    List.iter aux problems ;
    vars, !nb_vars, consts, !nb_consts

  let make problems : t =
    let vars, nb_vars, consts, nb_consts = make_mapping problems in
    let get_index = function
      | Pure.Constant p -> LongIdent.HMap.find consts p
      | Pure.Var v -> Variable.HMap.find vars v + nb_consts
    in
    let nb_atom = nb_vars + nb_consts in

    let assoc_pure = Array.make nb_atom Pure.dummy in
    LongIdent.HMap.iter (fun k i -> assoc_pure.(i) <- Pure.constant k) consts ;
    Variable.HMap.iter (fun k i -> assoc_pure.(i+nb_consts) <- Pure.var k) vars ;

    let first_var = nb_consts in
    let system =
      Iter.of_list problems
      |> Iter.map (add_problem get_index nb_atom)
      |> Iter.to_array (* TO OPTIM *)
    in
    { nb_atom ; assoc_pure ; first_var ; system }

  type dioph_solution = int array
  let get_solution = Array.get

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

(** See section 6.2 and 6.3 *)
module Dioph2Sol : sig

  type t = Bitv.t * Tuple.t Variable.HMap.t

  val[@warning "-32"] pp : Env.t -> t Fmt.t

  val get_solutions :
    Env.t -> System.t -> System.dioph_solution Iter.t ->
    t Iter.t

end = struct
  (** In the following, [i] is always the row/solution index and [j] is always
      the column/variable index. *)

  type t = Bitv.t * Tuple.t Variable.HMap.t

  let pp env ppf (subset, unif : t) =
    let namefmt = Env.var_names env in
    let pp_pair ppf (v,t) =
      Fmt.pf ppf "@[%a → %a@]" (Variable.pp namefmt) v (Tuple.pp namefmt) t
    in
    Fmt.pf ppf "@[%a: { %a }@]"
      Bitv.pp subset
      (Fmt.iter_bindings ~sep:(Fmt.unit ";@ ")
         Variable.HMap.iter pp_pair)
      unif

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
    (* assert (Array.length sol >= first_var) ; *)
    let rec aux j =
      if j >= first_var then Pure.var (Env.gen gen)
      else if System.get_solution sol j <> 0 then
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
      CCVector.push stack sol ;
      let i = CCRef.get_then_incr counter in
      for j = 0 to nb_columns - 1 do
        if System.get_solution sol j <> 0 then
          bitvars.(j) <- Bitv.add bitvars.(j) i
        else ()
      done;
    end;
    assert (!counter < Bitv.capacity) ; (* Ensure we are not doing something silly with bitsets. *)
    bitvars

  (* TO OPTIM *)
  let make_term buffer l : Tuple.t =
    CCVector.clear buffer;
    let f (n, symb) =
      for _ = 1 to n do CCVector.push buffer symb done
    in
    List.iter f l;
    Tuple.make @@ CCVector.to_array buffer

  let unifier_of_subset
      vars solutions symbols subset : t =
    assert (CCVector.length solutions = Array.length symbols);
    let unifiers = Variable.HMap.create (Array.length vars) in
    let solutions = CCVector.unsafe_get_array solutions in
    for i = 0 to Array.length symbols - 1 do
      if Bitv.mem i subset then
        let sol = solutions.(i) in
        (* assert (Array.length sol = Array.length vars) ; *)
        let symb = symbols.(i) in
        (* log (fun m -> m "Checking %i:%a for subset %a@." i Pure.pp symb Bitv.pp subset) ; *)
        for j = 0 to Array.length vars - 1 do
          match vars.(j) with
          | Pure.Constant _ -> ()
          | Pure.Var var ->
            let multiplicity = System.get_solution sol j in
            Variable.HMap.add_list unifiers var (multiplicity, symb)
        done;
    done;
    (* log (fun m -> m "Unif: %a@." Fmt.(iter_bindings ~sep:(unit" | ") Variable.HMap.iter @@ *)
    (*    pair ~sep:(unit" -> ") Variable.pp @@ list ~sep:(unit",@ ") @@ pair int Pure.pp ) unifiers *)
    (* ) ; *)
    let buffer = CCVector.create_with ~capacity:10 Pure.dummy in
    let tbl = Variable.HMap.create (Variable.HMap.length unifiers) in
    Variable.HMap.iter
      (fun k l ->
         let pure_term = make_term buffer l in
         Variable.HMap.add tbl k pure_term)
      unifiers ;
    subset, tbl

  (** Combine everything *)
  let get_solutions env
      ({System. nb_atom; assoc_pure;_} as system)
      (seq_solutions:System.dioph_solution Iter.t) : t Iter.t =
    let stack_solutions = CCVector.create () in
    let bitvars = extract_solutions stack_solutions nb_atom seq_solutions in
    (* Fmt.epr "@[Bitvars: %a@]@," (Fmt.Dump.array Bitv.pp) bitvars;
     * Fmt.epr "@[<v2>Sol stack:@ %a@]@,"
     *   (CCVector.pp @@ Fmt.Dump.array Fmt.int) stack_solutions; *)
    let symbols = symbols_of_solutions env system stack_solutions in
    (* Fmt.epr "@[Symbols: %a@]@," (Fmt.Dump.array @@ Pure.pp namefmt) symbols; *)
    let subsets = iterate_subsets (Array.length symbols) system bitvars in
    Iter.map
      (unifier_of_subset assoc_pure stack_solutions symbols)
      subsets

end

(** Main process *)

(* Alternative unit type.
   Distinguish main loop from side-effecting functions.
*)
type done_ty = Done

let rec process_stack env (stack:Stack.t) : done_ty =
  match Stack.pop stack with
  | Some (Expr (t1, t2), stack) -> insert_rec env stack t1 t2
  | Some (Var (v, t), stack) -> insert_var env stack v t
  | None -> Done

and insert_rec env stack (t1 : Type.t) (t2 : Type.t) : done_ty =
  match t1, t2 with
  (* Decomposition rule
     (s₁,...,sₙ) p ≡ (t₁,...,tₙ) p  --> ∀i, sᵢ ≡ tᵢ
     when p is a type constructor.
  *)
  | Type.Constr (p1, args1), Type.Constr (p2, args2)
    when LongIdent.compare p1 p2 = 0 ->
    let stack = Stack.push_array2 args1 args2 stack in
    process_stack env stack

  (* Two arrows, we apply VA repeatedly
     (a₁,...,aₙ) -> r ≡ (a'₁,...,a'ₙ) -> r'  -->  an equivalent arrow problem
  *)
  | Type.Arrow (arg1, ret1), Type.Arrow (arg2, ret2)->
    let stack, pure_arg1 = variable_abstraction_all env stack arg1 in
    let stack, pure_ret1 = variable_abstraction env stack ret1 in
    let stack, pure_arg2 = variable_abstraction_all env stack arg2 in
    let stack, pure_ret2 = variable_abstraction env stack ret2 in
    Env.push_arrow env (Arrow.make pure_arg1 pure_ret1) (Arrow.make pure_arg2 pure_ret2) ;
    process_stack env stack

  (* Two tuples, we apply VA repeatedly
     (s₁,...,sₙ) ≡ (t₁,...,tₙ) --> an equivalent pure problem
  *)
  | Tuple s, Tuple t ->
    let stack, pure_s = variable_abstraction_all env stack s in
    let stack, pure_t = variable_abstraction_all env stack t in
    Env.push_tuple env pure_s pure_t ;
    process_stack env stack

  | Var v, t | t, Var v ->
    insert_var env stack v t

  (* Clash rule
     Terms are incompatible
  *)
  | Constr _, Constr _  (* if same constructor, already checked above *)
  | (Constr _ | Tuple _ | Arrow _ | Other _),
    (Constr _ | Tuple _ | Arrow _ | Other _)
    ->
    fail t1 t2

(* Repeated application of VA on an array of subexpressions. *)
and variable_abstraction_all env stack a =
  let r = ref stack in
  let f x =
    let stack, x = variable_abstraction env !r x in
    r := stack ;
    x
  in
  !r, Array.map f @@ Type.MSet.as_array a

(* rule VA/Variable Abstraction
   Given a tuple, assign a variable to each subexpressions that is foreign
   and push them on stack.

   Returns the new state of the stack and the substituted expression.
*)
and variable_abstraction env stack t =
  match t with
  (* A nested tuple. We consider that a pure subproblem *)
  | Type.Tuple ts ->
    let stack, all_vars = variable_abstraction_all env stack ts in
    let var = Env.gen env in
    Env.push_tuple env [|Pure.var var|] all_vars;
    stack, Pure.var var
  (* Not a foreign subterm *)
  | Var i -> stack, Pure.var i
  | Constr (p, [||]) -> stack, Pure.constant p
  (* It's a foreign subterm *)
  | Arrow _ | Constr (_, _) | Other _ ->
    let var = Env.gen env in
    let stack = Stack.push_quasi_solved stack var t in
    stack, Pure.var var

and insert_var env stack x s =
  match Env.representative env x with
  | V x ->
    begin match s with
      | Type.Constr (_, [||])
      | Type.Tuple _ | Type.Constr _ | Type.Arrow _ | Type.Other _ ->
        quasi_solved env stack x s
      | Type.Var y ->
        non_proper env stack x y
    end
  | E (_,u) ->
    (* variable was already bound *)
    insert_rec env stack u s

(* Quasi solved equation
   'x = (s₁,...sₙ)
   'x = (s₁,...sₙ) p
*)
and quasi_solved env stack x s =
  match Env.get env x with
  | None ->
    Env.attach env x s ;
    process_stack env stack

  (* Rule representative *)
  | Some (Type.Var y) ->
    Env.attach env y s ;
    process_stack env stack

  (* Rule AC-Merge *)
  | Some t ->
    (* TODO: use size of terms *)
    insert_rec env stack t s

(* Non proper equations
   'x ≡ 'y
*)
and non_proper env stack (x:Variable.t) (y:Variable.t) =
  let xr = Env.representative env x
  and yr = Env.representative env y
  in
  match xr, yr with
  | V x', V y' when Variable.equal x' y' ->
    process_stack env stack
  | V x', (E (y',_) | V y')
  | E (y',_), V x' ->
    Env.attach env x' (Type.var y') ;
    process_stack env stack
  | E (_, t), E (_, s) ->
    if Measure.size NodeCount t < Measure.size NodeCount s then
      insert_rec env stack t s
    else
      insert_rec env stack s t

let insert env t u : unit =
  let Done = insert_rec env Stack.empty t u in
  ()



let process_tuples env : System.t =
  let rec iter acc =
    match Env.pop_tuple env with
    | None -> acc
    | Some pb ->
      let pb = System.simplify_problem env pb in
      iter (pb::acc)
  in
  let l = iter [] in
  let sys = System.make l in
  sys

let rec process_arrows env : System.t =
  match Env.pop_arrow env with
  | None -> process_tuples env
  | Some {Arrow. left; right } ->
    Env.push_tuple env [|left.ret|] [|right.ret|];
    Env.push_tuple env left.args right.args;
    process_arrows env

(* Insertion of unifiers *)
and insert_substitution env stack x p =
  let ty = Tuple.as_typexpr p in
  let Done = insert_var env stack x ty in
  ()
let fork_with_solutions env ((_, map) : Dioph2Sol.t) =
  let s = Stack.empty in
  let env = Env.copy env in
  Variable.HMap.iter (insert_substitution env s) map;
  env


let solve_system env system =
  let dioph_sols = System.solve system in
  Dioph2Sol.get_solutions env system dioph_sols

(** Checking for cycles *)
(* TO OPTIM/MEASURE *)
let occur_check env : bool =
  let nb_preds = Variable.HMap.create 17 in
  let succs = Variable.HMap.create 17 in
  let nb_representatives = Variable.HMap.length nb_preds in

  let fill_nb_preds x ty =
    let aux v =
      Variable.HMap.incr nb_preds v ;
      Variable.HMap.add_list succs x v ;
    in
    Type.vars ty |> Iter.iter aux
  in
  Variable.Map.iter fill_nb_preds (Env.vars env);

  let rec loop n q = match q with
    | _ when n = nb_representatives -> true
    (* We eliminated all the variables: there are no cycles *)
    | [] -> false (* there is a cycle *)
    | x :: q ->
      let aux l v =
        Variable.HMap.decr nb_preds v ;
        let n = Variable.HMap.find nb_preds v in
        if n = 0 then v :: l
        else l
      in
      let q = List.fold_left aux q (Variable.HMap.find succs x) in
      loop (n+1) q
  in

  let no_preds =
    Variable.HMap.fold (fun x p l -> if p = 0 then x :: l else l) nb_preds []
  in
  loop 0 no_preds

let unifiers (tyenv : Type.Env.t) (pairs: _ list) : Unifier.t Iter.t =
  let rec solving_loop env k =
    if not (occur_check env) then ()
    else
      let system = process_arrows env in
      log (fun m -> m "@[<v2>System:@,%a" System.pp system) ;
      let solutions = solve_system env system in
      log (fun m -> m "@]@.") ;
      let f sol k =
        log (fun m -> m "@[<v2>Solution:@,%a@]@." (Dioph2Sol.pp env) sol) ;
        try
          let env = fork_with_solutions env sol in
          match Env.is_solved env with
          | Some map ->
            log (fun m -> m "@[<v2>Solved env:@,%a@]@." Env.pp env) ;
            k map
          | None ->
            log (fun m -> m "@[<v2>New env:@,%a@]@." Env.pp env) ;
            solving_loop env k
        with
        | FailUnif (t1, t2) ->
          log (fun m -> m "@[<v2>Conflict between:@;<1 2>@[%a@]@ and@;<1 2>@[%a@]@]@.@."
            (Type.pp tyenv.var_names) t1
            (Type.pp tyenv.var_names) t2
          )
      in
      Iter.flat_map f solutions k
  in
  let env0 = Env.make tyenv in
  try
    List.iter (fun (t1,t2) -> insert env0 t1 t2) pairs;
    log (fun m -> m "@[<v2>env0: @,%a@]@." Env.pp env0) ;
    solving_loop env0
  with
  | FailUnif _ -> Iter.empty

let unify (env : Type.Env.t) pairs =
  Iter.min ~lt:Unifier.lt @@ unifiers env pairs

let unifiable (env : Type.Env.t) pairs =
  not @@ Iter.is_empty @@ unifiers env pairs
