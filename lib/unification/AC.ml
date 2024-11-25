open Syntactic.Infix

module Partial = Type.Tuple.Partial

let acu = true

(* Shape *)
module S = Shape.Kind

module Stack = Syntactic.Stack

module System : sig
  type t = {
    nb_atom : int ; (* Number of atoms in each equation *)
    assoc_type : Type.t array ; (* Map from indices to the associated terms *)
    nb_non_var : int ; (* Non variables are at the beginning of the array, Variables at the end. *)
    nb_non_tuple : int ; (* Among the variable, first we have the non tuple variables *)
    system : int array array ;
  }

  val[@warning "-32"] pp : t Fmt.t

  val simplify_problem : Env.t -> ACTerm.t ACTerm.problem -> Type.t array ACTerm.problem

  val make : (Type.t array ACTerm.problem) list -> t * t List.t

  type dioph_solution
  val get_solution : dioph_solution -> int -> int

  val solve : (int array -> bool) -> t -> dioph_solution Iter.t

  [@@@ocaml.warning "-32"]
  val pp_dioph : dioph_solution Fmt.t
  [@@@ocaml.warning "+32"]

end = struct

  module Dioph = Diophantine.Make()
  module Solver = Dioph.Homogeneous_system

  type t = {
    nb_atom : int ; (* Number of atoms in each equation *)
    assoc_type : Type.t array ; (* Map from indices to the associated types *)
    nb_non_var : int ; (* Constants are at the beginning of the array, Variables at the end. *)
    nb_non_tuple : int ; (* Among the variable, first we have the non tuple variables *)
    system : int array array ;
  }

  let pp ppf {system; assoc_type; nb_atom; nb_non_var; nb_non_tuple} =
    Format.fprintf ppf "@[<v>{nb_atom: %i@ nb_non_var: %i@ nb_non_tuple: %i@ assoc: %a@ system: %a}@]"
    nb_atom nb_non_var nb_non_tuple
    Fmt.(vbox (array ~sep:(any ",") Type.pp)) assoc_type
    Fmt.(vbox (array ~sep:cut @@ array ~sep:(any ", ") int)) system

  (** For each variable, look at the represntative,
      if the representative is a constant or a variable, replace the variable by it
      otherwise replace the variable by the last variable on the chain. *)
  let simplify_problem env {ACTerm. left ; right} =
    let simplify tuple =
      let n, l =
        Type.Tuple.fold
          (fun t (n_acc, l_acc) ->
            let n, it = S.simplify env t in
            (n_acc + n, it:: l_acc))
          tuple (0, [])
      in
      let a = Array.make n Type.dummy in
      let i = ref 0 in
      Iter.flatten (CCList.to_iter l) (fun t -> assert(!i < n); a.(CCRef.get_then_incr i) <- t);
      a
    in
    {ACTerm. left = simplify left ;
             right = simplify right }

  (* Here the cut function is used in case we know that a type will not be compatible
     with the current build system, we can "remove" it by leaving it's coeficient to 0
     in the associated diophantine system. *)
  let add_problem get_index nb_atom {ACTerm. left; right} =
    let equation = Array.make nb_atom 0 in
    let add dir r =
      match get_index r with
      | Some i -> equation.(i) <- equation.(i) + dir
      | None -> ()
    in
    Array.iter (add 1) left ;
    Array.iter (add (-1)) right ;
    equation

  (* The number of constants and variables in a system *)
  let make_mapping problems =
    let all_types = List.fold_left (fun s {ACTerm. left; right} ->
      let s = Array.fold_left (fun s t -> Type.Set.add t s) s left in
      Array.fold_left (fun s t -> Type.Set.add t s) s right) Type.Set.empty problems
    in
    let shape_partition = S.partition all_types in
    let f part count t map =
      if part t then
        Type.Map.update t (fun x ->
          match x with
          | Some _ -> x
          | None -> Some (CCRef.get_then_incr count)) map
      else map
    in
    let aux part =
      let count = ref 0 in
      let m = Type.Set.fold (f part count) all_types Type.Map.empty in
      (!count, m)
    in
    let nb_non_tuple_var = Type.Set.fold (function
      | Type.Var v ->
        if Variable.is_non_tuple v then (+) 1 else Fun.id
      |  _ -> Fun.id) all_types 0
    in
    nb_non_tuple_var,
    {
      S.variable = aux shape_partition.variable;
      shapes = List.map aux shape_partition.shapes;
    }

  (*NOTE: Current implementation.
    A simple type is either a constant, a varialbe or a frozen variable.
     - We create a system only for variables
     - For each equivalent class on non variables, the shapes,
       we create a system for each.
       In this system, each type appearing is consider to be equal only to it self.

    Possible improvements:
      - In each equivalence class, we can look for term that are equal up to the current substitution
      TODO: is it true that equal type up to iso in the current substitution will be equal here?
       *)
  let make problems : t * t List.t =
    let nb_non_tuple, mapping = make_mapping problems in
    let nb_vars, vars = mapping.variable in
    let shape_partition = mapping.shapes in
    let get_index map t =
      Type.Map.get t map
    in

    let get_index_shape var_map map nb_free t =
      match Type.Map.get t var_map with
      | Some i -> Some (i + nb_free)
      | None -> Type.Map.get t map
    in

    let var_system =
      let nb_atom = nb_vars in
      let assoc_type = Array.make nb_atom Type.dummy in
      Type.Map.iter (fun k i ->
        assert (if i < nb_non_tuple then Type.is_non_tuple_var k
                else not (Type.is_non_tuple_var k));
        assoc_type.(i) <- k) vars ;
      let nb_non_var = 0 in
      let system =
        List.map (add_problem (get_index vars) nb_atom) problems
        |> Array.of_list
      in
      { nb_atom ; assoc_type ; nb_non_var; nb_non_tuple ; system }
    in

    let gen_shape_system nb_frees types_map =
      let nb_atom = nb_vars + nb_frees in
      let assoc_type = Array.make nb_atom Type.dummy in
      Type.Map.iter (fun t i -> assoc_type.(i) <- t) types_map;
      Type.Map.iter (fun k i -> assoc_type.(i + nb_frees) <- k) vars ;

      let nb_non_var = nb_frees in
      let system =
        List.map (add_problem
          (get_index_shape vars types_map nb_frees) nb_atom) problems
        |> Array.of_list
      in
      { nb_atom ; assoc_type ; nb_non_var; nb_non_tuple ; system }
    in
    let shape_systems = List.map
            (fun (n, tm) -> gen_shape_system n tm)
            shape_partition
    in
    var_system, shape_systems

  type dioph_solution = int array
  let get_solution = Array.get

  let solve cut { system ; _ } : dioph_solution Iter.t =
    Solver.solve ~cut @@ Solver.make system

  let pp_dioph =
    Fmt.(vbox (array ~sep:(any ", ") int))
end

(** See section 6.2 and 6.3 *)
module Dioph2Sol : sig

  type t = Env.t

  val get_solutions :
    Env.t -> System.t -> System.dioph_solution Iter.t
        -> (System.t * System.dioph_solution Iter.t) list -> t Iter.t

end = struct
  (** In the following, [i] is always the row/solution index and [j] is always
      the column/variable index. *)

  module Trace = Utils.Tracing

  type t = Env.t

  let _pp ppf (subset, unif) =
    let pp_pair ppf (v,t) =
      Fmt.pf ppf "@[%a → %a@]" Variable.pp v ACTerm.pp t
    in
    Fmt.pf ppf "@[<v2>%a: {@ %a@]@ }"
      Bitv.pp subset
      (Fmt.iter_bindings ~sep:Fmt.cut
         Variable.HMap.iter pp_pair)
      unif

  exception Bail

  (** Merge two env into one and treat the equation that result from the merge.
      For performence reason, the first env should be the main one.
      For exemple, is [env] is the accumulated one and [sol_env] the one from a solution,
      the function should be call [merge_env env sol_env]. *)
  let merge_env env1 env2 =
    let new_env, stack = Env.merge env1 env2 in
    if List.is_empty stack then new_env else
      match Syntactic.process_stack new_env (Stack.of_list stack) with
      | Syntactic.FailUnif _ | FailedOccurCheck _ -> raise Bail
      | Done -> new_env

  (* TODO: We could improve the naive iteration as follow, for each non_var elt of the system,
     we try all solution that cover it if it is not already cover *)
  let iterate_shape_subsets partials env system (solutions : (_ * _ * _) array) bitvars_cover k =
    let mask = Bitv.all_until (system.System.nb_non_var - 1) in
    let rec aux env coverage = function
      | -1 ->
          (* Check if the subset is large enough *)
          if Bitv.(is_subset mask coverage) then
            k (env, Bitv.(coverage lsr system.nb_non_var))
      | n ->
          aux env coverage (n-1);
          (* Check if the subset is small enough *)
          if Bitv.(is_empty (coverage && bitvars_cover.(n) && mask)) then (
            let (local_sol, symb, vars_part) = solutions.(n) in
            match merge_env env local_sol with
            | env ->
                let backup = Array.copy partials in
                List.iter (fun (i, n) -> partials.(i) <- Partial.add_n partials.(i) symb n) vars_part;
                let coverage = Bitv.(coverage || bitvars_cover.(n)) in
                aux env coverage (n-1);
                Array.blit backup 0 partials 0 (Array.length partials)
            | exception Bail -> ()
          )
    in
    aux env Bitv.empty (Array.length solutions - 1)

  let iterate_var_subsets_acu partials system (solutions : (_ * _ * _) array) bitvars_cover =
    (* Note: the environnement in the solution of the var system is useless, therefore we just discard it *)
    assert (system.System.nb_non_var = 0);
    let mask = Bitv.all_until (system.System.nb_non_tuple - 1) in
    let non_tuples_sols, pure_sols =
      List.combine (Array.to_list bitvars_cover) (Array.to_list solutions)
        |> List.partition (fun (b, _) -> not Bitv.(is_empty (b && mask)))
    in
    List.iter
      (fun (_, (_, symb, vars_part)) ->
        List.iter (fun (i, n) -> partials.(i) <- Partial.add_n partials.(i) symb n) vars_part
      ) pure_sols;
    fun env shape_coverage k ->
      let rec aux env coverage = function
        | [] -> (
          let partials =
            Variable.Map.of_iter
            (fun f ->
              Array.iteri
                (fun i p ->
                  match system.assoc_type.(i) with
                  | Var v -> f (v, p)
                  | _ -> failwith "Impossible, this should be a variable"
                ) partials
            )
          in
          let final_env, stack = Env.commit env partials in
            match
              let* _ =
                if List.is_empty stack then Done
                else Syntactic.process_stack final_env (Stack.of_list stack)
              in
              Syntactic.occur_check final_env
            with
            | Syntactic.FailUnif _ | FailedOccurCheck _ -> ()
            | Done ->
                  k final_env )
        | (cover, (_, symb, vars_part)):: t ->
          aux env coverage t;
          if Bitv.(is_empty (coverage && cover && mask)) then (
            let coverage = Bitv.(coverage || cover) in
            let backup = Array.copy partials in
            List.iter (fun (i, n) -> partials.(i) <- Partial.add_n partials.(i) symb n) vars_part;
            aux env coverage t;
            Array.blit backup 0 partials 0 (Array.length partials)
          )
      in
      aux env shape_coverage non_tuples_sols

  let iterate_var_subsets_ac partials system (solutions : (_ * _ * _) array) bitvars_cover env shape_coverage k =
    assert (system.System.nb_non_var = 0);
    let mask = Bitv.all_until (system.System.nb_atom - 1) in
    let rec aux env coverage = function
      | -1 ->
          Timeout.check ();
          (* Check if the subset is large enough *)
          if Bitv.(equal mask coverage) then (
            let partials =
              Variable.Map.of_iter
              (fun f ->
                Array.iteri
                  (fun i p ->
                    match system.assoc_type.(i) with
                    | Var v -> f (v, p)
                    | _ -> failwith "Impossible, this should be a variable"
                  ) partials
              )
            in
            let final_env, stack = Env.commit env partials in
            match
              let* _ =
                if List.is_empty stack then Done
                else Syntactic.process_stack final_env (Stack.of_list stack)
              in
              Syntactic.occur_check final_env
            with
            | Syntactic.FailUnif _ | FailedOccurCheck _ -> ()
            | Done ->
                  k final_env
          )
      | n ->
          aux env coverage (n-1);
          let coverage = Bitv.(coverage || bitvars_cover.(n)) in
          let (_, symb, vars_part) = solutions.(n) in
          let backup = Array.copy partials in
          List.iter (fun (i, n) -> partials.(i) <- Partial.add_n partials.(i) symb n) vars_part;
          aux env coverage (n-1);
          Array.blit backup 0 partials 0 (Array.length partials)
    in
    aux env shape_coverage (Array.length solutions - 1)

  let iterate_var_subsets =
    if acu then iterate_var_subsets_acu else iterate_var_subsets_ac

  let get_first_assoc_type sol {System. assoc_type; nb_non_var; _} =
    let rec aux i =
      assert (i < nb_non_var);
      if System.get_solution sol i <> 0 then
        assoc_type.(i), i + 1
      else aux (i+1)
    in
    aux 0

  (** Given a solution of the diophantine system, generate the equations associated to it
      and process them using the syntactic simplification. Return the environnement
      obtain after processing the equations and containing the partial assignment of
      variables *)
  let dioph2env env ({System. nb_atom; assoc_type; nb_non_var; _} as system) sol =
    let symb, start_i =
      if nb_non_var = 0 then
        Type.var (Env.tyenv env) (Env.gen Variable.Flags.empty env), 0
      else get_first_assoc_type sol system
    in
    let env = Env.copy env in
    (* Generate equations between the first variable of the solution *)
    let stack = ref Stack.empty in
    for i = start_i to nb_non_var - 1 do
      if System.get_solution sol i > 0 then
        stack := Stack.push !stack symb assoc_type.(i)
    done;
    (* Process the equations if we reach a contradiction, we stop, otherwise
       we generate the partial assignment for the variables. *)
    Logs.debug (fun m -> m "Stack sol dioph: @[[%a]@]" Stack.pp !stack);
    match
      if Stack.is_empty !stack then Syntactic.Done
      else Syntactic.process_stack env !stack (* TODO: maybe add an occur_check here but it would need an adapted version that looks in the partial too *)
    with
    | Syntactic.FailUnif _ | FailedOccurCheck _ -> None
    | Done ->
        let partial_add = ref [] in
        for i = nb_non_var to nb_atom - 1 do
          (* For the variable v, this mean that the variable should be associated with tuple
             that contains `System.get_solution sol i` time `symb` plus other stuff comming from
             from the other solutions that will be combined together.*)
          if System.get_solution sol i > 0 then
            (* When combining the solutions, we will store the partial tuple for each variable
               in a array of size `nb_atom - nb_non_var`, therefore we store as first composent
               the index in this futur table. *)
            partial_add := (i - nb_non_var, System.get_solution sol i) :: !partial_add
        done;
        Some (env, symb, !partial_add)

  (** From the iter of solution of the diophantine equations system,
      extract the solution in a vector and return an array of bitset
      for each variables, the bitset contains i if the ith solution
      gives a non-zero value to the variables. *)
  let extract_solutions env system
      (seq_solutions:System.dioph_solution Iter.t) =
    let bitvars = CCVector.create () in
    let stack = CCVector.create () in
    let counter = ref 0 in
    seq_solutions begin fun sol ->
      match dioph2env env system sol with
      | None -> ()
      | Some env_sol ->
        CCVector.push stack env_sol ;
        CCVector.push bitvars Bitv.empty;
        let i = CCRef.get_then_incr counter in
        for j = 0 to system.System.nb_atom - 1 do
          if System.get_solution sol j <> 0 then
            CCVector.set bitvars i (Bitv.add (CCVector.get bitvars i) j)
          else ()
        done;
    end;
    assert (!counter < Bitv.capacity) ; (* Ensure we are not doing something silly with bitsets. *)
    assert (!counter = CCVector.length stack);
    (CCVector.to_array stack, CCVector.to_array bitvars)

  (** Combine everything *)
  let get_solutions env system var_solutions shapes_sols k =

    (* Initialise each variable to unit with an empty list for ACU
       in case of AC, the condition force each variable to be associated with something
       therefore we did not need it before. *)
    let partials =
      Array.init (system.System.nb_atom - system.nb_non_var) (fun _ -> Type.Tuple.Partial.mk ())
    in

    Logs.debug (fun m -> m "Extract var solutions");
    let env_sols, sol_coverages = extract_solutions env system var_solutions in
    Logs.debug (fun m -> m "Extract shapes solutions");
    let shapes_sols =
      List.map (fun (system, solutions) -> system, extract_solutions env system solutions) shapes_sols
    in
    let iterate_var_subsets = iterate_var_subsets partials system env_sols sol_coverages in
    let rec combine_shapes env shapes_sols acc_coverage k =
      match shapes_sols with
      | (system, (env_sols, bitvars)) :: t ->
          let iter_sol = iterate_shape_subsets partials env system env_sols bitvars in
          iter_sol (fun (env_sol, coverage) ->
            combine_shapes env_sol t Bitv.(acc_coverage || coverage) k
          )
      | [] -> (* Here we could have a custom occur check to avoid the iteration on var *)
          iterate_var_subsets env acc_coverage k
    in
    let n_solutions = ref 0 in
    (* TODO: We should sort shapes_sol by the number of nb_non_var, the bigger first as it is
       more likele to be the one bringing clashes. Because constant will have nb_non_var = 1
       and bring no clash. Also arrow for now bring no clashes. *)
    Trace.wrap_ac_sol (combine_shapes env shapes_sols Bitv.empty)
      (fun x -> incr n_solutions; k x);
    Trace.message ~data:(fun () -> [("n", `Int !n_solutions)] )"Number of AC solutions"
end

(** [make_systems env problems] take a list of AC problem. Which mean problem
    of the form [t_1 * t_2 * t_4 = t_5 * t_6] and return a colection of
    systems of diophantine equations [homogenous_system * heterogenous_systems]
    where [homogenous_system] is a homogenous system of diophantine equations
    corresponding to the projection of [problems] on the variables.
    [heterogenous_systems] is a list of heterogenous systems which correspond
    to the projection of [problems] on the different shapes. *)
let make_systems env problems : System.t * _ =
  System.make @@ List.map (System.simplify_problem env) problems

let rec exists f s k stop : bool =
  if k = stop then false
  else f (System.get_solution s k) || exists f s (k+1) stop

(* TODO: If a shape system have no solution, then there is no solution to the problem and
 we could cut early. *)
(** Take a collection of diophantine equations corresponding to the projection
    of the original problem on to each constant and the homogenous systems,
    solve them and combine them into solution to the original problems. *)
let solve_systems env (var_system, shape_systems) =
  Logs.debug (fun m -> m "@[Var system: %a@]" System.pp var_system);
  Logs.debug (fun m -> m "@[Shape systems: %a@]" Fmt.(vbox @@ list ~sep:cut System.pp) shape_systems);

  let cut nb_not_tuple x =
    let rec cut_aux i stop solution =
      if i < stop then
        let v = solution.(i) in
        v > 1 || cut_aux (i+1) stop solution
      else false
    in
    cut_aux 0 nb_not_tuple x
  in

  let var_sols =
    System.solve (cut var_system.nb_non_tuple) var_system
      |> Iter.filter (fun sol ->
          exists (fun x -> x > 0) sol 0 var_system.nb_atom)
      (* TODO: Maybe a bug in the solver.
         If the only solution is the solution null (in case the system is empty,
         then the Hublot tree will generatethe solution constisting of the empty set
         (which is equal to 0) and the solution consisting of the solution 0 generating
         two identical solution instead of one. *)
  in

  (* TODO: for each Shape we need to solve the var system with only var.
      Then we filter it out here, we could tweak the solver to not have to do that.
      We initialise the solver by giving 1 to every position of firt_var, this way
      we garanty that the solution have a one there. *)
  (* TODO: here we should clean the solution, for all solution, we can
   check if two positions < nb_shapes receive a 1 but the two associated types
   are not compatible, then we should remove this solution *)
  let shapes_sols = List.map (fun (system: System.t) ->
    assert (system.nb_non_var > 0);
    ( system,
      System.solve (cut (system.nb_non_var + system.nb_non_tuple)) system
        |> Iter.filter (fun sol ->
            exists (fun x -> x > 0) sol 0 system.nb_non_var)
        )
    ) shape_systems
  in
  Dioph2Sol.get_solutions env var_system var_sols shapes_sols

let solve env problems =
  match problems with
  | [] -> failwith "Nothing to solve"
  | _ ->
    Logs.debug (fun m -> m "Solving AC system: @,@[%a@]" (CCList.pp ACTerm.pp_problem) problems);
    make_systems env problems
    |> solve_systems env

