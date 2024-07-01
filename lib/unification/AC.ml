(* Shape *)
module S = Shape.Kind

module Stack = Syntactic.Stack

module System : sig
  type t = {
    nb_atom : int ; (* Number of atoms in each equation *)
    assoc_type : Type.t array ; (* Map from indices to the associated terms *)
    first_var : int ; (* Constants are at the beginning of the array, Variables at the end. *)
    system : int array array ;
  }

  val[@warning "-32"] pp : t Fmt.t

  val simplify_problem : Env.t -> ACTerm.problem -> ACTerm.problem

  val make : ACTerm.problem list -> t * t List.t

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
    first_var : int ; (* Constants are at the beginning of the array, Variables at the end. *)
    system : int array array ;
  }

  let pp ppf {system; assoc_type; nb_atom; first_var} =
    Format.fprintf ppf "@[<v>{nb_atom: %i@ first_var: %i@ assoc: %a@ system: %a}@]"
    nb_atom first_var
    Fmt.(vbox (array ~sep:(any ",") Type.pp)) assoc_type
    Fmt.(vbox (array ~sep:cut @@ array ~sep:(any ", ") int)) system

  (** For each variable, look at the represntative,
      if the representative is a constant or a variable, replace the variable by it
      otherwise replace the variable by the last variable on the chain. *)
  let simplify_problem env {ACTerm. left ; right} =
    let f (t: Type.t) = match t with
      | Type.FrozenVar _ | Type.Constr (_, _) | Type.Arrow (_, _)
      | Type.Other _ -> [|t|]
      | Type.Tuple t  -> Type.NSet.as_array t
      | Type.Var v ->
        match Env.representative env v with
        | V v' -> [|Type.var (Env.tyenv env) v'|]
        | E (_, Tuple t) -> Type.NSet.as_array t
        | E (_, t) -> [|t|]
    in
    {ACTerm. left = CCArray.flat_map f left ; right = CCArray.flat_map f right }

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
    let f part count map t =
      if part t then
        Type.Map.update t (fun x ->
          match x with
          | Some _ -> x
          | None -> Some (CCRef.get_then_incr count)) map
      else map
    in
    let aux part =
      let count = ref 0 in
      let m = List.fold_left (fun m {ACTerm. left ; right} ->
        let m = Array.fold_left (f part count) m left in
        Array.fold_left (f part count) m right) Type.Map.empty problems
      in
      (!count, m)
    in
    (aux shape_partition.variable, List.map aux shape_partition.shapes)

(* TODO: We have a partition of shape, then we solve the system, then we can filter solution
   if some stuff is obviously not compatible *)
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
    let (nb_vars, vars), shape_partition = make_mapping problems in
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
      Type.Map.iter (fun k i -> assoc_type.(i) <- k) vars ;
      let first_var = 0 in
      let system =
        List.map (add_problem (get_index vars) nb_atom) problems
        |> Array.of_list
      in
      { nb_atom ; assoc_type ; first_var ; system }
    in

    let gen_shape_system nb_frees types_map =
      let nb_atom = nb_vars + nb_frees in
      let assoc_type = Array.make nb_atom Type.dummy in
      Type.Map.iter (fun t i -> assoc_type.(i) <- t) types_map;
      Type.Map.iter (fun k i -> assoc_type.(i + nb_frees) <- k) vars ;

      let first_var = nb_frees in
      let system =
        List.map (add_problem (get_index_shape vars types_map nb_frees) nb_atom) problems
        |> Array.of_list
      in
      { nb_atom ; assoc_type ; first_var ; system }
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

  (** Construction of the hullot tree to iterate through subsets. *)

  let rec for_all2_range f a k stop : bool =
    k = stop || f k a.(k) && for_all2_range f a (k+1) stop

  let large_enough const_vec bitvars subset =
    let f k col = Bitv.mem k const_vec || Bitv.do_intersect col subset in
    for_all2_range f bitvars 0 @@ Array.length bitvars

  let small_enough_kind first_var bitvars bitset =
    let f _ col = Bitv.(is_singleton_or_empty (bitset && col)) in
    for_all2_range f bitvars 0 first_var

  let large_enough_kind first_var bitvars subset =
    let f _ col = Bitv.do_intersect col subset in
    for_all2_range f bitvars 0 first_var

  let combine_shape_sols shape_sols =
    CCList.map_product_l (fun (system, solutions, bitvars, sols) ->
      List.map (fun x -> (system, solutions, bitvars, x))
      (Iter.to_list sols))
    shape_sols

  let iterate_shape_subsets len system bitvars =
    Hullot.Default.iter ~len
      ~small:(small_enough_kind system.System.first_var bitvars)
      ~large:(large_enough_kind system.first_var bitvars)

  let iterate_subsets nb_columns shape_combined_sols len bitvars =
    let shape_parts = combine_shape_sols shape_combined_sols in
    Iter.flat_map (fun shape_part ->
      let shape_vec= 
        let bitv = ref Bitv.empty in
        List.iter (fun (system, solutions, bitvars, shape_sol) ->
          for i = 0 to nb_columns - 1 do
            for j = 0 to CCVector.length solutions - 1 do
              if Bitv.mem j shape_sol && Bitv.mem j bitvars.(i + system.System.first_var) then
                bitv := Bitv.add !bitv i
            done
          done
        ) shape_part;
        !bitv
      in
      Iter.map (fun x -> (shape_part, x))
        (Hullot.Default.iter ~len
          ~small:(fun _ -> true)
          ~large:(large_enough shape_vec bitvars))
    ) (Iter.of_list shape_parts)

  let get_first_assoc_type sol {System. assoc_type; first_var; _} =
    let rec aux i =
      assert (i < first_var);
      if System.get_solution sol i <> 0 then
        assoc_type.(i)
      else aux (i+1)
    in
    aux 0

  (** Given a solution of the diophantine system, generate the equations associated to it
      and process them using the syntactic simplification. Return the environnement
      obtain after processing the equations and containing the partial assignment of
      variables *)
  let dioph2env env ({System. nb_atom; assoc_type; first_var; _} as system) sol =
    let symb =
      if first_var = 0 then
        Type.var (Env.tyenv env) (Env.gen env)
      else get_first_assoc_type sol system
    in
    let env = Env.copy env in
    (* Generate equations between the first variable of the solution *)
    let stack = ref Stack.empty in
    for i = 0 to first_var - 1 do
      if System.get_solution sol i > 0 then (
        stack := Stack.push !stack symb assoc_type.(i)
      )
    done;
    (* Process the equations if we reach a contradiction, we stop, otherwise
       we generate the partial assignment for the variables. *)
    Logs.debug (fun m -> m "Stack sol dioph: @[[%a]@]" Stack.pp !stack);
    match Syntactic.process_stack env !stack with
    | Syntactic.FailUnif _ | FailedOccurCheck _ -> None
    | Done ->
        for i = first_var to nb_atom - 1 do
          if System.get_solution sol i > 0 then (
            match assoc_type.(i) with
            | Var v -> Env.extend_partial ~by:(System.get_solution sol i) env v symb
            | _ -> failwith "Impossible"
          )
        done;
        Some env

  (** From the iter of solution of the diophantine equations system,
      extract the solution in a vector and return an array of bitset
      for each variables, the bitset contains i if the ith solution
      gives a non-zero value to the variables. *)
  let extract_solutions env stack system
      (seq_solutions:System.dioph_solution Iter.t) : Bitv.t array =
    let nb_columns = system.System.nb_atom in
    let bitvars = Array.make nb_columns Bitv.empty in
    let counter = ref 0 in
    seq_solutions begin fun sol ->
      match dioph2env env system sol with
      | None -> ()
      | Some env_sol ->
        CCVector.push stack env_sol ;
        let i = CCRef.get_then_incr counter in
        for j = 0 to nb_columns - 1 do
          if System.get_solution sol j <> 0 then
            bitvars.(j) <- Bitv.add bitvars.(j) i
          else ()
        done;
    end;
    assert (!counter < Bitv.capacity) ; (* Ensure we are not doing something silly with bitsets. *)
    assert (!counter = CCVector.length stack);
    bitvars

  let env_of_subset env solutions (shape_sols, var_sols) =
    (* TODO: vars should be only variable therefore the type should
       change removing the need for the match *)

    let exception Bail in
    let nb_sol = CCVector.length solutions in
    let solutions = CCVector.unsafe_get_array solutions in
    let final_env = ref env in

    try
      (* We add the variables comming from the solution in var_sols *)
      Logs.debug (fun m -> m "Merge env var sol: %a" Bitv.pp var_sols);
      for i = 0 to nb_sol - 1 do
        if Bitv.mem i var_sols then
          let new_env, stack = Env.merge !final_env solutions.(i) in
          match Syntactic.process_stack new_env (Stack.of_list stack) with
          | Syntactic.FailUnif _ | FailedOccurCheck _ -> raise Bail
          | Done -> final_env := new_env
      done;

      (* We add the variable comming from shape_sols *)
      List.iter (fun (_system, solutions, _bitvars, shape_sol) ->
        Logs.debug (fun m -> m "Merge env shape sol: %a" Bitv.pp shape_sol);
        let nb_sol = CCVector.length solutions in
        let solutions = CCVector.unsafe_get_array solutions in
        for i = 0 to nb_sol - 1 do
          if Bitv.mem i shape_sol then
            let new_env, stack = Env.merge !final_env solutions.(i) in
            match Syntactic.process_stack new_env (Stack.of_list stack) with
            | Syntactic.FailUnif _ | FailedOccurCheck _ -> raise Bail
            | Done -> final_env := new_env
        done
      ) shape_sols;

      Logs.debug (fun m -> m "Commit into final env");
      let final_env, stack = Env.commit !final_env in
      match
        (* TODO: need to check the call to occur_check here *)
        let open Syntactic in
        let* _ = Syntactic.process_stack final_env (Stack.of_list stack) in
        Syntactic.occur_check final_env
      with
      | Syntactic.FailUnif _ | FailedOccurCheck _ -> raise Bail
      | Done ->
          Some final_env
    with Bail -> None


  let get_shapes_solutions env
      (system, solutions) =
    let stack_env_sols = CCVector.create () in
    let bitvars = extract_solutions env stack_env_sols system solutions in
    let iter_sol = iterate_shape_subsets (CCVector.length stack_env_sols) system bitvars in
    system, stack_env_sols, bitvars, iter_sol

  (** Combine everything *)
  let get_solutions env system var_solutions shapes_sols k =
    let stack_env_sols = CCVector.create () in
    Logs.debug (fun m -> m "Extract var solutions");
    let bitvars = extract_solutions env stack_env_sols system var_solutions in
    Logs.debug (fun m -> m "Extract shapes solutions");
    let shapes_combined_sols = List.map (get_shapes_solutions env) shapes_sols in
    let subsets = iterate_subsets system.System.nb_atom shapes_combined_sols
                    (CCVector.length stack_env_sols) bitvars in
    let n_solutions = ref 0 in
    Trace.wrap_ac_sol (Iter.filter_map
      (env_of_subset env stack_env_sols)
      subsets) (fun x -> incr n_solutions; k x);
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

(* TODO: If a system have no solution, then there is no solution to the problem and
 we could cut early. *)
(** Take a collection of diophantine equations corresponding to the projection
    of the original problem on to each constant and the homogenous systems,
    solve them and combine them into solution to the original problems. *)
let solve_systems env (var_system, shape_systems) =
  Logs.debug (fun m -> m "@[Var system: %a@]" System.pp var_system);
  Logs.debug (fun m -> m "@[Shape systems: %a@]" Fmt.(vbox @@ list ~sep:cut System.pp) shape_systems);

  let var_sols =
    System.solve (fun _ -> false) var_system
      |> Iter.filter (fun sol ->
          let b = exists (fun x -> x > 0) sol 0 var_system.nb_atom in
          if not b then Logs.debug (fun m -> m "WRONG");
          b
          )
      (* TODO: Maybe a bug in the solver.
         If the only solution is the solution null (in case the system is empty,
         then the Hublot tree will generatethe solution constisting of the empty set
         (which is equal to 0) and the solution consisting of the solution 0 generating
         two identical solution instead of one. *)
  in

  let cut_shape nb_shapes x =
    let rec cut_aux i stop solution =
      if i < stop then
        let v = solution.(i) in
        v > 1 || cut_aux (i+1) stop solution
      else false
    in
    cut_aux 0 nb_shapes x
  in

  (* TODO: for each Shape we need to solve the var system with only var.
      Then we filter it out here, we could tweak the solver to not have to do that.
      We initialise the solver by giving 1 to every position of firt_var, this way
      we garanty that the solution have a one there. *)
  (* TODO: here we should clean the solution, for all solution, we can
   check if two positions < nb_shapes receive a 1 but the two associated types
   are not compatible, then we should remove this solution *)
  let shapes_sols = List.map (fun (system: System.t) ->
    ( system,
      System.solve (cut_shape system.first_var) system
        |> Iter.filter (fun sol ->
            exists (fun x -> x > 0) sol 0 system.first_var)
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

