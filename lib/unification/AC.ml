open Syntactic.Infix
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
    {ACTerm. left = CCArray.flat_map (S.simplify env) left ;
             right = CCArray.flat_map (S.simplify env) right }

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
    {
      S.variable = aux shape_partition.variable;
      non_arrow_var = aux shape_partition.non_arrow_var;
      shapes = List.map aux shape_partition.shapes;
      arrows = aux shape_partition.arrows
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
    let mapping = make_mapping problems in
    let nb_vars, vars = mapping.variable in
    let nb_non_arrow, non_arrow = mapping.non_arrow_var in
    let shape_partition = mapping.shapes in
    let nb_arrow, arrow = mapping.arrows in
    let nb_all_vars = nb_vars + nb_non_arrow in
    let all_vars = Type.Map.merge (fun _t o1 o2 ->
      match o1, o2 with
      | Some _, None -> o1
      | None, Some i -> Some (nb_vars + i)
      | Some _, Some _| None, None -> failwith "Variables exists in both normal and non arrow")
      vars non_arrow
    in
    let get_index map t =
      Type.Map.get t map
    in

    let get_index_shape var_map map nb_free t =
      match Type.Map.get t var_map with
      | Some i -> Some (i + nb_free)
      | None -> Type.Map.get t map
    in

    let var_system =
      let nb_atom = nb_all_vars in
      let assoc_type = Array.make nb_atom Type.dummy in
      Type.Map.iter (fun k i -> assoc_type.(i) <- k) all_vars ;
      let first_var = 0 in
      let system =
        List.map (add_problem (get_index all_vars) nb_atom) problems
        |> Array.of_list
      in
      { nb_atom ; assoc_type ; first_var ; system }
    in

    let gen_shape_system nb_frees types_map =
      let nb_atom = nb_all_vars + nb_frees in
      let assoc_type = Array.make nb_atom Type.dummy in
      Type.Map.iter (fun t i -> assoc_type.(i) <- t) types_map;
      Type.Map.iter (fun k i -> assoc_type.(i + nb_frees) <- k) all_vars ;

      let first_var = nb_frees in
      let system =
        List.map (add_problem (get_index_shape all_vars types_map nb_frees) nb_atom) problems
        |> Array.of_list
      in
      { nb_atom ; assoc_type ; first_var ; system }
    in
    let shape_systems = List.map
            (fun (n, tm) -> gen_shape_system n tm)
            shape_partition
    in
    let gen_arrow_system nb_frees types_map =
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
    let arrow_system =  gen_arrow_system nb_arrow arrow in
    var_system, (arrow_system::shape_systems)

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

  let iterate_shape_subsets env system solutions bitvars_cover k =
    let mask = Bitv.all_until (system.System.first_var - 1) in
    let rec aux env coverage = function
      | -1 ->
          (* Check if the subset is large enough *)
          if Bitv.(is_subset mask coverage) then
            k (env, Bitv.(coverage lsr system.first_var))
      | n ->
          aux env coverage (n-1);
          (* Check if the subset is small enough *)
          if Bitv.(is_empty (coverage && bitvars_cover.(n) && mask)) then
            match merge_env env solutions.(n) with
            | env ->
                let coverage = Bitv.(coverage || bitvars_cover.(n)) in
                aux env coverage (n-1)
            | exception Bail -> ()
    in
    aux env Bitv.empty (Array.length solutions - 1)

  let iterate_var_subsets env shape_coverage system solutions bitvars_cover k =
    let mask = Bitv.all_until (system.System.nb_atom - 1) in
    let rec aux env coverage = function
      | -1 ->
          (* Check if the subset is large enough *)
          Timeout.check ();
          if Bitv.(equal mask coverage) then (
            let final_env, stack = Env.commit env in
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
          match merge_env env solutions.(n) with
          | env ->
              let coverage = Bitv.(coverage || bitvars_cover.(n)) in
              aux env coverage (n-1)
          | exception Bail -> ()
    in
    aux env shape_coverage (Array.length solutions - 1)

  let get_first_assoc_type sol {System. assoc_type; first_var; _} =
    let rec aux i =
      assert (i < first_var);
      if System.get_solution sol i <> 0 then
        assoc_type.(i), i + 1
      else aux (i+1)
    in
    aux 0

  (** Given a solution of the diophantine system, generate the equations associated to it
      and process them using the syntactic simplification. Return the environnement
      obtain after processing the equations and containing the partial assignment of
      variables *)
  let dioph2env env ({System. nb_atom; assoc_type; first_var; _} as system) sol =
    let symb, start_i =
      if first_var = 0 then
        Type.var (Env.tyenv env) (Env.gen env), 0
      else get_first_assoc_type sol system
    in
    let env = Env.copy env in
    (* Generate equations between the first variable of the solution *)
    let stack = ref Stack.empty in
    for i = start_i to first_var - 1 do
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
        for i = first_var to nb_atom - 1 do
          if System.get_solution sol i > 0 then (
            match assoc_type.(i) with
            | Var v | NonArrowVar v -> Env.extend_partial ~by:(System.get_solution sol i) env v symb
            | _ -> failwith (CCFormat.sprintf "Impossible: %a" Type.pp assoc_type.(i))
          )
        done;
        Some env

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
    Logs.debug (fun m -> m "Extract var solutions");
    let env_sols, sol_coverages = extract_solutions env system var_solutions in
    Logs.debug (fun m -> m "Extract shapes solutions");
    let shapes_sols =
      List.map (fun (system, solutions) -> system, extract_solutions env system solutions) shapes_sols
    in
    let rec combine_shapes env shapes_sols acc_coverage k =
      match shapes_sols with
      | (system, (env_sols, bitvars)) :: t ->
          let iter_sol = iterate_shape_subsets env system env_sols bitvars in
          iter_sol (fun (env_sol, coverage) ->
            combine_shapes env_sol t Bitv.(acc_coverage || coverage) k
          )
      | [] -> (* Here we could have a custom occur check to avoid the iteration on var *)
          iterate_var_subsets env acc_coverage system env_sols sol_coverages k
    in
    let n_solutions = ref 0 in
    (* TODO: We should sort shapes_sol by the number of first_var, the bigger first as it is
       more likele to be the one bringing clashes. Because constant will have first_var = 1
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

  let var_sols =
    System.solve (fun _ -> false) var_system
      |> Iter.filter (fun sol ->
          exists (fun x -> x > 0) sol 0 var_system.nb_atom)
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

