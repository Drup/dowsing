(* Shape *)
module S = Shape.Kind

module System : sig
  type t = {
    nb_atom : int ; (* Number of atoms in each equation *)
    assoc_type : Type.t array ; (* Map from indices to the associated terms *)
    first_var : int ; (* Constants are at the beginning of the array, Variables at the end. *)
    system : int array array ;
  }

  val[@warning "-32"] pp : t Fmt.t

  val simplify_problem : Env.t -> ACTerm.problem -> ACTerm.problem

  val make : Env.t -> ACTerm.problem list -> t * t List.t

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
    assoc_type : Type.t array ; (* Map from indices to the associated pure terms *)
    first_var : int ; (* Constants are at the beginning of the array, Variables at the end. *)
    system : int array array ;
  }

  let pp ppf {system; assoc_type; nb_atom; first_var} =
    Format.fprintf ppf "@[<v>{nb_atom: %i@ fist_var: %i@ assoc: %a@ system: %a}@]"
    nb_atom first_var
    Fmt.(vbox (array ~sep:(any ",") Type.pp)) assoc_type
    Fmt.(vbox (array ~sep:cut @@ array ~sep:(any ", ") int)) system

  (** For each variable, look at the represntative,
      if the representative is a constant or a variable, replace the variable by it
      otherwise replace the variable by the last variable on the chain. *)
  let simplify_problem env {ACTerm. left ; right} =
    let f x = match x with
      | Pure.Constant _ | Pure.FrozenVar _ -> x
      | Pure.Var v ->
        match Env.representative env v with
        | V v' -> Var v'
        | E (_,Constr (p,[||])) -> Constant p
        | E (_, FrozenVar v) -> FrozenVar v
        | E (v', _) -> Var v'
    in
    {ACTerm. left = Array.map f left ; right = Array.map f right }

  let add_problem get_index nb_atom {ACTerm. left; right} =
    let equation = Array.make nb_atom 0 in
    let add dir r =
      let i = get_index r in
      if i >= 0 then
        equation.(i) <- equation.(i) + dir
    in
    Array.iter (add 1) left ;
    Array.iter (add (-1)) right ;
    equation

  let get_type_of_repr env = function
    | Pure.Constant name -> Type.constr (Env.tyenv env) name [||]
    | FrozenVar v -> Type.frozen_var (Env.tyenv env) v
    | Var v ->
        match Env.representative env v with
          | V _ -> assert false
          | E (_, t) -> t

  let partition_frees frees =
    S.Map.map (fun (n, l) ->
      ( n,
      List.fold_left
        (fun (c, m) t ->
          if Type.Map.mem t m then (c, m)
          else (c+1, Type.Map.add t c m)
        )
        (0, Type.Map.empty) l )
        |> snd
      ) frees

  (* The number of constants and variables in a system *)
  let make_mapping env problems =
    let vars = Variable.HMap.create 4 in
    let frees = ref S.Map.empty in
    let nb_vars = ref 0 in
    let f p = match S.shape_or_var env p with
      | Either.Right v ->
          if Variable.HMap.mem vars v then ()
          else Variable.HMap.add vars v @@ CCRef.get_then_incr nb_vars
      | Left s ->
          let t = get_type_of_repr env p in
          frees := S.Map.update s
          (function None -> Some (1, [ t ]) | Some (n, l) -> Some (n+1, t::l))
          !frees
    in
    let aux {ACTerm. left ; right} =
      Array.iter f left ; Array.iter f right
    in
    List.iter aux problems ;
    (* Partition the term with a free symbol into equivalence classes *)
    let free_classes = partition_frees !frees in
    (vars, !nb_vars, free_classes)

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
  let make env problems : t * t List.t =
    let vars, nb_vars, shape_partition = make_mapping env problems in
    let get_index_var p =
      match S.shape_or_var env p with
      | Either.Right v -> Variable.HMap.find vars v
      | _ -> -1
    in
    let get_index_shape s =
      let nb_frees, frees_map = S.Map.find s shape_partition in
      fun p -> match S.shape_or_var env p with
      | Either.Right v -> Variable.HMap.find vars v + nb_frees
      | Either.Left s' ->
          let t = get_type_of_repr env p in
          if S.equal s s' then Type.Map.find t frees_map
          else -1
    in

    let var_system =
      let nb_atom = nb_vars in
      let assoc_type = Array.make nb_atom Type.dummy in
      Variable.HMap.iter (fun k i -> assoc_type.(i) <- Type.var (Env.tyenv env) k) vars ;
      let first_var = 0 in
      let system =
        List.map (add_problem (get_index_var) nb_atom) problems
        |> Array.of_list
      in
      { nb_atom ; assoc_type ; first_var ; system }
    in

    let gen_shape_system k nb_frees types_map =
      let nb_atom = nb_vars + nb_frees in
      let assoc_type = Array.make nb_atom Type.dummy in
      Type.Map.iter (fun t i -> assoc_type.(i) <- t) types_map;
      Variable.HMap.iter (fun k i -> assoc_type.(i + nb_frees) <- Type.var (Env.tyenv env) k) vars ;

      let first_var = nb_frees in
      let system =
        List.map (add_problem (get_index_shape k) nb_atom) problems
        |> Array.of_list
      in
      { nb_atom ; assoc_type ; first_var ; system }
    in
    let shape_systems = List.map
            (fun (k, (n, tm)) -> gen_shape_system k n tm)
            @@ S.Map.to_list shape_partition
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

  type t = (Type.t * Type.t) Iter.t

  val get_solutions :
    Env.t -> System.t -> System.dioph_solution Iter.t
        -> (System.t * System.dioph_solution Iter.t) list -> t Iter.t

end = struct
  (** In the following, [i] is always the row/solution index and [j] is always
      the column/variable index. *)

  module Trace = Utils.Tracing

  type t = (Type.t * Type.t) Iter.t

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

  let combine_shape_sols kind_sols =
    CCList.map_product_l (fun (nb_kinds, symbols, solutions, bitvars, assoc_type, sols) ->
      List.map (fun x -> (nb_kinds, symbols, solutions, bitvars, assoc_type, x))
      (Iter.to_list sols))
    kind_sols

  let iterate_shape_subsets len system bitvars =
    Hullot.Default.iter ~len
      ~small:(small_enough_kind system.System.first_var bitvars)
      ~large:(large_enough_kind system.first_var bitvars)

  let iterate_subsets nb_columns shape_combined_sols len bitvars =
    let shape_parts = combine_shape_sols shape_combined_sols in
    Iter.flat_map (fun shape_part ->
      let shape_vec= 
        let bitv = ref Bitv.empty in
        List.iter (fun (nb_kinds, symbols, _solutions, bitvars, _assoc_type, kind_sols) ->
          for i = 0 to nb_columns - 1 do
            for j = 0 to Array.length symbols - 1 do
              if Bitv.mem j kind_sols && Bitv.mem j bitvars.(i + nb_kinds) then
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

  (** From the iter of solution of the diophantine equations system,
      extract the solution in a vector and return an array of bitset
      for each variables, the bitset contains i if the ith solution
      gives a non-zero value to the variables. *)
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
  let make_term env buffer l : Type.t =
    CCVector.clear buffer;
    let f acc (n, symb) =
      let tmp = ref acc in
      for _ = 1 to n do tmp := symb :: ! tmp done;
      !tmp
    in
    CCList.fold_left f [] l
      |> fun l -> Type.tuple env @@ Type.NSet.of_list l

  let unifier_of_subset env vars solutions symbols (shape_sols, pure_sols) =
    (* TODO: vars should be only variable therefore the type should
       change removing the need for the match *)
    assert (CCVector.length solutions = Array.length symbols);

    (* Unifier is a map from variable to a list of pure terms presenting the
       the tuple associated with the variable *)
    let unifiers = Variable.HMap.create (Array.length vars) in
    let equations = ref [] in
    let solutions = CCVector.unsafe_get_array solutions in

    (* We add the variables comming from the solution in pure_sols *)
    for i = 0 to Array.length symbols - 1 do
      if Bitv.mem i pure_sols then
        let sol = solutions.(i) in
        (* assert (Array.length sol = Array.length vars) ; *)
        let symb = symbols.(i) in
        (* log (fun m -> m "Checking %i:%a for subset %a@." i Pure.pp symb Bitv.pp subset) ; *)
        for j = 0 to Array.length vars - 1 do
          match vars.(j) with
          | Type.Var var ->
            let multiplicity = System.get_solution sol j in
            Variable.HMap.add_list unifiers var (multiplicity, symb)
          | _ -> failwith "Impossible"
        done;
    done;
    (* log (fun m -> m "Unif: %a@." Fmt.(iter_bindings ~sep:(unit" | ") Variable.HMap.iter @@ *)
    (*    pair ~sep:(unit" -> ") Variable.pp @@ list ~sep:(unit",@ ") @@ pair int Pure.pp ) unifiers *)
    (* ) ; *)

    (* We add the variable comming from kind_sols *)
    List.iter (fun (_nb_shapes, symbols, solutions, _bitvars, assoc_type, shape_sol) ->
      let solutions = CCVector.unsafe_get_array solutions in
      for i = 0 to Array.length symbols - 1 do
        if Bitv.mem i shape_sol then
          let sol = solutions.(i) in
          let symb = symbols.(i) in
          Array.iteri (fun j v -> match v with
          | Type.Var var ->
              let multiplicity = System.get_solution sol j in
              Variable.HMap.add_list unifiers var (multiplicity, symb)
          | t ->
              equations := (symb, t) :: !equations
          ) assoc_type
      done
    ) shape_sols;

    let buffer = CCVector.create_with ~capacity:10 Type.dummy in
    fun k ->
      List.iter k !equations;
      Variable.HMap.iter
        (fun key l ->
           let pure_term = make_term env buffer l in
           k (Type.var env key, pure_term))
        unifiers

  let get_first_assoc_type sol assoc_type =
    let rec aux i =
      if System.get_solution sol i <> 0 then
        assoc_type.(i)
      else aux (i+1)
    in
    aux 0

  let get_shapes_solutions
      (({System. nb_atom; first_var; assoc_type; _} as system), solutions) =
    let stack_solution = CCVector.create () in
    let bitvars = extract_solutions stack_solution nb_atom solutions in
    let symbols = Array.init (CCVector.length stack_solution)
          (fun i -> get_first_assoc_type (CCVector.get stack_solution i) assoc_type) in
    let iter_sol = iterate_shape_subsets (Array.length symbols) system bitvars in
    first_var, symbols, stack_solution, bitvars, assoc_type, iter_sol

  (** Combine everything *)
  let get_solutions env
      {System. nb_atom; assoc_type;_}
      pure_solutions shapes_sols k =
    let stack_solutions = CCVector.create () in
    let bitvars = extract_solutions stack_solutions nb_atom pure_solutions in
    (* Logs.debug (fun m -> m "@[Bitvars: %a@]@." (Fmt.Dump.array Bitv.pp) bitvars);
    Logs.debug (fun m -> m "@[<v2>Sol stack:@ %a@]@."
      (CCVector.pp System.pp_solution) stack_solutions); *)
    let symbols = Array.init (CCVector.length stack_solutions)
                             (fun _ -> Type.var (Env.tyenv env) (Env.gen env)) in
    (* Logs.debug (fun m -> m "@[Symbols: %a@]@." (Fmt.Dump.array @@ Pure.pp) symbols); *)
    let shapes_combined_sols = List.map get_shapes_solutions shapes_sols in
    let subsets = iterate_subsets nb_atom shapes_combined_sols
                    (Array.length symbols) bitvars in
    let n_solutions = ref 0 in
    Trace.wrap_ac_sol (Iter.map
      (unifier_of_subset (Env.tyenv env) assoc_type stack_solutions symbols)
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
  System.make env @@ List.map (System.simplify_problem env) problems

let rec exists f s k stop : bool =
  if k = stop then false
  else f (System.get_solution s k) || exists f s (k+1) stop

(* TODO: If a system have no solution, then there is no solution to the problem and
 we could cut early. *)
(** Take a collection of diophantine equations corresponding to the projection
    of the original problem on to each constant and the homogenous systems,
    solve them and combine them into solution to the original problems. *)
let solve_systems env (var_system, shape_systems) =
  Logs.debug (fun m -> m "@[Pure system: %a@]" System.pp var_system);
  Logs.debug (fun m -> m "@[Kind system: %a@]" Fmt.(vbox @@ list ~sep:cut System.pp) shape_systems);

  let var_sols = System.solve (fun _ -> false) var_system in

  let cut_shape nb_shapes x =
    let rec cut_aux i stop solution =
      if i < stop then
        let v = solution.(i) in
        v > 1 || cut_aux (i+1) stop solution
      else false
    in
    cut_aux 0 nb_shapes x
  in

  (* TODO: for each Kind we need to solve the pure system with only var.
      Then we filter it out here, we could tweak the solver to not have to do that.
      We initialise the solver by giving 1 to every position of firt_var, this way
      we garanty that the solution have a one there. *)
  (* TODO: here we should clean the solution, for all solution, we can
   check if two positions < nb_shapes receive a 1 but the two associated types
   are not compatible, then we should remove this solution *)
  let shapes_sols = List.map (fun (system: System.t) ->
    ( system,
      System.solve (cut_shape system.first_var) system
        |> Iter.filter (fun s ->
            exists (fun x -> x > 0) s 0 system.first_var)
        )
    ) shape_systems
  in
  Dioph2Sol.get_solutions env var_system var_sols shapes_sols

let solve env problems =
  match problems with
  | [] -> Iter.empty
  | _ ->
    make_systems env problems
    |> solve_systems env

