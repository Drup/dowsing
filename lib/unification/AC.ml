module System : sig

  type t = {
    nb_atom : int ; (* Number of atoms in each equation *)
    assoc_pure : Pure.t array ; (* Map from indices to the associated pure terms *)
    first_var : int ; (* Constants are at the beginning of the array, Variables at the end. *)
    system : int array array ;
  }

  val[@warning "-32"] pp : t Fmt.t

  val simplify_problem : Env.t -> ACTerm.problem -> ACTerm.problem

  (* Storing the pure term with the system repeat information because in [assoc_pure],
    the first value contains it, but in the future we want to have heterogenous system
    meaning we may loose this information. *)
  val make : ACTerm.problem list -> t * (Pure.t * t) list

  type dioph_solution
  val get_solution : dioph_solution -> int -> int

  val solve : t -> dioph_solution Iter.t

  val pp_dioph : dioph_solution Fmt.t

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
    Fmt.(vbox (array ~sep:cut @@ array ~sep:(any ", ") int)) ppf system

  (* Replace variables by their representative/a constant *)
  let simplify_problem env {ACTerm. left ; right} =
    let f x = match x with
      | Pure.Constant _ | Pure.FrozenVar _ -> x
      | Pure.Var v ->
        match Env.representative env v with
        | V v' -> Var v'
        | E (_,Constr (p,[||])) -> Constant p
        | E _ -> x
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

  (* The number of constants and variables in a system *)
  let make_mapping problems =
    let vars = Variable.HMap.create 4 in
    let nb_vars = ref 0 in
    let consts = ref Pure.Set.empty in
    let nb_consts = ref 0 in
    let f = function
      | Pure.Var v ->
        if Variable.HMap.mem vars v then () else
          Variable.HMap.add vars v @@ CCRef.get_then_incr nb_vars
      | Constant _ | FrozenVar _ as c ->
        consts := Pure.Set.add c !consts
    in
    let aux {ACTerm. left ; right} =
      Array.iter f left ; Array.iter f right
    in
    List.iter aux problems ;
    vars, !nb_vars, Pure.Set.to_list !consts, !nb_consts

  let make problems : t * (Pure.t * t) list =
    let vars, nb_vars, consts, _nb_consts = make_mapping problems in
    let get_index const term =
      match term, const with
      | (Pure.Constant _ | FrozenVar _), None -> -1
      | Pure.Var v, Some _ -> Variable.HMap.find vars v + 1
      | Pure.Var v, None -> Variable.HMap.find vars v
      | _, Some c -> if Pure.equal term c then 0 else - 1
    in

    let gen const =
      let nb_consts = match const with Some _ -> 1 | None -> 0 in
      let nb_atom = nb_vars + nb_consts in
      let assoc_pure = Array.make nb_atom Pure.dummy in
      begin match const with
      | Some k -> assoc_pure.(0) <- k
      | None -> ()
      end;
      Variable.HMap.iter (fun k i -> assoc_pure.(i+nb_consts) <- Pure.var k) vars ;

      let first_var = nb_consts in
      let system =
        Iter.of_list problems
        |> Iter.map (add_problem (get_index const) nb_atom)
        |> Iter.to_array (* TO OPTIM *)
      in
      { nb_atom ; assoc_pure ; first_var ; system }
    in
    gen None, List.map (fun p -> p, gen (Some p)) consts

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

  let pp_dioph =
    Fmt.(vbox (array ~sep:(any ", ") int))
end

(** See section 6.2 and 6.3 *)
module Dioph2Sol : sig

  type t = (Variable.t * ACTerm.t) Iter.t

  val get_solutions :
    Env.t -> System.t -> System.dioph_solution Iter.t  -> (Pure.t * System.dioph_solution list) list -> t Iter.t

end = struct
  (** In the following, [i] is always the row/solution index and [j] is always
      the column/variable index. *)

  module Trace = Trace_core

  type t = (Variable.t * ACTerm.t) Iter.t

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

  let combine_const_sols const_sols =
    CCList.map_product_l (fun (c, l) -> List.map (fun x -> (c,x)) l) const_sols

  let iterate_subsets nb_columns const_sols len bitvars =
    let consts_parts = combine_const_sols const_sols |> Iter.of_list in
    Iter.flat_map (fun consts_part ->
      let const_vec =
        let bitv = ref Bitv.empty in
        for j = 0 to nb_columns - 1 do
          if List.exists (fun (_, sol) -> System.get_solution sol (j+1) <> 0 ) consts_part then
            bitv := Bitv.add !bitv j
        done;
        !bitv
      in
      Iter.map (fun x -> (consts_part, x))
        (Hullot.Default.iter ~len
          ~small:(fun _ -> true)
          ~large:(large_enough const_vec bitvars))
    ) consts_parts

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
  let make_term buffer l : ACTerm.t =
    CCVector.clear buffer;
    let f (n, symb) =
      for _ = 1 to n do CCVector.push buffer symb done
    in
    List.iter f l;
    ACTerm.make @@ CCVector.to_array buffer

  let unifier_of_subset vars solutions symbols (const_sols, pure_sols) =
    (* TODO: vars should be only variable therefore the type should
       change removing the need for the match *)
    assert (CCVector.length solutions = Array.length symbols);

    (* Unifier is a map from variable to a list of pure terms presenting the
       the tuple associated with the variable *)
    let unifiers = Variable.HMap.create (Array.length vars) in
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
          | Pure.Constant _ | Pure.FrozenVar _ -> ()
          | Pure.Var var ->
            let multiplicity = System.get_solution sol j in
            Variable.HMap.add_list unifiers var (multiplicity, symb)
        done;
    done;
    (* log (fun m -> m "Unif: %a@." Fmt.(iter_bindings ~sep:(unit" | ") Variable.HMap.iter @@ *)
    (*    pair ~sep:(unit" -> ") Variable.pp @@ list ~sep:(unit",@ ") @@ pair int Pure.pp ) unifiers *)
    (* ) ; *)

    (* We add the constant comming from const_sols *)
    List.iter (fun (constant, sol) ->
      for j = 0 to Array.length vars - 1 do
        assert (System.get_solution sol 0 = 1);
        match vars.(j) with
        | Pure.Constant _ | Pure.FrozenVar _ -> ()
        | Pure.Var var ->
            (* We have a shift of 1 because the first entry is for the constant (TODO: change it once using heterogenous system) *)
            let multiplicity = System.get_solution sol (j+1) in
            Variable.HMap.add_list unifiers var (multiplicity, constant)
      done
    ) const_sols;

    let buffer = CCVector.create_with ~capacity:10 Pure.dummy in
    fun k ->
      Variable.HMap.iter
        (fun key l ->
           let pure_term = make_term buffer l in
           k (key, pure_term))
        unifiers

  (** Combine everything *)
  let get_solutions env
      {System. nb_atom; assoc_pure;_}
      pure_solutions const_sols k =
    let stack_solutions = CCVector.create () in
    let bitvars = extract_solutions stack_solutions nb_atom pure_solutions in
    (* Logs.debug (fun m -> m "@[Bitvars: %a@]@." (Fmt.Dump.array Bitv.pp) bitvars);
    Logs.debug (fun m -> m "@[<v2>Sol stack:@ %a@]@."
      (CCVector.pp System.pp_solution) stack_solutions); *)
    let symbols = Array.init (CCVector.length stack_solutions) (fun _ -> Pure.var (Env.gen env)) in
    (* Logs.debug (fun m -> m "@[Symbols: %a@]@." (Fmt.Dump.array @@ Pure.pp) symbols); *)
    let subsets = iterate_subsets nb_atom const_sols (Array.length symbols) bitvars in (* TODO need change here *)
    let n_solutions = ref 0 in
    Iter.map
      (unifier_of_subset assoc_pure stack_solutions symbols)
      subsets (fun x -> incr n_solutions; k x);
    Trace.message ~data:(fun () -> [("n", `Int !n_solutions)] )"Number of AC solutions"
end

(** [make_systems env problems] take a list of AC problem. Which mean problem
    of the form [t_1 * t_2 * t_4 = t_5 * t_6] and return a colection of
    systems of diophantine equations [homogenous_system * heterogenous_systems]
    where [homogenous_system] is a homogenous system of diophantine equations
    corresponding to the projection of [problems] with no constant.
    [heterogenous_systems] is a list of heterogenous systems together with the
    pure term associated to it which correspond to the projection of [problems]
    on this constant. *)
let make_systems env problems : System.t * (Pure.t * System.t) list =
  System.make @@ List.map (System.simplify_problem env) problems

(** Take a collection of diophantine equations corresponding to the projection
    of the original problem on to each constant and the homogenous systems,
    solve them and combine them into solution to the original problems. *)
let solve_systems env (pure_system, const_systems) =
  Logs.debug (fun m -> m "@[Pure system: %a@]" System.pp pure_system);
  Logs.debug (fun m -> m "@[Const system: %a@]" Fmt.(vbox @@ list ~sep:cut (pair Pure.pp System.pp)) const_systems);
  let const_sols = List.map (fun (p, system) ->
    (p, System.solve system
        |> Iter.filter (fun s -> System.get_solution s 0 = 1)
        |> Iter.to_list)) const_systems
  in
  let pure_sols = System.solve pure_system in
  Logs.debug (fun m -> m "@[Const solution: %a@]" Fmt.Dump.(list @@ pair Pure.pp (list System.pp_dioph)) const_sols);
  Dioph2Sol.get_solutions env pure_system pure_sols const_sols

let solve env problems =
  match problems with
  | [] -> Iter.empty
  | _ ->
    make_systems env problems
    |> solve_systems env

