(* Shape *)
module Shape = struct
  (** The shape of a type is:
    - [Constant] constants or frozen variables
    - [Var] if it is a non fronzen variable
    - [Free] if the top symbols is a free symbols, arrow or tuples. *)
  type t = Const of Pure.t | Var of Variable.t | Free of Variable.t * Type.Kind'.t * Type.t

  let rec _is_const : Type.t -> bool = function
    | Type.Var _ -> false
    | Type.FrozenVar _ | Type.Other _ -> true
    | Type.Constr (_, params) -> Array.for_all _is_const params
    | Type.Tuple ts -> Type.NSet.fold (fun t b -> b && _is_const t) ts true
    | Type.Arrow (params, ret) ->
        Type.NSet.fold (fun t b -> b && _is_const t) params (_is_const ret)

  (** Given a Pure.t return the shape of the representative *)
  let of_pure_repr env = function
    | Pure.Var v -> (
        match Env.representative env v with
        | V v -> Var v

        (* TODO: check if those 3 cases are meaningful. Other should be
           considered as a constant but is not in Pure yet.*)
        | E (_, (Var _ | Tuple _ | Other _)) -> failwith "Wrong assumption on pure AC system."
        | E (_, FrozenVar fv) -> Const (Pure.FrozenVar fv)
        | E (_, Constr (c, [||])) -> Const (Constant c)
        | E (v, ((Constr _ | Arrow _) as t)) ->
            Free (v, Type.kind' t, t)
    )
    | (Constant _ | FrozenVar _) as c ->
        Const c

  let is_compatible env p k =
    match of_pure_repr env p with
    | Const _ | Var _ -> false
    | Free (_, k', _) -> Type.Kind'.equal k' k

end

module System : sig
  type t = {
    nb_atom : int ; (* Number of atoms in each equation *)
    (* TODO: remove assoc_pure, does not seems useful *)
    assoc_pure : Pure.t array ; (* Map from indices to the associated pure terms *)
    first_var : int ; (* Constants are at the beginning of the array, Variables at the end. *)
    system : int array array ;
  }

  val[@warning "-32"] pp : t Fmt.t

  val simplify_problem : Env.t -> ACTerm.problem -> ACTerm.problem

  (* Storing the pure term with the system repeat information because in [assoc_pure],
     the first value contains it, but in the future we want to have heterogenous system
     meaning we may loose this information. *)
  val make : Env.t -> ACTerm.problem list -> t * (Pure.t * t) list * t List.t

  type dioph_solution
  val get_solution : dioph_solution -> int -> int

  val solve : (int array -> bool) -> t -> dioph_solution Iter.t

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

  let partition_frees frees =
    Type.Kind'.Map.map (fun (n, l) ->
      ( n,
      List.fold_left
        (fun (c, m) (v, t) ->
          if Type.Map.mem t m then (c, m)
          else (c+1, Type.Map.add t (c, v) m)
        )
        (0, Type.Map.empty) l )
        |> snd
      ) frees

  (* The number of constants and variables in a system *)
  let make_mapping env problems =
    let vars = Variable.HMap.create 4 in
    let frees = ref Type.Kind'.Map.empty in
    let nb_vars = ref 0 in
    let consts = ref Pure.Set.empty in
    let f p = match Shape.of_pure_repr env p with
      | Shape.Const c -> consts := Pure.Set.add c !consts
      | Shape.Var v ->
          if Variable.HMap.mem vars v then ()
          else Variable.HMap.add vars v @@ CCRef.get_then_incr nb_vars
      | Shape.Free (v, s, t) -> frees := Type.Kind'.Map.update s (function None -> Some (1, [v, t]) | Some (n, l) -> Some (n+1, (v, t)::l)) !frees
    in
    let aux {ACTerm. left ; right} =
      Array.iter f left ; Array.iter f right
    in
    List.iter aux problems ;
    (* Partition the term with a free symbol into equivalence classes *)
    let free_classes = partition_frees !frees in
    (vars, !nb_vars, free_classes, Pure.Set.to_list !consts)

  let get_compatibles env free_classes c =
    let acc = ref 0 in
    let m = Type.Kind'.Map.filter_map (fun k (n, tm) ->
      if Shape.is_compatible env c k then (
        acc := !acc + n;
        Some (!acc - n, tm)
      ) else None) free_classes
    in
    !acc, m

  (*NOTE: Current implementation.
    A simple type is either a constant, a varialbe or a frozen variable.
     - We create a system for each constants and frozen variable
     - We create a system only for variables
     - For each equivalent class on non simple type (Define by the Kind' currently)
       we create a system for each.
       In this system, each type appearing is consider to be equal only to it self.

    Possible improvements:
      - We can increase the set of constant type, be including non simple constant type
      - In each equivalence class, we can look for term that are equal up to the current substitution
      TODO: is it true that equal type up to iso in the current substitution will be equal here?
       *)
  let make env problems : t * (Pure.t * t) list * t List.t =
    let vars, nb_vars, free_classes, consts = make_mapping env problems in
    let get_index_const (nb_frees, compatible_frees) c =
      fun p -> match Shape.of_pure_repr env p with
          | Shape.Var v -> Variable.HMap.find vars v + nb_frees + 1
          | Shape.Free (_, k, t) -> begin
              match Type.Kind'.Map.get k compatible_frees with
              | None -> -1
              | Some (nk, tm) ->
              let i, _ = Type.Map.find t tm in
              nk + i + 1
            end
          | Shape.Const t -> if Pure.equal t c then 0 else -1
    in
    let get_index_var p =
      match Shape.of_pure_repr env p with
      | Shape.Var v -> Variable.HMap.find vars v
      | _ -> -1
    in
    let get_index_kind k =
      let nb_frees, frees_map = Type.Kind'.Map.find k free_classes in
      fun p -> match Shape.of_pure_repr env p with
      | Shape.Const _ -> -1
      | Shape.Var v -> Variable.HMap.find vars v + nb_frees
      | Shape.Free (_, k', t) ->
          if Type.Kind'.equal k k' then fst (Type.Map.find t frees_map)
          else -1
    in

    let gen_const c =
      let nb_atom = nb_vars + 1 in
      let assoc_pure = Array.make nb_atom Pure.dummy in
      assoc_pure.(0) <- c;
      Variable.HMap.iter (fun k i -> assoc_pure.(i + 1) <- Pure.var k) vars ;

      let first_var = 1 in
      let system =
        List.map (add_problem (get_index_const (get_compatibles env free_classes c) c) nb_atom) problems
        |> Array.of_list
      in
      { nb_atom ; assoc_pure ; first_var ; system }
    in
    let const_systems = List.map (fun p -> p, gen_const p) consts in
    
    let var_system =
      let nb_atom = nb_vars in
      let assoc_pure = Array.make nb_atom Pure.dummy in
      Variable.HMap.iter (fun k i -> assoc_pure.(i) <- Pure.var k) vars ;
      let first_var = 0 in
      let system =
        List.map (add_problem (get_index_var) nb_atom) problems
        |> Array.of_list
      in
      { nb_atom ; assoc_pure ; first_var ; system }
    in

    let gen_kind k nb_frees types_map =
      let nb_atom = nb_vars + nb_frees in
      let assoc_pure = Array.make nb_atom Pure.dummy in
      Type.Map.iter (fun _t (i, v) -> assoc_pure.(i) <- Var v) types_map;
      Variable.HMap.iter (fun k i -> assoc_pure.(i + nb_frees) <- Pure.var k) vars ;

      let first_var = nb_frees in
      let system =
        List.map (add_problem (get_index_kind k) nb_atom) problems
        |> Array.of_list
      in
      { nb_atom ; assoc_pure ; first_var ; system }
    in
    let kind_systems = List.map
            (fun (k, (n, tm)) -> gen_kind k n tm)
            @@ Type.Kind'.Map.to_list free_classes
    in
    var_system, const_systems, kind_systems

  type dioph_solution = int array
  let get_solution = Array.get

  let solve cut { system ; _ } : dioph_solution Iter.t =
    Solver.solve ~cut @@ Solver.make system

  let pp_dioph =
    Fmt.(vbox (array ~sep:(any ", ") int))
end

(** See section 6.2 and 6.3 *)
module Dioph2Sol : sig

  type t = (Variable.t * ACTerm.t) Iter.t

  val get_solutions :
    Env.t -> System.t -> System.dioph_solution Iter.t
        -> (Pure.t * System.dioph_solution list) list
        -> (System.t * System.dioph_solution Iter.t) list -> t Iter.t

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

  let small_enough first_var bitvars bitset =
    let f _ col = Bitv.(is_singleton_or_empty (bitset && col)) in
    for_all2_range f bitvars 0 first_var

  let large_enough_kind first_var bitvars subset =
    let f _ col = Bitv.do_intersect col subset in
    for_all2_range f bitvars 0 first_var

  let combine_const_sols const_sols =
    CCList.map_product_l (fun (c, l) -> List.map (fun x -> (c,x)) l) const_sols

  let combine_kind_sols kind_sols =
    CCList.map_product_l (fun (nb_kinds, symbols, solutions, bitvars, assoc_pure, sols) ->
      List.map (fun x -> (nb_kinds, symbols, solutions, bitvars, assoc_pure, x)) (Iter.to_list sols))
    kind_sols

  let iterate_kind_subsets len system bitvars =
    Hullot.Default.iter ~len
      ~small:(small_enough system.System.first_var bitvars)
      ~large:(large_enough_kind system.first_var bitvars)

  let prod_iter i1 i2 =
    Iter.flat_map (fun x -> Iter.map (fun y -> (x,y)) i2) i1

  let iterate_subsets nb_columns const_sols kind_combined_sols len bitvars =
    let consts_parts = combine_const_sols const_sols |> Iter.of_list in
    let kinds_parts = combine_kind_sols kind_combined_sols |> Iter.of_list in
    let fixed_parts = prod_iter consts_parts kinds_parts in
    Iter.flat_map (fun (consts_part, kinds_part) ->
      let const_vec =
        let bitv = ref Bitv.empty in
        for j = 0 to nb_columns - 1 do
          if List.exists (fun (_, sol) -> System.get_solution sol (j+1) <> 0 ) consts_part then
            bitv := Bitv.add !bitv j
        done;
        !bitv
      in
      let kind_vec = 
        let bitv = ref Bitv.empty in
        List.iter (fun (nb_kinds, symbols, _solutions, bitvars, _assoc_pure, kind_sols) ->
          for i = 0 to nb_columns - 1 do
            for j = 0 to Array.length symbols - 1 do
              if Bitv.mem j kind_sols && Bitv.mem j bitvars.(i + nb_kinds) then
                bitv := Bitv.add !bitv i
            done
          done
        ) kinds_part;
        !bitv
      in
      Iter.map (fun x -> (consts_part, kinds_part, x))
        (Hullot.Default.iter ~len
          ~small:(fun _ -> true)
          ~large:(large_enough Bitv.(const_vec || kind_vec) bitvars))
    ) fixed_parts

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

  let unifier_of_subset vars solutions symbols (const_sols, kind_sols, pure_sols) =
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

    (* We add the variable comming from kind_sols *)
    List.iter (fun (_nb_kinds, symbols, solutions, _bitvars, assoc_pure, kind_sol) ->
      let solutions = CCVector.unsafe_get_array solutions in
      for i = 0 to Array.length symbols - 1 do
        if Bitv.mem i kind_sol then
          let sol = solutions.(i) in
          let symb = symbols.(i) in
          Array.iteri (fun j v -> match v with
          | Pure.Constant _ | FrozenVar _ -> ()
          | Var var ->
              let multiplicity = System.get_solution sol j in
              Variable.HMap.add_list unifiers var (multiplicity, symb)
          ) assoc_pure
      done
    ) kind_sols;

    let buffer = CCVector.create_with ~capacity:10 Pure.dummy in
    fun k ->
      Variable.HMap.iter
        (fun key l ->
           let pure_term = make_term buffer l in
           k (key, pure_term))
        unifiers

  let get_kind_solutions env (({System. nb_atom; first_var; assoc_pure; _} as system), solutions) =
    let stack_solution = CCVector.create () in
    let bitvars = extract_solutions stack_solution nb_atom solutions in
    let symbols = Array.init (CCVector.length stack_solution) (fun _ -> Pure.var (Env.gen env)) in
    first_var, symbols, stack_solution, bitvars, assoc_pure, iterate_kind_subsets nb_atom system bitvars

  (** Combine everything *)
  let get_solutions env
      {System. nb_atom; assoc_pure;_}
      pure_solutions const_sols kind_sols k =
    let stack_solutions = CCVector.create () in
    let bitvars = extract_solutions stack_solutions nb_atom pure_solutions in
    (* Logs.debug (fun m -> m "@[Bitvars: %a@]@." (Fmt.Dump.array Bitv.pp) bitvars);
    Logs.debug (fun m -> m "@[<v2>Sol stack:@ %a@]@."
      (CCVector.pp System.pp_solution) stack_solutions); *)
    let symbols = Array.init (CCVector.length stack_solutions) (fun _ -> Pure.var (Env.gen env)) in
    (* Logs.debug (fun m -> m "@[Symbols: %a@]@." (Fmt.Dump.array @@ Pure.pp) symbols); *)
    let kind_combined_sols = List.map (get_kind_solutions env) kind_sols in
    let subsets = iterate_subsets nb_atom const_sols kind_combined_sols
                    (Array.length symbols) bitvars in (* TODO need change here *)
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
let make_systems env problems : System.t * (Pure.t * System.t) list * _ =
  System.make env @@ List.map (System.simplify_problem env) problems

let rec exists f s k stop : bool =
  if k = stop then false
  else f (System.get_solution s k) || exists f s (k+1) stop

(** Take a collection of diophantine equations corresponding to the projection
    of the original problem on to each constant and the homogenous systems,
    solve them and combine them into solution to the original problems. *)
let solve_systems env (var_system, const_systems, kind_systems) =
  Logs.debug (fun m -> m "@[Pure system: %a@]" System.pp var_system);
  Logs.debug (fun m -> m "@[Const system: %a@]" Fmt.(vbox @@ list ~sep:cut (pair Pure.pp System.pp)) const_systems);
  let cut_const solution =
    solution.(1) > 1
  in

  let const_sols = List.map (fun (p, system) ->
    (p, System.solve cut_const system
        |> Iter.filter (fun s -> System.get_solution s 0 = 1)
        |> Iter.to_list)) const_systems
  in
  let var_sols = System.solve (fun _ -> false) var_system in

  let cut_kind nb_kinds x =
    let rec cut_aux i stop solution =
      if i < stop then
        let v = solution.(i) in
        v > 1 || cut_aux (i+1) stop solution
      else false
    in
    cut_aux 0 nb_kinds x
  in

  (* TODO: for each Kind we need to solve the pure system with only var.
      Then we filter it out here, we could tweak the solver to not have to do that.
      We initialise the solver by giving 1 to every position of firt_var, this way
      we garanty that the solution have a one there. *)
  let kind_sols = List.map (fun (system: System.t) ->
    ( system,
      System.solve (cut_kind system.first_var) system
        |> Iter.filter (fun s ->
            exists (fun x -> x > 0) s 0 system.first_var))
    ) kind_systems
  in
  Logs.debug (fun m -> m "@[Const solution: %a@]" Fmt.Dump.(list @@ pair Pure.pp (list System.pp_dioph)) const_sols);
  Dioph2Sol.get_solutions env var_system var_sols const_sols kind_sols

let solve env problems =
  match problems with
  | [] -> Iter.empty
  | _ ->
    make_systems env problems
    |> solve_systems env

