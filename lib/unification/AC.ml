module System : sig

  type t = {
    nb_atom : int ; (* Number of atoms in each equation *)
    assoc_pure : Pure.t array ; (* Map from indices to the associated pure terms *)
    first_var : int ; (* Constants are at the beginning of the array, Variables at the end. *)
    system : int array array ;
  }

  val[@warning "-32"] pp : t Fmt.t

  val simplify_problem : Env.t -> ACTerm.problem -> ACTerm.problem

  val make : ACTerm.problem list -> t

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
  let simplify_problem env {ACTerm. left ; right} =
    let f x = match x with
      | Pure.Constant _ -> x
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
    let aux {ACTerm. left ; right} =
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

  type t = Bitv.t * ACTerm.t Variable.HMap.t

  val[@warning "-32"] pp : Env.t -> t Fmt.t

  val get_solutions :
    Env.t -> System.t -> System.dioph_solution Iter.t ->
    t Iter.t

end = struct
  (** In the following, [i] is always the row/solution index and [j] is always
      the column/variable index. *)

  type t = Bitv.t * ACTerm.t Variable.HMap.t

  let pp env ppf (subset, unif) =
    let namefmt = Env.var_names env in
    let pp_pair ppf (v,t) =
      Fmt.pf ppf "@[%a → %a@]" (Variable.pp namefmt) v (ACTerm.pp namefmt) t
    in
    Fmt.pf ppf "@[<v2>%a: {@ %a@]@ }"
      Bitv.pp subset
      (Fmt.iter_bindings ~sep:Fmt.cut
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
  let make_term buffer l : ACTerm.t =
    CCVector.clear buffer;
    let f (n, symb) =
      for _ = 1 to n do CCVector.push buffer symb done
    in
    List.iter f l;
    ACTerm.make @@ CCVector.to_array buffer

  let unifier_of_subset vars solutions symbols subset =
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
      (seq_solutions:System.dioph_solution Iter.t) : _ Iter.t =
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

let make_system env problems : System.t =
  System.make @@ List.map (System.simplify_problem env) problems

let solve_system env system =
  let dioph_sols = System.solve system in
  Dioph2Sol.get_solutions env system dioph_sols

let solve env problems =
  match problems with
  | [] -> Iter.empty
  | _ ->
    make_system env problems
    |> solve_system env
