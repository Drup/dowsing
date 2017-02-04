(**************************************************************************)
(*                                                                        *)
(*     The CiME3 tool box for term rewriting                              *)
(*     Copyright (C) 2007                                                 *)
(*                                                                        *)
(*                                                                        *)
(*     Evelyne Contejean                                                  *)
(*     Pierre Courtieu                                                    *)
(*     Julien Forest                                                      *)
(*     Olivier Pons                                                       *)
(*     Xavier Urbain                                                      *)
(*                                                                        *)
(*     CNRS-LRI-Universite Paris Sud XI                                   *)
(*     Cedric-CNAM-ENSIIE                                                 *)
(*                                                                        *)
(*                                                                        *)
(*   This file is distributed under the terms of the CeCILL-C licence     *)
(*                                                                        *)
(**************************************************************************)


(**************************************************************************

   Resolution of systems of linear Diophantine equations
   see contejean94ic).

   $Id: dioph.ml 4 2004-10-19 14:04:20Z contejea $

 ***************************************************************************)

type elt =
  {
    mutable frozen : bool;
    mutable value : int
  }

type node =
  {
    mutable tuple   : elt array;
    default    : int array;
    mutable constr : int array list
  }

type stack =
  {
    hmax  : int;            (* maximal height *)
    mutable h : int;        (* current height *)
    mutable v : node array  (* content as a vector *)
  }

type state =
  {
    st : stack; (* the current stack *)
    mutable sol : int array list (* already found solutions *)
  }

let pp_elt fmt c =
  Format.fprintf fmt "%s(%d) "
    (if c.frozen then " F" else "NF")
    c.value

let pp_stack fmt stack =
  for i=0 to (pred stack.h) do
    Format.fprintf fmt
      "@[tuple =@ @[%a@],@ default =@ @[%a@], constraint =@ @[%a@]@]@."
      Fmt.(array pp_elt) stack.v.(i).tuple
      Fmt.(array int) stack.v.(i).default
      Fmt.(list ~sep:cut @@ array ~sep:sp int) stack.v.(i).constr
  done


let empty_stack n =
  let empty_node = {tuple = [||]; default = [||]; constr = []} in
  let vp = Array.make n empty_node in
  {hmax = n; h = 0; v = vp}

let push x p =
  if p.h < p.hmax
  then
    begin
      p.v.(p.h) <- x;
      p.h <- succ p.h
    end
  else failwith "Stack.push"


let pop p =
  if p.h > 0
  then
    begin
      p.h <- (pred p.h);
      p.v.(p.h)
    end
  else failwith "Stack.pop"

let height p = p.h

let do_stack f p =
  for i=0 to (pred p.h) do
    f p.v.(i)
  done


let pp_state fmt ~nb_eqs ~nb_vars ~v_type syst s =
  Fmt.pf fmt "@[ne = %d, nv = %d, v_type = %a@]@." nb_eqs nb_vars
    (fun fmt vt ->
       for i=0 to (pred (Array.length vt)) do
         Fmt.pf fmt "%d " vt.(i)
       done) v_type;
  Fmt.pf fmt "Input system =@ @[%a@]@]@."
    (fun fmt syst ->
       for i=0 to pred nb_eqs do
         for j=0 to pred nb_vars do
           Fmt.pf fmt "%d " syst.(i).(j)
         done;
         Fmt.pf fmt "@\n"
       done) syst;
  Fmt.pf fmt "Current stack =@ @[%a@]@." pp_stack s.st


let init_state nb_eq nb_var v_type system =

  (* initialization of the stack *)

  let p = empty_stack nb_var in
  for j=0 to (pred nb_var) do
    (* building the jth canonical vector *)
    let ej = Array.make nb_var {frozen = false; value = 0} in
    for i=0 to (pred nb_var) do
      ej.(i) <- {frozen = (i < j); value = if i=j then 1 else 0}
    done;

    (* freezing the components corresponding to other theories if the
       variable associated with the jth component is instanciated. *)
    if (v_type.(0) <= j)
    then
      begin
        ej.(j).frozen <- true;
        for i=1 to (pred (pred (Array.length v_type))) do
          if (v_type.(i) > j)
          then
            for k = v_type.(i) to (pred (v_type.(i+1))) do
              ej.(k).frozen <- true
            done
        done
      end;


    let d = Array.make nb_eq 0 in
    for i=0 to (pred nb_eq) do d.(i) <- system.(i).(j) done;

    let nj = {tuple = ej; default = d; constr = []} in
    push nj p

  done;

  {
    st = p;
    sol = [];
  }

let is_a_solution nb_eqs n =
  try
    for i = 0 to pred nb_eqs do
      if n.default.(i) <> 0
      then raise Exit
    done;
    true
  with Exit -> false

let extract_solution nb_vars n =
  let s = Array.make nb_vars 0 in
  for i=0 to pred nb_vars do
    s.(i) <- n.tuple.(i).value
  done;
  s

let add_constraint nb_vars solution my_state =
  do_stack
    (fun n ->
       let c = Array.make nb_vars 0 in
       for i = 0 to pred nb_vars do
         if solution.(i) > n.tuple.(i).value
         then c.(i) <- solution.(i) -  n.tuple.(i).value
       done;

       n.constr <- c :: n.constr;

       (* updating the frozen components *)
       let latitude = ref 0
       and indice_latitude = ref nb_vars in
       try
         for i = 0 to pred nb_vars do
           latitude := !latitude + c.(i);
           if !latitude > 1
           then raise Exit;
           if c.(i) > 0
           then indice_latitude := i
         done;
         if !latitude = 1
         then n.tuple.(!indice_latitude).frozen <- true
       with Exit -> ())
    my_state.st


let scalar_product v w =
  let p = ref 0 in
  for i=0 to pred (Array.length v) do
    p:= !p + (v.(i) * w.(i))
  done;
  !p


let increm_index_list nb_var v_type tab_defaut n =
  let ti = ref [] in
  for i = pred nb_var downto v_type.(0) do
    if (n.tuple.(i).frozen = false) &&
       (n.tuple.(i).value = 0) &&
       scalar_product n.default tab_defaut.(i) < 0
    then ti := i :: !ti
  done;

  for i = pred v_type.(0) downto 0 do
    if (n.tuple.(i).frozen = false) &&
       scalar_product n.default tab_defaut.(i) < 0
    then ti := i :: !ti
  done;
  !ti


let is_null c =
  try
    for i = 0 to pred (Array.length c) do
      if c.(i) <> 0
      then raise Exit
    done;
    true
  with Exit -> false


let copy_tuple v =
  let v' = Array.make (Array.length v) {frozen = true; value = 0} in
  for i=0 to pred (Array.length v) do
    v'.(i) <- {frozen = v.(i).frozen; value = v.(i).value}
  done;
  v'


let push_a_stack_elt nb_eqs _nb_vars v_type tab_default my_state n i =
  let n_copy =
    {
      tuple = copy_tuple n.tuple;
      default = Array.copy n.default;
      constr = [];
    } in
  (* updating the ith component: t -> t+ei *)
  n_copy.tuple.(i).value <- n_copy.tuple.(i).value + 1;

  (* updating the constraints *)
  let is_minimal = ref true in
  let rec update_constraint = function
    | [] -> []
    | c::l ->
      if !is_minimal
      then
        let c' = Array.copy c in
        begin
          if c'.(i) > 0
          then
            begin
              c'.(i) <- pred c'.(i);
              if is_null c'
              then
                begin
                  is_minimal := false;
                  []
                end
              else c' :: (update_constraint l)
            end
          else c' :: (update_constraint l)
        end
      else [] in
  let new_constr = update_constraint n.constr in
  if !is_minimal
  then
    begin
      n_copy.constr <- new_constr;

      (* freezing when the component is associated with a constant *)
      if v_type.(0) <= i
      then
        begin
          n_copy.tuple.(i).frozen <- true;
          for j = 1 to pred (pred (Array.length v_type)) do
            if v_type.(j) > i || v_type.(j+1) <= i
            then
              for k = v_type.(j) to pred v_type.(j+1) do
                n_copy.tuple.(k).frozen <- true
              done
          done
        end;

      (* updating the default *)
      for j=0 to pred nb_eqs do
        n_copy.default.(j) <- n_copy.default.(j) + tab_default.(i).(j)
      done;

      push n_copy my_state.st

    end



let process nb_eqs nb_vars v_type tab_default my_state =
  let current_elt = pop my_state.st in
  if is_a_solution nb_eqs current_elt
  then
    begin
      let s = extract_solution nb_vars current_elt in
      my_state.sol <- s :: (my_state.sol);
      add_constraint nb_vars s my_state
    end
  else
    List.iter
      (function i ->
         push_a_stack_elt nb_eqs nb_vars v_type tab_default
           my_state current_elt i;
         current_elt.tuple.(i).frozen <- true)
      (increm_index_list nb_vars v_type tab_default current_elt)


let solve ?(debug=false) v_type system =
  let nb_eqs = Array.length system
  and nb_vars = v_type.(pred (Array.length v_type)) in
  let tab_default = Array.make_matrix nb_vars nb_eqs 0 in
  for i = 0 to pred nb_eqs do
    for j = 0 to pred nb_vars do
      tab_default.(j).(i) <- system.(i).(j)
    done
  done;
  let my_state  = init_state nb_eqs nb_vars v_type system in
  while height my_state.st > 0 do
    process nb_eqs nb_vars v_type tab_default my_state ;
    if debug then
      pp_state Format.err_formatter ~nb_eqs ~nb_vars ~v_type system my_state
  done;
  Array.of_list my_state.sol
