module Fmt = CCFormat
module Vec = CCVector

module type S = DiophantineIntf.S

module Make() = struct
  module Z = CCInt

  exception Inconsistent_lengths

  let[@inline] is_zero x : bool = Z.equal Z.zero x

  module Solution = struct
    type t = Z.t array

    let pp out (sol:t): unit =
      if Array.length sol=0 then Fmt.int out 0
      else Fmt.array ~sep:(Fmt.return "@ + ") Z.pp out sol
  end

  type solution = Solution.t

  (* implement section 6 of "An efficient Incremental Algorithm for Solving
     Systems of Linear Diophantine Equations", Contejean and Devie, 1994.
  *)
  module Homogeneous_system = struct
    type t = {
      eqns: Z.t array array;
      n_vars: int; (* length of each sub-array *)
    }

    let[@inline] n_vars t = t.n_vars
    let[@inline] len t = Array.length t.eqns

    let make eqns : t =
      if Array.length eqns = 0 then (
        {eqns; n_vars=0}
      ) else (
        let n_vars = Array.length eqns.(0) in
        if not (CCArray.for_all (fun a -> Array.length a = n_vars) eqns) then (
          raise Inconsistent_lengths;
        );
        {eqns;n_vars}
      )

    let[@inline] eqns e = e.eqns

    (** State used for computing solutions

        invariants:
        - element number [i] in the stack has exactly its [i] first elements
          frozen (in the [>_n] lexicographic ordering)
        - there are never more than [n_vars] elements in the stack.

        Consequently, we use one large array of size [n_vars ** 2],
        and a stack size, to represent in place the stack elements.
    *)
    type state = {
      eqns: t; (* system [a] *)
      cut: Z.t array -> bool;
      stack: Z.t array array; (* stack of vectors *)
      frozen: bool array array; (* frozen indices *)
      sums: Z.t array array; (* [i] -> [a · stack.(i)] *)
      tmp_frozen: bool array; (* temporary array for frozen elements *)
      mutable len: int; (* number of vectors in [stack] *)
      mutable solutions: solution list; (* solutions found so far *)
      mutable recycle: Z.t array Vec.vector; (* local allocator to re-use arrays *)
    }

    let[@inline] st_n (st:state) : int = st.eqns.n_vars

    (* recycle an array *)
    let[@inline] st_recycle (st:state) (v:Z.t array): unit = Vec.push st.recycle v

    (* allocate (or re-use) an array of size [n] *)
    let[@inline] st_alloc_z_arr (st:state) n : Z.t array =
      match Vec.pop st.recycle with
        | None -> Array.make n Z.zero
        | Some v -> v

    let[@inline] st_alloc_vec st = st_alloc_z_arr st (st_n st)
    let[@inline] st_alloc_sums st = st_alloc_z_arr st (len st.eqns)

    (* scalar product *)
    let[@inline] scalar_prod (s1:Z.t array) (s2:Z.t array) : Z.t =
      CCArray.fold2
        (fun s_i a_i x_i -> Z.(s_i + a_i * x_i))
        Z.zero s1 s2

    (* [s1 < s2] *)
    let lt_sol (s1:solution) (s2:solution) : bool =
      (* is at least one elt of [s1] strictly smaller than in [s2]? *)
      let lt_elt = ref false in
      try
        for i=0 to Array.length s2-1 do
          begin match Z.compare s1.(i) s2.(i) with
            | 0 -> ()
            | n when n<0 -> lt_elt := true
            | _ -> raise Exit (* impossible *)
          end
        done;
        !lt_elt
      with Exit ->
        false

    let[@inline] blit_full a ~into : unit = Array.blit a 0 into 0 (Array.length a)

    (* compute the value of each equation for vector [tup] *)
    let[@inline] compute_sum ?into (st:state) (s:solution) : Z.t array =
      let into = match into with
        | None -> st_alloc_z_arr st (Array.length st.eqns.eqns)
        | Some v -> v
      in
      Array.iteri (fun i eqn -> into.(i) <- scalar_prod s eqn) st.eqns.eqns;
      into

    let mk_state ~cut eqns : state =
      let n = eqns.n_vars in
      (* the initial stack is:
         [[1;0;0;0]; [0;1;0;0]; [0;0;1;0]; [0;0;0;1]].
         Its last element is the smallest one in the tuple order, since we are
         going to explore the graph in increasing order *)
      let stack =
        Array.init n
          (fun i -> Array.init n (fun j -> if i=j then Z.one else Z.zero))
      in
      let st = {
        eqns;
        stack;
        cut;
        len=n;
        sums=Array.init n (fun _ -> Array.make (len eqns) Z.zero);
        frozen=Array.init n (fun _ -> Array.make n false);
        tmp_frozen=Array.make n false;
        solutions=[];
        recycle=Vec.create();
      } in
      (* initialize [sums] *)
      Array.iteri (fun i vec -> st.sums.(i) <- compute_sum st vec) st.stack;
      (* initialize [frozen] *)
      Array.iteri (fun j froz -> for k=0 to j-1 do froz.(k) <- true done) st.frozen;
      (* return state *)
      st

    (* consider vector [t_i] and unit vector [e_j], does [a(t_i)·e_j < 0]?
       If [true], it means we should explore [t_i + e_j] as a promising direction.
       @param sum the vector [a(t_i)] *)
    let right_direction (st:state) (sum:Z.t array) (j:int) : bool =
      let prod = ref Z.zero in
       (* compute scalar product of [a(vec)] with [a(e_j)] to [sum] *)
       for k=0 to len st.eqns - 1 do
         prod := Z.(!prod + sum.(k) * st.eqns.eqns.(k).(j));
       done;
       (* right direction iff sum is negative *)
       Z.compare !prod Z.zero < 0

    (* is there a solution that is strictly smaller than [s]? *)
    let[@inline] is_subsumed_by_sol (st:state) (s:Z.t array) : bool =
      List.exists (fun sol -> lt_sol sol s) st.solutions

    let pp_vec = CCFormat.(within "[" "]" @@ hvbox @@ array ~sep:(return "@ ") Z.pp)
    let pp_bvec =
      let pp_bit out b = CCFormat.int out (if b then 1 else 0) in
      CCFormat.(within "[" "]" @@ hvbox @@ array ~sep:(return "@ ") pp_bit)

    let pp = CCFormat.(map eqns @@ within "(" ")" @@ vbox @@ array pp_vec)
    let pp_sol = pp_vec

    let log_real_ k =
      k (fun fmt ->
        Format.fprintf Format.std_formatter "@[<2>diophantine: ";
        Format.kfprintf
          (fun out -> Format.fprintf out "@]@.")
          Format.std_formatter fmt)

    (* enable printing of logs? *)
    let log_enabled = ref false

    let[@inline] log_ fmt = if !log_enabled then log_real_ fmt

    (* main solving algorithm *)
    let solve_main (st:state) (yield : solution -> unit) : unit =
      let n = st_n st in
      while st.len > 0 do
        st.len <- st.len - 1;
        let i = st.len in
        (* pop top element *)
        let vec = st.stack.(i) in
        let frozen = st.tmp_frozen in
        blit_full st.frozen.(i) ~into:frozen;
        let sums = st.sums.(i) in
        log_ (fun k->k "> @[<2>explore %a :frozen %a@ :sums %a@]"
            pp_vec vec pp_bvec frozen pp_vec sums);
        (* check if we got a solution *)
        if CCArray.for_all is_zero sums then (
          let sol = Array.copy vec in
          st.solutions <- sol :: st.solutions;
          log_ (fun k->k "! solution %a" pp_vec sol);
          yield sol;
        ) else (
          (* explore next states *)
          for j=0 to n - 1 do
            if not frozen.(j) &&
               right_direction st sums j &&
               not (is_subsumed_by_sol st vec)
            then (
              (* explore [vec + e_j] *)
              let new_vec = st_alloc_vec st in
              blit_full vec ~into:new_vec;
              new_vec.(j) <- Z.succ new_vec.(j);
              log_ (fun k->k "  new_vec %a (st.len %d)" pp_vec new_vec st.len);
              if st.cut new_vec then (
                (* cut branch *)
                st_recycle st new_vec;
              ) else (
                assert (st.len < n);
                (* push onto stack *)
                st.stack.(st.len) <- new_vec;
                blit_full frozen ~into:st.frozen.(st.len);
                log_ (fun k->k "  push %a :frozen %a"
                    pp_vec new_vec pp_bvec st.frozen.(st.len));
                let new_sum = st_alloc_sums st in
                st.sums.(st.len) <- compute_sum ~into:new_sum st new_vec;
                (* freeze [j] *)
                frozen.(j) <- true;
                (* push on stack *)
                st.len <- st.len + 1;
              )
            )
          done;
        );
        (* dispose of local resources *)
        st_recycle st sums;
        st_recycle st vec;
      done

    let default_cut _ = false

    (* call [f] on every solution, and return the final list *)
    let solve_inner ?(cut=default_cut) t ~f : _ list =
      if t.n_vars = 0 then (
        let sol = [||] in
        f sol;
        [sol]
      ) else (
        let st = mk_state ~cut t in
        solve_main st f;
        st.solutions
      )

    let solve ?cut (t:t) : solution Iter.t =
      fun yield ->
        let _ = solve_inner ?cut t ~f:yield in
        ()

    let[@inline] solve_l ?cut t = solve_inner ?cut t ~f:(fun _ -> ())
  end

  let solve ?cut arr =
    Homogeneous_system.solve ?cut (Homogeneous_system.make arr)

  let solve_l ?cut arr =
    Homogeneous_system.solve_l ?cut (Homogeneous_system.make arr)
end

