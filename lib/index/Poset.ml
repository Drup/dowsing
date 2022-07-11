open Graph
module Logs = (val Logs.(src_log @@ Src.create __MODULE__))

let _info = Logs.info
let debug = Logs.debug

module G = Imperative.Digraph.ConcreteBidirectional (TypeId)
module Edge_set = Set.Make (G.E)
module Components = Graph.Components.Undirected (G)

type top_info = { is_only_top : bool  (** Is the only top of its cc *) }

type t = {
  env : Type.Env.t;
  graph : G.t;
  mutable tops : top_info TypeId.Map.t;
  mutable bottoms : TypeId.Set.t;
  mutable classes : TypeId.Set.t TypeId.Map.t;
}

type poset = t

(*Printers*)

let pp_vertex fmt e = Fmt.box TypeId.pp fmt e
let pp_edge fmt (s, d) = Fmt.pf fmt "@[%a ==> %a@]" TypeId.pp s TypeId.pp d

let pp fmt { graph; _ } =
  if G.nb_vertex graph = 0 then Format.fprintf fmt "empty"
  else
    Format.fprintf fmt "@[<v 2>Vertices: %a@]@.@[<v2>Edges: %a@]@."
      (Fmt.iter G.iter_vertex pp_vertex)
      graph
      (Fmt.iter G.iter_edges_e pp_edge)
      graph

module Unif_state = struct
  type t = Unknown | False | True of Subst.t
end

module D = Graphviz.Dot (struct
  type t = poset * TypeId.Range.t * Subst.t TypeId.Map.t

  module V = struct
    type t = TypeId.t * Unif_state.t * TypeId.Set.t
  end

  module E = struct
    type t = V.t * V.t

    let src = fst
    let dst = snd
  end

  let annotate_vertex (g, range, unifs) v =
    let matching_equiv_class =
      try TypeId.Map.find v g.classes with Not_found -> TypeId.Set.empty
    in
    match (TypeId.Map.find_opt v unifs, TypeId.check v range) with
    | None, true -> (v, Unif_state.Unknown, matching_equiv_class)
    | None, false -> (v, Unif_state.False, matching_equiv_class)
    | Some sub, _ -> (v, Unif_state.True sub, matching_equiv_class)

  let iter_vertex f (g, range, unifs) =
    let f' v = f @@ annotate_vertex (g, range, unifs) v in
    G.iter_vertex f' g.graph

  let iter_edges_e f (g, range, unifs) =
    let f' (src, dst) =
      f
      @@ ( annotate_vertex (g, range, unifs) src,
           annotate_vertex (g, range, unifs) dst )
    in
    G.iter_edges_e f' g.graph

  let graph_attributes _ = []
  let default_vertex_attributes _ = []
  let vertex_name (v, _, _) = "\"" ^ Fmt.to_to_string pp_vertex v ^ "\""

  let vertex_attributes (v, state, matching_equiv_class) =
    let v_name = Fmt.str "%a@." pp_vertex v in
    let label =
      TypeId.Set.fold
        (fun v str -> str ^ Fmt.str "%a@." pp_vertex v)
        matching_equiv_class ""
    in
    let color_attr, font_color, lbl =
      match state with
      | Unif_state.Unknown ->
          (`Color 0xFFAF00, `Fontcolor 0x000000, v_name ^ label)
      | Unif_state.False ->
          (`Color 0xc60013, `Fontcolor 0xFFFFFF, v_name ^ label)
      | Unif_state.True u ->
          let str = Fmt.str "%a @." Subst.pp u in
          ( `Color 0x007500,
            `Fontcolor 0xFFFFFF,
            v_name ^ label ^ "Unif : " ^ str )
    in
    [ color_attr; font_color; `Shape `Box; `Style `Filled; `Label lbl ]

  let get_subgraph _ = None
  let default_edge_attributes _ = []
  let edge_attributes _ = []
end)

let xdot ?(range = TypeId.Range.empty) ?(unifs = TypeId.Map.empty) e =
  let s = Filename.temp_file "dowsing_" ".dot" in
  let fmt = Format.formatter_of_out_channel @@ open_out s in
  Fmt.pf fmt "%a@." D.fprint_graph (e, range, unifs);
  let _ = Unix.system @@ Fmt.str "xdot %s" @@ Filename.quote s in
  ()

(*Initialization and transformations*)

let init env =
  let g = G.create () in
  let tops = TypeId.Map.empty in
  let bottoms = TypeId.Set.empty in
  let classes = TypeId.Map.empty in
  { env; graph = g; tops; bottoms; classes }

let size poset = G.nb_vertex poset.graph
let connectivity poset = G.nb_edges poset.graph

let clear poset =
  poset.tops <- TypeId.Map.empty;
  poset.bottoms <- TypeId.Set.empty;
  G.clear poset.graph

module Changes = struct
  type handle_edges = {
    mutable remove_edges : Edge_set.t;
    mutable lower_bounds : TypeId.Set.t;
    mutable upper_bounds : TypeId.Set.t;
  }

  type t =
    | Add_to_class of TypeId.t * TypeId.t
    | Add_as_new_class of handle_edges

  let empty () =
    {
      upper_bounds = TypeId.Set.empty;
      remove_edges = Edge_set.empty;
      lower_bounds = TypeId.Set.empty;
    }

  let add_to_class new_type repr classes =
    let old_set =
      match TypeId.Map.get repr classes with
      | Some s -> s
      | None -> TypeId.Set.empty
    in
    let new_set = TypeId.Set.add new_type old_set in
    TypeId.Map.add repr new_set classes

  let rm_and_add set (prev_v, new_v) =
    match prev_v with
    | None -> TypeId.Set.add new_v set
    | Some ty -> set |> TypeId.Set.remove ty |> TypeId.Set.add new_v

  let add_upper_bound ch (previous_top, new_top) =
    ch.upper_bounds <- rm_and_add ch.upper_bounds (previous_top, new_top)

  let add_lower_bound ch (new_bot, previous_bot) =
    ch.lower_bounds <- rm_and_add ch.lower_bounds (previous_bot, new_bot)

  let remove_edge ch dir (prev, current) =
    match prev with
    | None -> ()
    | Some ty ->
        let edge =
          match dir with `down -> (ty, current) | `up -> (current, ty)
        in
        ch.remove_edges <- Edge_set.add edge ch.remove_edges

  let apply poset changes vertex_0 =
    match changes with
    | Add_to_class (ty, repr) ->
        poset.classes <- add_to_class ty repr poset.classes
    | Add_as_new_class ch ->
        G.add_vertex poset.graph vertex_0;
        Edge_set.iter
          (fun edge ->
            debug (fun m -> m "Remove Edge %a @," pp_edge edge);
            G.remove_edge_e poset.graph edge)
          ch.remove_edges;
        TypeId.Set.iter
          (fun dst ->
            let edge = G.E.create vertex_0 () dst in
            debug (fun m -> m "Add Edge %a @," pp_edge edge);
            poset.tops <- TypeId.Map.remove dst poset.tops;
            G.add_edge_e poset.graph edge)
          ch.lower_bounds;
        TypeId.Set.iter
          (fun src ->
            let edge = G.E.create src () vertex_0 in
            debug (fun m -> m "Add Edge %a @," pp_edge edge);
            poset.bottoms <- TypeId.Set.remove src poset.bottoms;
            G.add_edge_e poset.graph edge)
          ch.upper_bounds;
        if TypeId.Set.is_empty ch.upper_bounds then
          poset.tops <-
            TypeId.Map.add vertex_0 { is_only_top = false } poset.tops;
        if TypeId.Set.is_empty ch.lower_bounds then
          poset.bottoms <- TypeId.Set.add vertex_0 poset.bottoms;
        ()
end

(** Annotate tops which are the only tops in a CC *)
let annotate_top poset =
  let comps = Components.components_list poset.graph in
  let tops_in_comps =
    List.map
      (fun l -> List.filter (fun v -> TypeId.Map.mem v poset.tops) l)
      comps
  in
  let new_tops =
    List.fold_left
      (fun tops tops_in_cc ->
        match tops_in_cc with
        | [ v ] -> TypeId.Map.add v { is_only_top = true } tops
        | _ -> tops)
      poset.tops tops_in_comps
  in
  poset.tops <- new_tops;
  ()

(*Adding in poset*)

exception Same_matching_class of G.V.t * G.V.t
exception Same_type of G.V.t

let add ?(with_feat = true) ({ env; graph; tops; bottoms; _ } as poset) vertex_0
    =
  let ty_0 = TypeId.ty vertex_0 in
  let ch = Changes.empty () in
  let already_seen_0 = TypeId.Set.empty in
  let to_visit : (_ * TypeId.t option * TypeId.t) Queue.t = Queue.create () in
  let bigger = ref 0 and smaller = ref 0 and uncomparable = ref 0 in
  let compare =
    if with_feat then MatchFeat.compare else Acic.compare ~hint:Unsure
  in
  let rec visit_down already_seen ~prev ~current =
    debug (fun m ->
        m "Visiting Edge down %a → %a@,"
          (Fmt.option ~none:(Fmt.any "⊤") pp_vertex)
          prev pp_vertex current);
    let comp = compare env (TypeId.ty current) ty_0 in
    debug (fun m -> m "%a@," Acic.pp_ord comp);
    match comp with
    | Matching_equiv ->
        if Type.equal ty_0 (TypeId.ty current) then raise (Same_type vertex_0)
        else raise (Same_matching_class (vertex_0, current))
    | Bigger ->
        incr bigger;
        let l = G.succ graph current in
        List.iter
          (fun next -> Queue.push (`down, Some current, next) to_visit)
          l;
        Changes.add_upper_bound ch (prev, current);
        visit_next already_seen
    | Uncomparable ->
        incr uncomparable;
        visit_next already_seen
    | Smaller ->
        incr smaller;
        Changes.remove_edge ch `down (prev, current);
        let already_seen = TypeId.Set.remove current already_seen in
        visit_next already_seen
  and visit_up already_seen ~prev ~current =
    debug (fun m ->
        m "Visiting Edge up %a → %a@,"
          (Fmt.option ~none:(Fmt.any "⊥") pp_vertex)
          prev pp_vertex current);
    let comp = compare env (TypeId.ty current) ty_0 in
    debug (fun m -> m "%a@," Acic.pp_ord comp);
    match comp with
    | Matching_equiv -> raise (Same_matching_class (vertex_0, current))
    | Bigger ->
        incr bigger;
        visit_next already_seen
    | Uncomparable ->
        incr uncomparable;
        visit_next already_seen
    | Smaller ->
        incr smaller;
        let l = G.pred graph current in
        List.iter (fun next -> Queue.push (`up, Some current, next) to_visit) l;
        Changes.add_lower_bound ch (current, prev);
        visit_next already_seen
  and visit_next already_seen =
    match Queue.take_opt to_visit with
    | None -> already_seen
    | Some (dir, prev, current) ->
        if TypeId.Set.mem current already_seen then (
          debug (fun m -> m "Already visited node %a@," TypeId.pp current);
          visit_next already_seen)
        else
          let next = match dir with `down -> visit_down | `up -> visit_up in
          let already_seen = TypeId.Set.add current already_seen in
          next already_seen ~prev ~current
  in
  try
    let changes =
      try
        debug (fun m -> m "@[<v 2>Node %a@." pp_vertex vertex_0);
        TypeId.Map.iter (fun v _ -> Queue.push (`down, None, v) to_visit) tops;
        let already_seen_0 = visit_next already_seen_0 in
        TypeId.Set.iter (fun v -> Queue.push (`up, None, v) to_visit) bottoms;
        let _already_seen_0 = visit_next already_seen_0 in
        Changes.Add_as_new_class ch
      with Same_matching_class (ty, repr) ->
        debug (fun m ->
            m "Found a type of same matching equivalence class : %a!@."
              pp_vertex repr);
        Changes.Add_to_class (ty, repr)
    in
    Changes.apply poset changes vertex_0;
    (* xdot poset; *)
    debug (fun m -> m "@]");
    debug (fun m ->
        m "@[New tops: %a@]@.@[New bots: %a @]@."
          (TypeId.Map.pp TypeId.pp Fmt.nop)
          poset.tops (TypeId.Set.pp TypeId.pp) poset.bottoms);
    debug (fun m ->
        m "@[<v 2>Explored:@ %i bigger@ %i uncomparable@ %i smaller@]@.@."
          !bigger !uncomparable !smaller);
    ()
  with Same_type v ->
    Format.eprintf "Type %a already present in Poset @." TypeId.pp v

(*Operating on poset*)

let iter_descendants t elt f =
  let rec aux h =
    f h;
    G.iter_succ aux t h
  in
  aux elt

let fold_selected_descendants t p f node0 state0 =
  let rec aux node state =
    let state = f node state in
    if p node then G.fold_succ aux t node state else state
  in
  aux node0 state0

let fold_descendants t f elt z =
  let rec aux h z =
    let z = f h z in
    G.fold_succ aux t h z
  in
  aux elt z

module Tmap = TypeId.Map

exception Uncorrect_class of TypeId.t * TypeId.t

let check poset env ~query:ty ~range:maybe_unif_from_trie =
  let unifs0 = Tmap.empty in
  let maybe_unifiable maybe_unif node = TypeId.check node maybe_unif in
  let update_no_unif node maybe_unif =
    if not @@ TypeId.check node maybe_unif then maybe_unif
    else
      TypeId.Range.remove
        (TypeId.Range.Interval.make node.TypeId.id node.TypeId.id)
        maybe_unif
  in
  let add_unifs_class ty_query node vm unifs =
    let unifs = Tmap.add node vm unifs in
    try
      match TypeId.Map.get node poset.classes with
      | Some set ->
          let get_unif ty_class =
            match Acic.unify env ty_query (TypeId.ty ty_class) with
            | Some vm -> vm
            | None -> raise (Uncorrect_class (ty_class, node))
          in

          TypeId.Set.fold
            (fun ty_class unifs ->
              let unif = get_unif ty_class in
              Tmap.add ty_class unif unifs)
            set unifs
      | None -> unifs
    with Uncorrect_class (ty_class, repr) ->
      debug (fun m ->
          m
            "Type %a in the Poset was not in the correct matching class of \
             type %a"
            TypeId.pp ty_class TypeId.pp repr);
      unifs
  in
  let to_visit = Queue.create () in
  let rec visit_next unifs maybe_unif =
    if Queue.is_empty to_visit then unifs
    else
      let node = Queue.pop to_visit in
      debug (fun m -> m "Visiting Node %a @," pp_vertex node);
      (* xdot poset ~range:!range ~unifs:!unifs; *)
      if Tmap.mem node unifs then
        (visit_next [@ocaml.tailcall]) unifs maybe_unif
      else if not (maybe_unifiable maybe_unif_from_trie node) then
        let maybe_unif =
          fold_descendants poset.graph update_no_unif node maybe_unif
        in
        (visit_next [@ocaml.tailcall]) unifs maybe_unif
      else if not (maybe_unifiable maybe_unif node) then
        (visit_next [@ocaml.tailcall]) unifs maybe_unif
      else
        match Acic.unify env ty node.ty with
        | Some vm ->
            let unifs = add_unifs_class ty node vm unifs in
            G.iter_succ (fun next -> Queue.push next to_visit) poset.graph node;
            (visit_next [@ocaml.tailcall]) unifs maybe_unif
        | None ->
            let range =
              fold_selected_descendants poset.graph
                (maybe_unifiable maybe_unif)
                update_no_unif node maybe_unif
            in
            (visit_next [@ocaml.tailcall]) unifs range
  in
  TypeId.Map.iter
    (fun top { is_only_top } ->
      if is_only_top && not (maybe_unifiable maybe_unif_from_trie top) then ()
      else Queue.push top to_visit)
    poset.tops;
  let unifs = visit_next unifs0 maybe_unif_from_trie in
  Tmap.to_iter unifs |> Iter.map (fun (ty_id, sub) -> (ty_id.TypeId.ty, sub))

let copy t =
  {
    env = t.env;
    graph = G.copy t.graph;
    tops = t.tops;
    bottoms = t.bottoms;
    classes = t.classes;
  }
