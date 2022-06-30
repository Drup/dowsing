open Graph
module Logs = (val Logs.(src_log @@ Src.create __MODULE__))

let _info = Logs.info
let debug = Logs.debug

module G = Imperative.Digraph.ConcreteBidirectional (TypeId)
module Edge_set = Set.Make (G.E)

type t = {
  env : Type.Env.t;
  graph : G.t;
  mutable tops : TypeId.Set.t;
  mutable bottoms : TypeId.Set.t;
}

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
  type t = G.t * TypeId.Range.t * Subst.t TypeId.Map.t

  module V = struct
    type t = TypeId.t * Unif_state.t
  end

  module E = struct
    type t = V.t * V.t

    let src = fst
    let dst = snd
  end

  let annotate_vertex (_g, range, unifs) v =
    match (TypeId.Map.find_opt v unifs, TypeId.check v range) with
    | None, true -> (v, Unif_state.Unknown)
    | None, false -> (v, Unif_state.False)
    | Some sub, _ -> (v, Unif_state.True sub)

  let iter_vertex f (g, range, unifs) =
    let f' v = f @@ annotate_vertex (g, range, unifs) v in
    G.iter_vertex f' g

  let iter_edges_e f (g, range, unifs) =
    let f' (src, dst) =
      f
      @@ ( annotate_vertex (g, range, unifs) src,
           annotate_vertex (g, range, unifs) dst )
    in
    G.iter_edges_e f' g

  let graph_attributes _ = []
  let default_vertex_attributes _ = []
  let vertex_name (v, _) = "\"" ^ Fmt.to_to_string pp_vertex v ^ "\""

  let vertex_attributes (v, state) =
    let v_name = Fmt.str "%a@." pp_vertex v in
    let color_attr, font_color, lbl =
      match state with
      | Unif_state.Unknown -> (`Color 0xFFAF00, `Fontcolor 0x000000, v_name)
      | Unif_state.False -> (`Color 0xc60013, `Fontcolor 0xFFFFFF, v_name)
      | Unif_state.True u ->
          let str = Fmt.str "%a" Subst.pp u in
          (`Color 0x007500, `Fontcolor 0xFFFFFF, v_name ^ "Unif : " ^ str)
    in
    [ color_attr; font_color; `Shape `Box; `Style `Filled; `Label lbl ]

  let get_subgraph _ = None
  let default_edge_attributes _ = []
  let edge_attributes _ = []
end)

let xdot ?(range = TypeId.Range.empty) ?(unifs = TypeId.Map.empty) e =
  let s = Filename.temp_file "dowsing_" ".dot" in
  let fmt = Format.formatter_of_out_channel @@ open_out s in
  Fmt.pf fmt "%a@." D.fprint_graph (e.graph, range, unifs);
  let _ = Unix.system @@ Fmt.str "xdot %s" @@ Filename.quote s in
  ()

(*Initialization and transformations*)

let init env =
  let g = G.create () in
  let tops = TypeId.Set.empty in
  let bottoms = TypeId.Set.empty in
  { env; graph = g; tops; bottoms }

let size poset = G.nb_vertex poset.graph

let clear poset =
  poset.tops <- TypeId.Set.empty;
  poset.bottoms <- TypeId.Set.empty;
  G.clear poset.graph

module Changes = struct
  type t = {
    mutable remove_edges : Edge_set.t;
    mutable lower_bounds : TypeId.Set.t;
    mutable upper_bounds : TypeId.Set.t;
  }

  let empty () =
    {
      upper_bounds = TypeId.Set.empty;
      remove_edges = Edge_set.empty;
      lower_bounds = TypeId.Set.empty;
    }

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

  let apply poset ch vertex_0 =
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
        poset.tops <- TypeId.Set.remove dst poset.tops;
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
      poset.tops <- TypeId.Set.add vertex_0 poset.tops;
    if TypeId.Set.is_empty ch.lower_bounds then
      poset.bottoms <- TypeId.Set.add vertex_0 poset.bottoms;
    ()
end

(*Adding in poset*)

exception Type_already_present of G.V.t

let add ({ env; graph; tops; bottoms } as poset) vertex_0 =
  let ty_0 = TypeId.ty vertex_0 in
  let ch = Changes.empty () in
  let already_seen_0 = TypeId.Set.empty in
  let to_visit : (_ * TypeId.t option * TypeId.t) Queue.t = Queue.create () in
  let bigger = ref 0 and smaller = ref 0 and uncomparable = ref 0 in
  let rec visit_down already_seen ~prev ~current =
    debug (fun m ->
        m "Visiting Edge down %a → %a@,"
          (Fmt.option ~none:(Fmt.any "⊤") pp_vertex)
          prev pp_vertex current);
    let comp = MatchFeat.compare env (TypeId.ty current) ty_0 in
    debug (fun m -> m "%a@," Acic.pp_ord comp);
    match comp with
    | Equal -> raise (Type_already_present current)
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
    let comp = MatchFeat.compare env (TypeId.ty current) ty_0 in
    debug (fun m -> m "%a@," Acic.pp_ord comp);
    match comp with
    | Equal -> raise (Type_already_present current)
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
    debug (fun m -> m "@[<v 2>Node %a@." pp_vertex vertex_0);
    TypeId.Set.iter (fun v -> Queue.push (`down, None, v) to_visit) tops;
    let already_seen_0 = visit_next already_seen_0 in
    TypeId.Set.iter (fun v -> Queue.push (`up, None, v) to_visit) bottoms;
    let _already_seen_0 = visit_next already_seen_0 in
    Changes.apply poset ch vertex_0;
    (* xdot poset; *)
    debug (fun m -> m "@]");
    debug (fun m ->
        m "@[New tops: %a@]@.@[New bots: %a @]@." (TypeId.Set.pp TypeId.pp)
          poset.tops (TypeId.Set.pp TypeId.pp) poset.bottoms);
    debug (fun m ->
        m "@[<v 2>Explored:@ %i bigger@ %i uncomparable@ %i smaller@]@.@."
          !bigger !uncomparable !smaller);
    ()
  with Type_already_present node ->
    debug (fun m -> m "Found the same type %a!@." pp_vertex node)

(*Operating on poset*)

let iter_succ t elt f =
  let rec aux l =
    match l with
    | [] -> ()
    | h :: q ->
        f h;
        aux (G.succ t h);
        aux q
  in
  aux [ elt ]

let fold_succ t elt f res_0 =
  let rec aux res l =
    match l with
    | [] -> res
    | h :: q ->
        let res1 = f res h in
        let res2 = aux res1 (G.succ t h) in
        aux res2 q
  in
  aux res_0 [ elt ]

let iter_pred t elt f =
  let rec aux l =
    match l with
    | [] -> ()
    | h :: q ->
        f h;
        aux (G.pred t h);
        aux q
  in
  aux [ elt ]

let fold_pred t elt f res_0 =
  let rec aux res l =
    match l with
    | [] -> res
    | h :: q ->
        let res1 = f res h in
        let res2 = aux res1 (G.pred t h) in
        aux res2 q
  in
  aux res_0 [ elt ]

let check poset env ~query:ty ~range =
  let module Tmap = TypeId.Map in
  let unifs = ref Tmap.empty in
  let range = ref range in
  let update_no_unif node =
    range :=
      TypeId.Range.remove
        (TypeId.Range.Interval.make node.TypeId.id node.TypeId.id)
        !range
  in
  let to_visit = Queue.create () in
  let rec visit_down node =
    debug (fun m -> m "Visiting Node %a @," pp_vertex node);
    (* xdot poset ~range:!range ~unifs:!unifs; *)
    if Tmap.mem node !unifs then visit_next ()
    else if not (TypeId.check node !range) then (
      iter_succ poset.graph node update_no_unif;
      visit_next ())
    else
      match Acic.unify env ty node.ty with
      | Some vm ->
          unifs := Tmap.add node vm !unifs;
          let l = G.succ poset.graph node in
          List.iter (fun next -> Queue.push next to_visit) l;
          visit_next ()
      | None ->
          iter_succ poset.graph node update_no_unif;
          visit_next ()
  and visit_next () =
    try
      let next = Queue.pop to_visit in
      visit_down next
    with Queue.Empty -> ()
  in
  TypeId.Set.iter (fun v -> Queue.push v to_visit) poset.tops;
  visit_next ();
  Tmap.to_iter !unifs |> Iter.map (fun (ty_id, sub) -> (ty_id.TypeId.ty, sub))

let copy t =
  { env = t.env; graph = G.copy t.graph; tops = t.tops; bottoms = t.bottoms }
