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

module Changes = struct
  type handle_edges = {
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

  let add_to_class new_type repr classes =
    let old_set =
      match TypeId.Map.get repr classes with
      | Some s -> s
      | None -> TypeId.Set.empty
    in
    let new_set = TypeId.Set.add new_type old_set in
    TypeId.Map.add repr new_set classes

  let add_upper_bound ch g new_top =
    let new_upper_bound =
      G.fold_pred (fun v -> TypeId.Set.remove v) g new_top ch.upper_bounds
      |> TypeId.Set.add new_top
    in
    ch.upper_bounds <- new_upper_bound

  let add_lower_bound ch g new_bot =
    let new_lower_bound =
      G.fold_succ (fun v -> TypeId.Set.remove v) g new_bot ch.lower_bounds
      |> TypeId.Set.add new_bot
    in
    ch.lower_bounds <- new_lower_bound

  let remove_edge ch dir (prev, current) =
    match prev with
    | None -> ()
    | Some ty ->
        let edge =
          match dir with `down -> (ty, current) | `up -> (current, ty)
        in
        ch.remove_edges <- Edge_set.add edge ch.remove_edges

  let add_to_class poset ty repr =
    poset.classes <- add_to_class ty repr poset.classes

  let add_new_class poset vertex_0 ch =
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

let add ({ env; graph; tops; bottoms; _ } as poset) tyid_0 ~range =
  let ty_0 = TypeId.ty tyid_0 in
  let ch = Changes.empty () in
  let already_seen_0 = TypeId.Set.empty in
  let to_visit : (_ * TypeId.t option * TypeId.t) Queue.t = Queue.create () in
  let bigger = ref 0 and smaller = ref 0 and uncomparable = ref 0 in
  let compare tyid =
    if TypeId.check tyid range then
      MatchFeat.compare env (TypeId.ty tyid) ty_0
    else
      Uncomparable
  in
  let rec visit_down already_seen ~prev ~current =
    debug (fun m ->
        m "Visiting Edge down %a → %a@,"
          (Fmt.option ~none:(Fmt.any "⊤") pp_vertex)
          prev pp_vertex current);
    match compare current with
    | Matching_equiv ->
      if Type.equal ty_0 (TypeId.ty current) then raise (Same_type tyid_0)
      else raise (Same_matching_class (tyid_0, current))
    | Bigger ->
      incr bigger;
      let l = G.succ graph current in
      List.iter
        (fun next -> Queue.push (`down, Some current, next) to_visit)
        l;
      Changes.add_upper_bound ch poset.graph current;
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
    match compare current with
    | Matching_equiv -> raise (Same_matching_class (tyid_0, current))
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
      Changes.add_lower_bound ch poset.graph current;
      visit_next already_seen
  and visit_next already_seen =
    match Queue.take_opt to_visit with
    | None -> already_seen
    | Some (dir, prev, current) ->
      if TypeId.Set.mem current already_seen then (
        let pp_dir fmt d =
          match d with `up -> Fmt.pf fmt "up" | `down -> Fmt.pf fmt "down"
        in
        debug (fun m ->
            m "Tried to visit an edge %a. Already visited node %a@," pp_dir
              dir TypeId.pp current);
        visit_next already_seen)
      else
        let already_seen = TypeId.Set.add current already_seen in
        match dir with
        | `down -> visit_down already_seen ~prev ~current
        | `up -> visit_up already_seen ~prev ~current
  in
  debug (fun m -> m "Adding type %a in the Poset" TypeId.pp tyid_0);
  begin try
      debug (fun m -> m "@[<v 2>Node %a@." pp_vertex tyid_0);
      TypeId.Map.iter (fun v _ -> Queue.push (`down, None, v) to_visit) tops;
      let already_seen_0 = visit_next already_seen_0 in
      TypeId.Set.iter (fun v -> Queue.push (`up, None, v) to_visit) bottoms;
      let _already_seen_0 = visit_next already_seen_0 in
      Changes.add_new_class poset tyid_0 ch
    with
    | Same_matching_class (ty, repr) ->
      debug (fun m ->
          m "Found a type of same matching equivalence class : %a!@."
            pp_vertex repr);
      Changes.add_to_class poset ty repr;
    | Same_type v ->
      debug (fun m -> 
          m "Type %a already present in Poset @." TypeId.pp v
        );
  end;
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

exception Incorrect_class of TypeId.t * TypeId.t

module MemoTbl = struct
  type state = Maybe | Unifiable of Subst.t | Not_unifiable
  type t = { tbl : state TypeId.Tbl.t ; rg : TypeId.Range.t }
  let init poset =
    let tbl = TypeId.Tbl.create (size poset) in
    tbl
  let get tbl tyid =
    TypeId.Tbl.get_or tbl tyid ~default:Maybe
  let mark_unif tbl tyid sub = 
    TypeId.Tbl.add tbl tyid (Unifiable sub)
  let mark_not_unif tbl tyid = 
    TypeId.Tbl.add tbl tyid Not_unifiable
  let iter_unifiable tbl =
    TypeId.Tbl.to_iter tbl
    |> Iter.filter_map (function (k, Unifiable sub) -> Some (k,sub) | _ -> None)
end
  
let check poset env ~query:ty ~range =
  let memotbl = MemoTbl.init poset in
  let check tyid =
    if TypeId.check tyid range then
      MemoTbl.get memotbl tyid
    else
      Not_unifiable
  in
  let mark_unif_class ty_query node subst =
    MemoTbl.mark_unif memotbl node subst;
    try
      match TypeId.Map.get node poset.classes with
      | Some set ->
        let get_unif ty_class =
          match Acic.unify env ty_query (TypeId.ty ty_class) with
          | Some vm -> vm
          | None -> raise (Incorrect_class (ty_class, node))
        in
        TypeId.Set.iter
          (fun ty_class ->
             let unif = get_unif ty_class in
             MemoTbl.mark_unif memotbl ty_class unif)
          set
      | None -> ()
    with Incorrect_class (ty_class, repr) ->
      debug (fun m ->
          m
            "Type %a in the Poset was not in the correct matching class of \
             type %a"
            TypeId.pp ty_class TypeId.pp repr)
  in
  let to_visit = Queue.create () in
  let rec visit_next () =
    if Queue.is_empty to_visit then ()
    else
      let node = Queue.pop to_visit in
      debug (fun m -> m "Visiting Node %a @," pp_vertex node);
      (* xdot poset ~range:!range ~unifs:!unifs; *)
      match check node with
      | Unifiable _ ->
        (visit_next [@ocaml.tailcall]) ()
      | Not_unifiable ->
        iter_descendants
          poset.graph node (MemoTbl.mark_not_unif memotbl) ;
        (visit_next [@ocaml.tailcall]) ()
      | Maybe ->
        match Acic.unify env ty node.ty with
        | Some vm ->
          mark_unif_class ty node vm;
          G.iter_succ (fun next -> Queue.push next to_visit) poset.graph node;
          (visit_next [@ocaml.tailcall]) ()
        | None ->
          iter_descendants
            poset.graph node (MemoTbl.mark_not_unif memotbl) ;
          (visit_next [@ocaml.tailcall]) ()
  in
  TypeId.Map.iter
    (fun top { is_only_top } ->
       if is_only_top && not (TypeId.check top range) then ()
       else Queue.push top to_visit)
    poset.tops;
  visit_next ();
  MemoTbl.iter_unifiable memotbl
  |> Iter.map (fun (ty_id, sub) -> (ty_id.TypeId.ty, sub))

let copy t =
  {
    env = t.env;
    graph = G.copy t.graph;
    tops = t.tops;
    bottoms = t.bottoms;
    classes = t.classes;
  }
