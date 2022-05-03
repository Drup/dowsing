open Graph

module Label = struct
  type t = Type.t
end

module type POSET = sig
  type t

  val init : Type.Env.t -> t
  val add : t -> Label.t -> unit
  (*val iter_succ : t -> G.V.t -> (G.V.t -> unit) -> unit*)
  (*val iter_pred : t -> G.V.t -> (G.V.t -> unit) -> unit*)
end

module Poset = struct
  module G = Imperative.Digraph.Abstract (Label)
  module Edge_set = Set.Make (G.E)
  module Vertex_set = Set.Make (G.V)

  let pp_vertex fmt e = Fmt.box Type.pp fmt (G.V.label e)

  let pp_edge fmt e =
    let s = G.V.label @@ G.E.src e and d = G.V.label @@ G.E.dst e in
    Fmt.pf fmt "@[%a ==> %a@]" Type.pp s Type.pp d

  type t = {
    env : Type.Env.t;
    graph : G.t;
    top : G.V.t; (*bottoms : Vertex_set.t;*)
  }

  let init env =
    let g = G.create () in
    let t = Type.of_string env "'a" in
    let node = G.V.create t in
    G.add_vertex g node;
    {
      env;
      graph = g;
      top =
        node
        (*bottoms =
          (let ept = Vertex_set.empty in
           Vertex_set.add node ept);*);
    }

  let size env = G.nb_vertex env.graph
  (*let nb_bottom env = Vertex_set.cardinal env.bottoms*)

  exception Type_already_present of G.V.t

  type changes = {
    mutable remove_edges : Edge_set.t;
    mutable lower_bounds : Vertex_set.t;
    mutable upper_bounds : Vertex_set.t;
  }

  let empty_changes () =
    {
      upper_bounds = Vertex_set.empty;
      remove_edges = Edge_set.empty;
      lower_bounds = Vertex_set.empty;
    }

  let add_upper_bound ch edge =
    ch.upper_bounds <- Vertex_set.remove (G.E.src edge) ch.upper_bounds;
    ch.upper_bounds <- Vertex_set.add (G.E.dst edge) ch.upper_bounds

  let add_lower_bound ch v = ch.lower_bounds <- Vertex_set.add v ch.lower_bounds

  let apply_changes graph ch node_0 =
    Edge_set.iter
      (fun edge ->
        Format.eprintf "Remove Edge %a @," pp_edge edge;
        G.remove_edge_e graph edge)
      ch.remove_edges;
    Vertex_set.iter
      (fun dst ->
        let edge = G.E.create node_0 () dst in
        Format.eprintf "Add Edge %a @," pp_edge edge;
        G.add_edge_e graph edge)
      ch.lower_bounds;
    Vertex_set.iter
      (fun src ->
        let edge = G.E.create src () node_0 in
        Format.eprintf "Add Edge %a @," pp_edge edge;
        G.add_edge_e graph edge)
      ch.upper_bounds;
    ()

  (* let bidirect_add { env; graph; lowest; biggest } elt_0 =
     let node_0 = G.V.create elt_0 in
     let ch = empty_changes () in
     let already_seen = Edge_set.empty in
     let to_visit = Queue.create () in
     let bigger = ref 0 and smaller = ref 0 and uncomparable = ref 0 in
     let rec visit_down already_seen vertex =
       Format.eprintf "Visiting Vertex %a @." pp_vertex vertex;
       match Unification.compare env elt_0 (G.V.label vertex) with
       | Equal -> raise (Type_already_present vertex)
       | Bigger ->
           incr bigger;
           add_upper_bound ch dst;
           ch.remove_edges <- Edge_set.add edge ch.remove_edges;
           visit_next already_seen
       | Smaller ->
           incr smaller;
           let l = G.succ_e graph (G.E.dst edge) in
           List.iter (fun elt -> Queue.push elt to_visit) l;
           add_lower_bound ch edge;
           visit_next already_seen
       | Uncomparable ->
           incr uncomparable;
           let l = extend_edges edge in
           (match l with
           | [] -> ()
           | _ ->
               let push elt = Queue.push elt to_visit in
               List.iter push l);
           visit_next already_seen
     and visit_next already_seen =
       match Queue.take_opt to_visit with
       | None -> ()
       | Some edge ->
           if Edge_set.mem edge already_seen then visit_next already_seen
           else
             let already_seen = Edge_set.add edge already_seen in
             visit already_seen edge
     in
     try
       Format.eprintf "@[<v 2>Node %a@," pp_vertex node_0;
       visit already_seen (G.E.create lowest () lowest);
       apply_changes graph ch node_0;
       Format.eprintf "@]@.";
       Format.eprintf
         "@[<v 2>Explored:@ %i smaller@ %i bigger@ %i uncomparable@]@." !smaller
         !bigger !uncomparable;
       node_0
     with Type_already_present node ->
       Format.eprintf "Found the same type %a!@]@." pp_vertex node;
       node
  *)

  let add { env; graph; top } elt_0 =
    let node_0 = G.V.create elt_0 in
    let ch = empty_changes () in
    let already_seen = Edge_set.empty in
    let extend_edges edge =
      let rec aux acc l =
        match l with
        | [] -> acc
        | h :: q -> aux (G.E.create (G.E.src edge) () h :: acc) q
      in
      aux [] (G.succ graph (G.E.dst edge))
    in
    let to_visit = Queue.create () in
    let bigger = ref 0 and smaller = ref 0 and uncomparable = ref 0 in
    let rec visit already_seen edge =
      Format.eprintf "Visiting Edge %a @." pp_edge edge;
      let dst = G.E.dst edge in
      match Unification.compare env (G.V.label dst) elt_0 with
      | Equal -> raise (Type_already_present dst)
      | Smaller ->
          incr smaller;
          add_lower_bound ch dst;
          ch.remove_edges <- Edge_set.add edge ch.remove_edges;
          visit_next already_seen
      | Bigger ->
          incr bigger;
          let l = G.succ_e graph (G.E.dst edge) in
          List.iter (fun elt -> Queue.push elt to_visit) l;
          add_upper_bound ch edge;
          visit_next already_seen
      | Uncomparable ->
          incr uncomparable;
          let l = extend_edges edge in
          (match l with
          | [] -> ()
          | _ ->
              let push elt = Queue.push elt to_visit in
              List.iter push l);
          visit_next already_seen
    and visit_next already_seen =
      match Queue.take_opt to_visit with
      | None -> ()
      | Some edge ->
          if Edge_set.mem edge already_seen then visit_next already_seen
          else
            let already_seen = Edge_set.add edge already_seen in
            visit already_seen edge
    in
    try
      Format.eprintf "@[<v 2>Node %a@," pp_vertex node_0;
      visit already_seen (G.E.create top () top);
      apply_changes graph ch node_0;
      Format.eprintf "@]@.";
      Format.eprintf
        "@[<v 2>Explored:@ %i bigger@ %i uncomparable@ %i smaller@]@." !bigger
        !uncomparable !smaller;
      node_0
    with Type_already_present node ->
      Format.eprintf "Found the same type %a!@]@." pp_vertex node;
      node

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

  let copy t = { env = t.env; graph = G.copy t.graph; top = t.top }

  let pp fmt { graph; _ } =
    if G.nb_vertex graph = 0 then Format.fprintf fmt "empty"
    else
      Format.fprintf fmt "@[<v 2>Vertices: %a@]@.@[<v2>Edges: %a@]@."
        (Fmt.iter G.iter_vertex pp_vertex)
        graph
        (Fmt.iter G.iter_edges_e pp_edge)
        graph

  module D = Graphviz.Dot (struct
    include G

    let graph_attributes _ = []
    let default_vertex_attributes _ = []
    let vertex_name v = "\"" ^ Fmt.to_to_string pp_vertex v ^ "\""
    let vertex_attributes _ = [ `Shape `Box ]
    let get_subgraph _ = None
    let default_edge_attributes _ = []
    let edge_attributes _ = []
  end)

  let xdot e =
    let s = Filename.temp_file "dowsing_" ".dot" in
    let fmt = Format.formatter_of_out_channel @@ open_out s in
    Fmt.pf fmt "%a@." D.fprint_graph e.graph;
    let _ = Unix.system @@ Fmt.str "xdot %s" @@ Filename.quote s in
    ()
end
