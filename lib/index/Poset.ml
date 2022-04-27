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

  type t = { env : Type.Env.t; graph : G.t; lowest : G.V.t }

  let init env =
    let g = G.create () in
    let t = Type.of_string env "'a" in
    let node = G.V.create t in
    G.add_vertex g node;
    { env; graph = g; lowest = node }

  type to_do =
    | Replace of G.E.t
    | Add_smaller of G.E.t
    | Add_uncomparable of G.E.t

  exception Type_already_present of G.V.t

  let add { env; graph; lowest } elt_0 =
    let node_0 = G.V.create elt_0 in
    let last_comparables = ref Vertex_set.empty in
    let rec insert_edges l =
      let pp_edge fmt e =
        let s = G.V.label @@ G.E.src e and d = G.V.label @@ G.E.dst e in
        Fmt.pf fmt "@[%a ==> %a@]" Type.pp s Type.pp d
      in
      match l with
      | [] -> ()
      | Replace edge :: rest ->
          Format.eprintf "Replace Edge %a @," pp_edge edge;
          G.remove_edge_e graph edge;
          G.add_edge graph (G.E.src edge) node_0;
          G.add_edge graph node_0 (G.E.dst edge);
          insert_edges rest
      | Add_smaller edge :: rest ->
          Format.eprintf "Add Edge %a @," pp_edge edge;
          G.add_edge_e graph edge;
          insert_edges rest
      | Add_uncomparable edge :: rest ->
          if Vertex_set.mem (G.E.src edge) !last_comparables then (
            Format.eprintf "Add Edge %a @," pp_edge edge;
            G.add_edge_e graph edge);
          insert_edges rest
    in

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
    let rec visit already_seen edge =
      let dst = G.E.dst edge in
      match Unification.compare env elt_0 (G.V.label dst) with
      | Equal -> raise (Type_already_present dst)
      | Bigger ->
          last_comparables := Vertex_set.remove (G.E.src edge) !last_comparables;
          last_comparables := Vertex_set.add (G.E.dst edge) !last_comparables;
          Replace edge :: visit_next already_seen
      | Smaller ->
          let push elt = Queue.push elt to_visit in
          let l = G.succ_e graph (G.E.dst edge) in
          let new_edges =
            match l with
            | [] -> [ Add_smaller (G.E.create (G.E.dst edge) () node_0) ]
            | _ ->
                List.iter push l;
                []
          in
          last_comparables := Vertex_set.remove (G.E.src edge) !last_comparables;
          last_comparables := Vertex_set.add (G.E.dst edge) !last_comparables;
          new_edges @ visit_next already_seen
      | Uncomparable ->
          let l = extend_edges edge in
          let new_edges =
            match l with
            | [] -> [ Add_uncomparable (G.E.create (G.E.src edge) () node_0) ]
            | _ ->
                let push elt = Queue.push elt to_visit in
                List.iter push l;
                []
          in
          new_edges @ visit_next already_seen
    and visit_next already_seen =
      match Queue.take_opt to_visit with
      | None -> []
      | Some edge ->
          if Edge_set.mem edge already_seen then visit_next already_seen
          else
            let already_seen = Edge_set.add edge already_seen in
            visit already_seen edge
    in
    try
      Format.eprintf "@[<v 2>Node %a@," pp_vertex node_0;
      insert_edges (visit already_seen (G.E.create lowest () lowest));
      Format.eprintf "@]@.";
      node_0
    with
    | Type_already_present node ->
        Format.eprintf "Found the same type %a!@]@." pp_vertex node;
        node
    | err ->
        Format.eprintf "error!@]@.";
        raise err

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

  let copy t = { env = t.env; graph = G.copy t.graph; lowest = t.lowest }

  let pp fmt { graph; _ } =
    let pp_edge fmt e =
      let s = G.V.label @@ G.E.src e and d = G.V.label @@ G.E.dst e in
      Fmt.pf fmt "@[%a ==> %a@]" Type.pp s Type.pp d
    in
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
