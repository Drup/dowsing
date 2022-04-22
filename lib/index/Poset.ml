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

  type t = { env : Type.Env.t; graph : G.t; lowest : G.V.t }

  let init env =
    let g = G.create () in
    let t = Type.of_string env "'a" in
    let node = G.V.create t in
    G.add_vertex g node;
    { env; graph = g; lowest = node }

  type to_do = Replace of G.E.t | Add of G.E.t

  exception Type_already_present

  let add { env; graph; lowest } elt_0 =
    let node_0 = G.V.create elt_0 in
    let rec insert_edges l =
      match l with
      | [] -> ()
      | Replace edge :: rest ->
          G.remove_edge_e graph edge;
          G.add_edge graph (G.E.src edge) node_0;
          G.add_edge graph node_0 (G.E.dst edge);
          insert_edges rest
      | Add edge :: rest ->
          G.add_edge_e graph edge;
          insert_edges rest
    in
    let already_seen = Edge_set.empty in
    let get_couples edge =
      let rec aux acc l =
        match l with
        | [] -> acc
        | h :: q -> aux (G.E.create (G.E.src edge) () h :: acc) q
      in
      aux [] (G.succ graph (G.E.dst edge))
    in
    let to_visit = Queue.create () in
    let rec visit already_seen edge =
      match Unification.compare env elt_0 (G.V.label (G.E.dst edge)) with
      | Equal -> raise Type_already_present
      | Bigger -> Replace edge :: visit_next already_seen
      | Smaller ->
          let push elt = Queue.push elt to_visit in
          let l = G.succ_e graph (G.E.dst edge) in
          let new_edges =
            match l with
            | [] -> [ Add (G.E.create (G.E.dst edge) () node_0) ]
            | _ ->
                List.iter push l;
                []
          in
          new_edges @ visit_next already_seen
      | Uncomparable ->
          let push elt = Queue.push elt to_visit in
          let l = get_couples edge in
          List.iter push l;
          visit_next already_seen
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
      insert_edges (visit already_seen (G.E.create lowest () lowest));
      node_0
    with Type_already_present -> node_0

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
      Fmt.pf fmt "%a -> %a" Type.pp_parens s Type.pp_parens d
    in
    Format.fprintf fmt "@[<v>%a@]@." (Fmt.iter G.iter_edges_e pp_edge) graph
end
