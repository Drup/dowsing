module P = Index__Poset
module Index = (val Index.(make Feature.all))

let e = Common.Type.Env.make Data
let all_tests = ref []
let add_tests name tests = all_tests := !all_tests @ [ (name, tests) ]

(* Common vertices and edges tests *)

let ( --> ) n n' = P.G.E.create n () n'

let vertices_and_edges =
  let x0 = P.init e in
  let t =
    let r = ref 0 in
    fun s ->
      P.add x0 @@ TypeId.mk (CCRef.get_then_incr r) (Type.of_string e s)
  in
  let b1 = t "'a" and b2 = t "int" and b3 = t "float" in
  let a1 = t "int -> int"
  and a2 = t "int -> 'b"
  and a3 = t "'a -> 'b"
  and a4 = t "int * float -> 'a"
  and a5 = t "'c -> int" in
  let base_types = [ b1; b2; b3 ] and arrow_types = [ a1; a2; a3; a4; a5 ] in
  let expected_edges =
    [
      b1 --> a3;
      b1 --> b2;
      b1 --> b3;
      a3 --> a2;
      a3 --> a5;
      a2 --> a1;
      a2 --> a4;
      a5 --> a1;
    ]
  in
  (x0, base_types @ arrow_types, expected_edges)

let mem_tests vertices_and_edges =
  let x0, vertices, edges = vertices_and_edges in
  let check_vertex x elt () =
    let b = P.G.mem_vertex x.P.graph elt in
    let s = Fmt.str "Searching for %a" TypeId.pp elt in
    Alcotest.(check bool) s true b
  and check_edge x edge () =
    let b = P.G.mem_edge_e x.P.graph edge in
    let s =
      Fmt.str "Searching for edge between %a and %a" TypeId.pp
        (P.G.E.src edge)
        TypeId.pp
        (P.G.E.dst edge)
    in
    Alcotest.(check bool) s true b
  in
  let rec aux_vertex i vl =
    match vl with
    | [] -> []
    | v :: q ->
        Alcotest.test_case (CCInt.to_string i) `Quick (check_vertex x0 v)
        :: aux_vertex (i + 1) q
  and aux_edge i el =
    match el with
    | [] -> []
    | e :: q ->
        Alcotest.test_case (CCInt.to_string i) `Quick (check_edge x0 e)
        :: aux_edge (i + 1) q
  in
  (aux_vertex 0 vertices, aux_edge 0 edges)

let () =
  let vt, et = mem_tests vertices_and_edges in
  add_tests "Vertices present" vt;
  add_tests "Edges present" et

(* do test *)

let () = Alcotest.run "Adding in poset" !all_tests
