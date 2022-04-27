module P = Index__Poset.Poset
module Index = (val Index.(make Feature.all))

let e = Common.Type.Env.make Data
let all_tests = ref []
let add_tests name tests = all_tests := !all_tests @ [ (name, tests) ]

(* Edge tests *)

let t s = P.G.V.create @@ Type.of_string e s
let ( --> ) n n' = (n, n')

let _types_and_edges =
  let n1 = t "'a" and n2 = t "int -> int" and n3 = t "int -> 'b" (* ... *) in
  let types = [ n1; n2; n3 ] in
  let expected_edges =
    [
      n1 --> n3;
      n3 --> n2 (*"'a -> (int -> 'c)\n (int -> 'c) -> (int -> int)\n"*);
    ]
  in
  (types, expected_edges)

let _mem_edge_tests type_edge =
  let types, edges = type_edge in
  let x0 = P.init e in
  let check_graph x edg () =
    let s, d = edg in
    let b = P.G.mem_edge x.P.graph s d in
    Format.eprintf "%a" P.pp x;
    let s =
      Fmt.str "Searching for edge between %a and %a" Type.pp (P.G.V.label s)
        Type.pp (P.G.V.label d)
    in
    Alcotest.(check bool) s true b
  in
  let rec aux i el =
    match el with
    | [] -> []
    | e :: q ->
        Alcotest.test_case (CCInt.to_string i) `Quick (check_graph x0 e)
        :: aux (i + 1) q
  in
  let _ = CCList.map (P.add x0) types in
  aux 0 edges

(*let () = add_tests "Edges present" @@ mem_edge_tests types_and_edges*)

(* Vertex tests *)

let base_types =
  let string_types = [ (* "'a"; *) "int"; "float" ] in
  CCList.map (Type.of_string e) string_types

let arrow_types =
  let string_types =
    [
      (* "'a"; *)
      "int -> int";
      "int -> 'a";
      "'a -> 'b";
      "int * float -> 'a";
      "'c -> int";
    ]
  in
  CCList.map (Type.of_string e) string_types

let mem_vertex_tests types =
  let x0 = P.init e in
  let check_graph x elt () =
    let b = P.G.mem_vertex x.P.graph elt in
    Format.eprintf "%a" P.pp x;
    let s = Fmt.str "Searching for %a" Type.pp (P.G.V.label elt) in
    Alcotest.(check bool) s true b
  in
  let rec aux i tl =
    match tl with
    | [] -> []
    | t :: q ->
        let node = P.add x0 t in
        Format.printf "%a" P.pp x0;
        Alcotest.test_case (CCInt.to_string i) `Quick (check_graph x0 node)
        :: aux (i + 1) q
  in
  let l = aux 0 types in
  P.xdot x0;
  l

let () =
  add_tests "Base types" @@ mem_vertex_tests base_types;
  add_tests "Arrow types" @@ mem_vertex_tests arrow_types;
  add_tests "Arrow then base types"
  @@ mem_vertex_tests (arrow_types @ base_types);
  add_tests "Base then arrow types"
  @@ mem_vertex_tests (base_types @ arrow_types);
  ()

(* do test *)

let () = Alcotest.run "Adding in poset" !all_tests
