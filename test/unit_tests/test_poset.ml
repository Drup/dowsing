module P = Db.Internals.Poset
module Index = Db.DefaultIndex

let e = Common.Type.Env.make ()

(* Common vertices and edges tests *)

let ( --> ) n n' = P.G.E.create n () n'

let make_checker s =
  let x0 = P.init e in
  let type_list = ref [] in
  let mk =
    let r = ref 0 in
    fun s ->
      let n = TypeId.mk (CCRef.get_then_incr r) (Type.of_string e s) in
      type_list := n :: !type_list;
      P.add x0 n
        ~range:TypeId.Range.(add (Interval.make 0 !r) empty);
      n
  in
  let check_vertex x elt () =
    let b = P.G.mem_vertex x.P.graph elt in
    let s = Fmt.str "Searching for %a" TypeId.pp elt in
    Alcotest.(check bool) s true b
  and check_edge x expected edge () =
    let b = P.G.mem_edge_e x.P.graph edge in
    let s =
      Fmt.str "Searching for edge between %a and %a" TypeId.pp
        (P.G.E.src edge)
        TypeId.pp
        (P.G.E.dst edge)
    in
    Alcotest.(check bool) s expected b
  in
  let add_vertex _ v =
    let s = Fmt.str "@[%a@]" TypeId.pp v in
    Alcotest.test_case s `Quick (check_vertex x0 v)
  in
  let add_edge _ (t,b) =
    let s = Fmt.str "@[(%a) → (%a)@]" TypeId.pp t TypeId.pp b in
    Alcotest.test_case s `Quick (check_edge x0 true (t,b))
  in
  let add_forbidden_edge _ (t,b) =
    let s = Fmt.str "@[(%a) → (%a)@]" TypeId.pp t TypeId.pp b in
    Alcotest.test_case s `Quick (check_edge x0 false (t,b))
  in
  let test edges forbidden_edges = [
    (s^" Vertex", List.mapi add_vertex !type_list);
    (s^" Edge", List.mapi add_edge edges);
    (s^" Forbidden Edge", List.mapi add_forbidden_edge forbidden_edges);
  ]
  in
  mk, test

let base =
  let t, test = make_checker "Base" in
  let b1 = t "'a"
  and b2 = t "int"
  and b3 = t "float"
  and a1 = t "int -> int"
  and a2 = t "int -> 'b"
  and a3 = t "'a -> 'b"
  and a4 = t "int * float -> 'a"
  and a5 = t "'c -> int" in
  let expected_edges = [
      b1 --> a3;
      b1 --> b2;
      b1 --> b3;
      a3 --> a2;
      a3 --> a5;
      a2 --> a1;
      a2 --> a4;
      a5 --> a1;
    ]
  and forbidden_edges = [
    b1 --> a1;
    b3 --> a2;
  ]
  in
  test expected_edges forbidden_edges

(* do test *)

let () = Alcotest.run "Adding in poset" (
    base
  )
