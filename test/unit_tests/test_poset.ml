module P = Index__Poset.Poset
module Index = (val Index.(make Feature.all))

let e = Common.Type.Env.make Data

let all_tests = ref []
let test_cnt = ref 0
let add_tests name tests = all_tests := !all_tests @ [ (name, tests) ]

(* Tests on graph pp *)

let types_and_results =
  let string_types =
    [
      "'a";
      "int -> int"
      (*"int -> 'a" ; "'a -> 'b"; "int * float -> 'a"; "'c -> int"*);
    ]
  in
  let types = CCList.map (Type.of_string e) string_types in
  let res =
    [
      "\n";
      "'b -> (int -> int)\n"
      (*"'a -> (int -> 'c)\n (int -> 'c) -> (int -> int)\n"*);
    ]
  in
  (types, res)

let string_graph_tests =
  let x = P.init e in
  let rec aux trl acc =
    match trl with
    | [], [] -> acc
    | t :: q1, r :: q2 ->
        let _ = P.add x t in
        aux (q1, q2) ((x, r) :: acc)
    | _ -> invalid_arg "The lists are not of the same size"
  in
  aux types_and_results []

let _string_tests =
  let make_test (g, str) =
    let check_graph () =
      let s = Format.asprintf "%a" P.pp g in
      Alcotest.(check string) "Same string repr" s str
    in
    incr test_cnt;
    Alcotest.test_case (CCInt.to_string !test_cnt) `Quick check_graph
  in
  CCList.map make_test string_graph_tests

(*let () = add_tests "String representation" string_tests*)

(* Tests on graph struct *)

let base_types =
  let string_types =
    [
      (* "'a"; *)
      "int";
      "float";
    ]
  in
  CCList.map (Type.of_string e) string_types
    
let arrow_types =
  let string_types =
    [
      "'a";
      "int -> int";
      "int -> 'a";
      "'a -> 'b";
      "int * float -> 'a";
      "'c -> int";
    ]
  in
  CCList.map (Type.of_string e) string_types

let mem_graph_tests types =
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
      Alcotest.test_case (CCInt.to_string i) `Quick (check_graph x0 node) ::
      aux (i+1) q
  in
  aux 0 types

let () =
  add_tests "Base types" @@ mem_graph_tests base_types;
  add_tests "Arrow types" @@ mem_graph_tests arrow_types;
  ()

(* do test *)

let () = Alcotest.run "Adding in poset" !all_tests

(* let x = P.init e
 * 
 * let add i (t, _) =
 *   Format.printf "%i: %a@." i Common.Type.pp t;
 *   ignore @@ P.add x t
 * 
 * let () =
 *   let idx = Index.load @@ Fpath.v "fmt.idx" in
 *   Index.iter idx |> Iter.iteri add;
 *   Format.printf "%a@." P.pp x *)
