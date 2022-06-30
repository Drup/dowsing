let all_tests = ref []
let test_cnt = ref 0
let add_tests name tests = all_tests := !all_tests @ [ (name, tests) ]

(* Unification.compare *)

let equal_tests =
  [ ("a * b", "b * a"); ("unit * a", "a"); ("'a -> 'b", "'b -> 'a") ]

let smaller_tests = [ ("int -> int", "'f -> int") ]
let bigger_tests = [ ("'f -> int", "int -> int") ]

let uncomparable_tests =
  [
    ("int", "float");
    ("'f -> int", "int -> 'b");
    ( "('a * unit * string * 'b t) format4 -> 'a",
      "('a * Buffer.t * unit * ('b * string) t) format4 -> 'a" );
  ]

let comp = Alcotest.testable Acic.pp_ord ( = )

let matching =
  let make_test res (str1, str2) =
    let env = Type.Env.make Data in
    let test () =
      let ty1 = Type.of_string env str1 in
      let ty2 = Type.of_string env str2 in
      let name = Fmt.str "Comparing %s and %s" str1 str2 in
      Alcotest.(check comp) name res @@ Acic.compare env ty1 ty2
    in
    incr test_cnt;
    Alcotest.test_case (CCInt.to_string !test_cnt) `Quick test
  in
  CCList.map (make_test Equal) equal_tests
  @ CCList.map (make_test Smaller) smaller_tests
  @ CCList.map (make_test Bigger) bigger_tests
  @ CCList.map (make_test Uncomparable) uncomparable_tests

let () = add_tests "Acic.compare" matching

let bicompat =
  [
    ("int * int * 'a -> float", "(int -> int) -> float");
    ("int * bool -> int", "bool -> int -> int");
  ]

let compat_leq =
  [
    ("int * int -> int -> float", "(int -> int) -> float");
    ("int * bool -> int", "'a -> int");
  ]

let compat_geq =
  [
    ("(int -> int) -> float", "int * int * 'a -> float * int");
    ("'a -> 'b", "float list");
  ]

let uncompat = [ ("float", "int"); ("'a -> 'a * int list", "float list") ]
let comp' = Alcotest.pair Alcotest.bool Alcotest.bool

let compat_match (t1 : Type.t) (t2 : Type.t) =
  let check_feat (compat_leq, compat_geq) feat =
    let module Ft = (val feat : Index.MatchFeat.S) in
    let feat1 = Ft.compute t1 and feat2 = Ft.compute t2 in
    let compat_leq = compat_leq && Ft.compatible ~query:feat1 ~data:feat2
    and compat_geq = compat_geq && Ft.compatible ~query:feat2 ~data:feat1 in
    (compat_leq, compat_geq)
  in
  let feats = Index.MatchFeat.all in
  CCList.fold_left check_feat (true, true) feats

let match_feat =
  let make_test res (str1, str2) =
    let env = Type.Env.make Data in
    let test () =
      let ty1 = Type.of_string env str1 in
      let ty2 = Type.of_string env str2 in
      let name = Fmt.str "Comparing %s and %s" str1 str2 in
      Alcotest.(check comp') name res @@ compat_match ty1 ty2
    in
    incr test_cnt;
    Alcotest.test_case (CCInt.to_string !test_cnt) `Quick test
  in
  CCList.map (make_test (true, true)) bicompat
  @ CCList.map (make_test (true, false)) compat_leq
  @ CCList.map (make_test (false, true)) compat_geq
  @ CCList.map (make_test (false, false)) uncompat

let () = add_tests "MatchFeat.compatible" match_feat

(* do test *)

let () = Alcotest.run ~argv:Sys.argv "Matching comparison" !all_tests
