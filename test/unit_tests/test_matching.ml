let all_tests = ref []
let test_cnt = ref 0
let add_tests name tests = all_tests := !all_tests @ [ (name, tests) ]

(* Unification.compare *)
                                                     
let equal_tests =
  [ ("a * b", "b * a"); ("unit * a", "a"); ("'a -> 'b", "'b -> 'a") ]
let smaller_tests = [ ("int -> int", "'f -> int") ]

let bigger_tests =
  [ ("'f -> int", "int -> int"); ("'a -> 'b", "int -> float list -> 'a") ]

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

let unsure =
  [
    ("int * int * 'a -> float", "(int -> int) -> float");
    ("int * bool -> int", "bool -> int -> int");
  ]

let not_bigger =
  [
    ("int * int -> int -> float", "(int -> int) -> float");
    ("int * bool -> int", "'a -> int");
  ]

let not_smaller =
  [
    ("(int -> int) -> float", "int * int * 'a -> float * int");
    ("'a -> 'b", "float list");
    ("'a -> 'b", "int * float list -> 'a");
  ]

let uncompatible = [ ("float", "int"); ("'a -> 'a * int list", "float list") ]

let comp' =
  Alcotest.testable Acic.pp_hint ( = )

let match_feat =
  let make_test res (str1, str2) =
    let env = Type.Env.make Data in
    let test () =
      let ty1 = Type.of_string env str1 in
      let ty2 = Type.of_string env str2 in
      let name = Fmt.str "Comparing %s and %s" str1 str2 in
      Alcotest.(check comp') name res @@ Index.MatchFeat.compat_match ty1 ty2
    in
    incr test_cnt;
    Alcotest.test_case (CCInt.to_string !test_cnt) `Quick test
  in
  CCList.map (make_test Unsure) unsure
  @ CCList.map (make_test Uncompatible) uncompatible
  @ CCList.map (make_test Not_bigger) not_bigger
  @ CCList.map (make_test Not_smaller) not_smaller

let () = add_tests "MatchFeat.compatible" match_feat

(* do test *)

let () = Alcotest.run ~argv:Sys.argv "Matching comparison" !all_tests
