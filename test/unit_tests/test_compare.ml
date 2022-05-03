let all_tests = ref []
let test_cnt = ref 0
let add_tests name tests = all_tests := !all_tests @ [ (name, tests) ]

(* Unification.compare *)

let equal_tests = [ ("a * b", "b * a"); ("unit * a", "a") ]
let smaller_tests = [ ("int -> int", "'f -> int") ]
let bigger_tests = [ ("'f -> int", "int -> int") ]
let uncomparable_tests = [ ("int", "float"); ("'f -> int", "int -> 'b") ]
let comp = Alcotest.testable Unification.pp_ord ( = )

let tests =
  let make_test res (str1, str2) =
    let env = Type.Env.make Data in
    let test () =
      let ty1 = Type.of_string env str1 in
      let ty2 = Type.of_string env str2 in
      let name = Fmt.str "Comparing %s and %s" str1 str2 in
      Alcotest.(check comp) name res @@ Unification.compare env ty1 ty2
    in
    incr test_cnt;
    Alcotest.test_case (CCInt.to_string !test_cnt) `Quick test
  in
  CCList.map (make_test Equal) equal_tests
  @ CCList.map (make_test Smaller) smaller_tests
  @ CCList.map (make_test Bigger) bigger_tests
  @ CCList.map (make_test Uncomparable) uncomparable_tests

let () = add_tests "Unification.compare" tests

(* do test *)

let () = Alcotest.run ~argv:Sys.argv "Poset comparison" !all_tests
