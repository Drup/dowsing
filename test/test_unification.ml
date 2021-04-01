let all_tests = ref []
let test_cnt = ref 0

let add_tests name tests =
  all_tests := ! all_tests @ [ name, tests ]

let env = Type.Env.make ()

(* Unification.unifiable *)

let pos_tests = [
  "int", "int" ;
  "'a", "'b" ;
  "int -> int", "int -> int" ;
  "int -> int", "int -> 'a" ;
  "int -> int", "'a -> int" ;
  "int -> int", "'a -> 'a" ;
  "int -> int -> int", "int * int -> int" ;
  "'a -> 'b", "int -> int -> int" ;
  "'a * 'b -> 'c", "int -> int -> int" ;
  "'a * 'b -> 'c", "int * int -> int -> int" ;
  "int -> int -> int -> int -> int", "int -> int * int -> int -> int" ;
  "int -> int -> int -> int -> int", "int * int * int -> int -> int" ;
  "'a -> 'b -> 'c", "'x -> 'y * 'z" ;
]

let neg_tests = [
  "int", "int -> int" ;
  "int", "int * int" ;
  "int", "int list" ;
  "'a -> 'b", "'a * 'b" ;
]

let tests =
  let make_test res (str1, str2) =
    let test () =
      let ty1 = Type.of_string env str1 in
      let ty2 = Type.of_string env str2 in
      Alcotest.(check bool) "" res @@ Unification.unifiable env [ ty1, ty2 ]
    in
    incr test_cnt ;
    Alcotest.test_case (Int.to_string ! test_cnt) `Quick test
  in
  CCList.map (make_test true) pos_tests @
  CCList.map (make_test false) neg_tests

let () = add_tests "Unification.unifiable" tests

(* do test *)

let () = Alcotest.run "common" ! all_tests
