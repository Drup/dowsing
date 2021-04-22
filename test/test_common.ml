let env = Type.Env.make ()

let type_testable = Alcotest.testable (Type.pp env.var_names) Type.equal

let all_tests = ref []
let test_cnt = ref 0
let add_tests name tests =
  all_tests := ! all_tests @ [ name, tests ]

let int = Type.constr (LongIdent.Lident "int") [||]
let (-->) = Type.arrow

(* Type.of_string *)

let tests =
  let open Type in [
    "int", int ;
    "int -> int", int --> int ;
    "int * int", tuple (MSet.of_list [ int ; int ]) ;
    "int * int -> int", (tuple @@ MSet.of_list [ int ; int ]) --> int ;
    "int -> int -> int", int --> (int --> int) ;
  ]

let tests =
  let make_test (str, ty) =
    let test () =
      Alcotest.check type_testable str (Type.of_string env str) ty
    in
    incr test_cnt ;
    Alcotest.test_case (Int.to_string ! test_cnt) `Quick test
  in
  CCList.map make_test tests

let () = add_tests "Type.of_string" tests

(* do test *)

let () = Alcotest.run "common" ! all_tests
