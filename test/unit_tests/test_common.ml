let env = Type.Env.make ()

let type_testable = Alcotest.testable Type.pp Type.equal

let all_tests = ref []
let test_cnt = ref 0
let add_tests name tests =
  all_tests := ! all_tests @ [ name, tests ]

let int = Type.constr env (LongIdent.Lident "int") [||]
let (-->) = Type.arrow env
let tuple tys = Type.(tuple env @@ Tuple.mk_l tys)

(* Type.of_string *)

let tests = [
  "int", int ;
  "int -> int", int --> int ;
  "int * int", tuple [ int ; int ] ;
  "int * int -> int", tuple [ int ; int ] --> int ;
  "int -> int -> int", tuple [ int; int ] --> int ;
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
