let env = Type.Env.make ()

let all_tests = ref []
let test_cnt = ref 0
let add_tests name tests =
  all_tests := ! all_tests @ [ name, tests ]

(* Unification.unifiable *)

let pos_tests = [
  (* com-2 *)
  "a * b", "b * a" ;
  "a * (b * c)", "(c * b) * a" ;
  "(a * b) f", "(b * a) f" ;
  "a * b -> c", "b * a -> c" ;
  "(a * b) f * c", "(b * a) f * c" ;
  "(a * b) f -> c", "(b * a) f -> c" ;
  "a -> (b * c) f", "a -> (c * b) f" ;
  (* ass-2 *)
  "(a * b) * c", "a * (b * c)" ;
  "((a * b) * c) f", "(a * (b * c)) f" ;
  "(a * b) * c -> d", "a * (b * c) -> d" ;
  "a -> ((b * c) * d) f", "a -> (b * (c * d)) f" ;
  (* ass-0 *)
  "unit * a", "a" ;
  "a * unit", "a" ;
  "unit * a * b", "a * b" ;
  "(unit * a) f", "a f" ;
  "unit * a -> b", "a -> b" ;
  "a -> unit * b", "a -> b" ;
  (* cur-2 *)
  "a * b -> c", "a -> b -> c" ;
  "a * b * c -> d", "a -> b -> c -> d" ;
  "a * b -> c", "b -> a -> c" ;
  "a * b -> c -> d", "a -> b -> c -> d" ;
  "a -> b * c -> d", "a -> b -> c -> d" ;
  "(a * b -> c) f", "(a -> b -> c) f" ;
  "(a * b * c -> d) f", "(a -> b -> c -> d) f" ;
  (* cur-0 *)
  "unit -> a", "a" ;
  "unit -> unit -> a", "a" ;
  "unit * unit -> a", "a" ;
  "a -> unit -> b", "a -> b" ;
  "(unit -> a) * b", "a * b" ;
  "(unit -> a) f", "a f" ;
  (* other *)
  "a -> a", "a -> 'a" ;
  "a -> a", "'a -> a" ;
  "a -> a", "'a -> 'a" ;
  "a -> a", "'a -> 'b" ;
  "a -> a -> a", "'a -> 'b" ;
  "a -> a -> a", "'a * 'b -> 'c" ;
  "a * a -> a -> a", "'a * 'b -> 'c" ;
  "'a -> 'b -> 'c", "'x -> 'y * 'z" ;
  "'a -> 'b f -> a", "'x g * 'y f -> 'x" ;
  "'a -> 'a -> a", "'x * a -> 'x" ;
]

let neg_tests = [
  "a", "a -> a" ;
  "a", "a * a" ;
  "a", "a f" ;
  "'a -> 'b", "'a * 'b" ;
  "'a f * a", "'x g * a" ;
  "'a f -> 'b g -> a", "'x h * 'y -> 'x" ;
  "'a -> 'a -> a", "'x * b -> 'x" ;
]

let tests =
  let make_test res (str1, str2) =
    let test () =
      let ty1 = Type.of_string env str1 in
      let ty2 = Type.of_string env str2 in
      Alcotest.(check bool) "" res @@ Unification.unifiable env [ ty1, ty2 ]
    in
    incr test_cnt ;
    Alcotest.test_case (CCInt.to_string ! test_cnt) `Quick test
  in
  CCList.map (make_test true) pos_tests @
  CCList.map (make_test false) neg_tests

let () = add_tests "Unification.unifiable" tests

(* do test *)

let () = Alcotest.run "unification" ! all_tests
