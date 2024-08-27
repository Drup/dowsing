let all_tests = ref []
let test_cnt = ref 0
let add_tests name tests =
  all_tests := ! all_tests @ [ name, tests ]

(* Acic.unifier *)

let pos_tests = [
  (* com-2 *)
  1, "a * b", "b * a" ;
  1, "int * float", "float * int";
  1, "a * (b * c)", "(c * b) * a" ;
  1, "(a * b) f", "(b * a) f" ;
  1, "a * b -> c", "b * a -> c" ;
  1, "(a * b) f * c", "(b * a) f * c" ;
  1, "(a * b) f -> c", "(b * a) f -> c" ;
  1, "a -> (b * c) f", "a -> (c * b) f" ;
  (* ass-2 *)
  1, "(a * b) * c", "a * (b * c)" ;
  1, "((a * b) * c) f", "(a * (b * c)) f" ;
  1, "(a * b) * c -> d", "a * (b * c) -> d" ;
  1, "a -> ((b * c) * d) f", "a -> (b * (c * d)) f" ;
  (* ass-0 *)
  1, "unit * a", "a" ;
  1, "a * unit", "a" ;
  1, "unit * a * b", "a * b" ;
  1, "(unit * a) f", "a f" ;
  1, "unit * a -> b", "a -> b" ;
  1, "a -> unit * b", "a -> b" ;
  (* cur-2 *)
  1, "a * b -> c", "a -> b -> c" ;
  1, "a * b * c -> d", "a -> b -> c -> d" ;
  1, "a * b -> c", "b -> a -> c" ;
  1, "a * b -> c -> d", "a -> b -> c -> d" ;
  1, "a -> b * c -> d", "a -> b -> c -> d" ;
  1, "(a * b -> c) f", "(a -> b -> c) f" ;
  1, "(a * b * c -> d) f", "(a -> b -> c -> d) f" ;
  (* currification + unification *)
  1, "a -> 'a", "a -> b -> c" ;
  1, "a * b -> c", "a -> 'a" ;
  1, "'a -> 'a", "(x -> y) * x -> y" ;
  (* other *)
  2, "a -> a", "a -> 'a" ;
  1, "a -> a", "'a -> a" ;
  1, "a -> a", "'a -> 'a" ;
  3, "a -> a", "'a -> 'b" ;
  4, "a -> a -> a", "'a -> 'b" ;
  9, "a -> a -> a", "'a * 'b -> 'c" ;
  14, "a * a -> a -> a", "'a * 'b -> 'c" ;
  6, "'a -> 'b -> 'c", "'x -> 'y * 'z" ;
  3, "'a -> 'b f -> a", "'x g * 'y f -> 'x" ; (* We find more general solution, therefore less *)
  1, "'a -> 'a -> a", "'x * a -> 'x" ;
  1, "'a * int", "bool * bool * 'b";
  108, "('a * 'b) -> ('a -> 'z) -> ('b -> 'y) -> unit", "('a * 'b) -> ('a -> 'z) -> ('b -> 'y) -> unit";
  4, "int list * int -> 'a", "int list * int -> 'a";
  (* sub constructor *)
  2, "int list -> 'a", "int list -> int";
  (* Tuples target of variable: TODO I think this is a case where a variable will point to a tuples *)
  1, "'a * 'b -> 'a", "int * unit * float -> int * unit";
  (* Loop *)
  2, "'a -> 'a", "'b -> 'b";
  1, "'a -> 'a -> 'a", "'x * b -> 'x" ;
  (* Need ACU *)
  2, "'a -> 'b", "'a * 'b" ;
]

let neg_tests = [
  (* cur-0 *)
  0, "unit -> a", "a" ;
  0, "unit -> unit -> a", "a" ;
  0, "unit * unit -> a", "a" ;
  0, "(unit -> a) * b", "a * b" ;
  0, "(unit -> a) f", "a f" ;
  (* currification + unification *)
  0, "a -> 'a", "x -> b -> c" ;
  0, "x * b -> c", "a -> 'a" ;
  0, "'a -> 'a", "(x -> y) * z -> y" ;
  (* other *)
  0, "a", "a -> a" ;
  0, "a", "a * a" ;
  0, "a", "a f" ;
  0, "'a f * a", "'x g * a" ;
  0, "'a f -> 'b g -> a", "'x h * 'y -> 'x" ;
  0, "'a -> 'a -> a", "'x * b -> 'x" ;
  0, "'a * float * 'a", "int * 'a * 'a" ;
  0, "int * int * int", "int * int";
  0, "int * int * int * 'a", "int * int";
  (* require occur check *)
  0, "'X -> 'X", "'a * ('a, 'b) t * ('a, 'b) t -> 'a option * ('a, 'b) t * ('a, 'b) t" ;
  0, "'a -> 'a list -> 'a", "'x -> 'x -> 'x" ;
  0, "('a * unit * string * 'b t) format4 -> 'a","('a * Buffer.t * unit * ('b * string) t) format4 -> 'a";
]

let slow_tests = [
  1, "'A * int list -> unit",
  "anchor * bitmap * bool * bool * color * color * color * color * color * color * color * color * cursor *  int * int * int * int * int * int * int * int * justification * relief * state * string * string * string -> 'a"
]
  

let tests =
  let make_test speed (res, str1, str2) =
    let env  = Type.Env.make () in
    let test () =
      let ty1 = Type.of_string env str1 in
      let ty2 = Type.of_string env str2 in
      let name = Fmt.str "%s ≡ %s" str1 str2 in
      Alcotest.(check int) name res @@ Iter.length (Acic.unifiers env ty1 ty2)
    in
    incr test_cnt ;
    Alcotest.test_case (CCInt.to_string ! test_cnt) speed test
  in
  CCList.map (make_test `Quick) pos_tests @
  CCList.map (make_test `Quick) neg_tests @
  CCList.map (make_test `Slow) slow_tests

let () = add_tests "Acic.unifier" tests

(* do test *)

let () = Alcotest.run
    ~quick_only:false (* Change for slow tests *)
    ~argv:Sys.argv
    "Unifier"
    !all_tests
