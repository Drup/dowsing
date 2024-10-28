let all_tests = ref []
let test_cnt = ref 0
let add_tests name tests =
  all_tests := ! all_tests @ [ name, tests ]

(* Acic.unifiable *)

let pos_tests = [
  (* com-2 *)
  "a * b", "b * a" ;
  "int * float", "float * int";
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
  (* currification + unification *)
  "a -> 'a", "a -> b -> c" ;
  "a * b -> c", "a -> 'a" ;
  "'a -> 'a", "(x -> y) * x -> y" ;
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
  "'a * int", "bool * bool * 'b";
  "('a * 'b) -> ('a -> 'z) -> ('b -> 'y) -> unit", "('a * 'b) -> ('a -> 'z) -> ('b -> 'y) -> unit";
  "int list * int -> 'a", "int list * int -> 'a";
  (* sub constructor *)
  "int list -> 'a", "int list -> int";
  (* Tuples target of variable *)
  "'a * 'b -> 'a", "int * unit * float -> int * unit";
  (* Loop *)
  "'a -> 'a", "'b -> 'b";
  "'a -> 'a -> 'a", "'x * b -> 'x" ;
  (* Bug with non-proper equations *)
  "('b, 'a, 'a, int, 'a, 'b) t", "('b, 'a, int, 'a, 'b, 'a) t";
  (* Bug with only the ACU no modif to syntactic, does not find the minimum solution *)
  "'a * 'a f", "'a * 'b * int f";
  (* Need ACU *)
  "int * float", "'a * 'b * 'c";
  "'a * float", "float";
  "'a -> 'b", "'a * 'b" ;
  (* Bug non-arrow *)
  "((float -> int) * float * int, int -> 'b) t", "('b * int, int -> 'b) t";
  (* Bug occur check collapse *)
  "'a * 'a -> 'a", "('c * 'd * 'e * 'f * (('c * 'e -> 'a)) * (('d * 'f -> 'b)) -> ('a * 'b))"
]

let neg_tests = [
  (* cur-0 *)
  "unit -> a", "a" ;
  "unit -> unit -> a", "a" ;
  "unit * unit -> a", "a" ;
  "(unit -> a) * b", "a * b" ;
  "(unit -> a) f", "a f" ;
  (* currification + unification *)
  "a -> 'a", "x -> b -> c" ;
  "x * b -> c", "a -> 'a" ;
  "'a -> 'a", "(x -> y) * z -> y" ;
  (* other *)
  "a", "a -> a" ;
  "a", "a * a" ;
  "a", "a f" ;
  "'a f * a", "'x g * a" ;
  "'a f -> 'b g -> a", "'x h * 'y -> 'x" ;
  "'a -> 'a -> a", "'x * b -> 'x" ;
  "'a * float * 'a", "int * 'a * 'a" ;
  "int * int * int", "int * int";
  "int * int * int * 'a", "int * int";
  (* require occur check *)
  "'X -> 'X", "'a * ('a, 'b) t * ('a, 'b) t -> 'a option * ('a, 'b) t * ('a, 'b) t" ;
  "'a -> 'a list -> 'a", "'x -> 'x -> 'x" ;
  "('a * unit * string * 'b t) format4 -> 'a","('a * Buffer.t * unit * ('b * string) t) format4 -> 'a";
  (* bug in occur check *)
  "(int, 'b, 'a, 'b, 'a) t", "('b, 'a, 'b * 'b, 'b, 'a) t";
]

let slow_tests = [
  "'A * int list -> unit",
  "anchor * bitmap * bool * bool * color * color * color * color * color * color * color * color * cursor *  int * int * int * int * int * int * int * int * justification * relief * state * string * string * string -> 'a"
]

let marked_var_pos_tests = [
  ">a. 'a * int", "int * float * float";
  "*a. 'a * int", " (int -> string) * int";
  ">a. 'a * int", " (int -> string) * int * float";
]

let marked_var_neg_tests = [
  "*a. 'a * int", "int * float * float";
  ">a. 'a * int", " (int -> string) * int";
]
  

let tests =
  let make_test speed res (str1, str2) =
    let env  = Type.Env.make () in
    let test () =
      let ty1 = Type.of_string env str1 in
      let ty2 = Type.of_string env str2 in
      let name = Fmt.str "%s ≡ %s" str1 str2 in
      Alcotest.(check bool) name res @@ Acic.unifiable env ty1 ty2
    in
    incr test_cnt ;
    Alcotest.test_case (CCInt.to_string ! test_cnt) speed test
  in
  let make_marked_test speed res (str1, str2) =
    let env  = Type.Env.make () in
    let test () =
      let ty1 = Scheme.of_string env str1 |> Scheme.to_type env in
      let ty2 = Scheme.of_string env str2 |> Scheme.to_type env in
      let name = Fmt.str "%s ≡ %s" str1 str2 in
      Alcotest.(check bool) name res @@ Acic.unifiable env ty1 ty2
    in
    incr test_cnt ;
    Alcotest.test_case (CCInt.to_string ! test_cnt) speed test
  in
  CCList.map (make_test `Quick true) pos_tests @
  CCList.map (make_test `Quick false) neg_tests @
  CCList.map (make_test `Slow true) slow_tests @
  CCList.map (make_marked_test `Quick true) marked_var_pos_tests @
  CCList.map (make_marked_test `Quick false) marked_var_neg_tests

let () = add_tests "Acic.unifiable" tests

(* do test *)

let () = Alcotest.run
    ~quick_only:false (* Change for slow tests *)
    ~argv:Sys.argv
    "Unifiable"
    !all_tests
