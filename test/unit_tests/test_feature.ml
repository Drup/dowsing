let all_tests = ref []
let test_cnt = ref 0
let add_tests name tests = all_tests := !all_tests @ [ (name, tests) ]

(* Feature.Head *)

let pos_head =
  [
    (*two variables*)
    ("float * (int * int list -> int list) -> 'a", "'a -> 'b");
    (*one variable*)
    ("float * int * int list -> bool", "'a");
    (*no variable*)
    ("int -> int", "float * int list -> bool -> int");
    ("float", "float");
    ("'a list -> int list", "'a -> 'b list");
    ("int -> (int * int list)", "bool -> int list * int");
  ]

let neg_head =
  [
    ("float", "int");
    ("int -> (int -> int)", "int * float -> float");
    ("'a -> 'a list", "float");
  ]

let head =
  let make_test speed res (str1, str2) =
    let env_query = Type.Env.make Query in
    let env = Type.Env.make Data in
    let test () =
      let module Ft = Index.Feature.Head in
      let ty1 = Type.of_string env_query str1 in
      let ty2 = Type.of_string env str2 in
      let feat1 = Ft.compute ty1 and feat2 = Ft.compute ty2 in
      Logs.debug (fun m ->
          m "%a for type %a : %a @." Index.Feature.pp
            (module Ft)
            Type.pp ty1 Ft.pp feat1);
      Logs.debug (fun m ->
          m "%a for type %a : %a @." Index.Feature.pp
            (module Ft)
            Type.pp ty2 Ft.pp feat2);
      let name = Fmt.str "%s ≡ %s" str1 str2 in
      Alcotest.(check bool) name res @@ Ft.compatible ~query:feat1 ~data:feat2
    in
    incr test_cnt;
    Alcotest.test_case (CCInt.to_string !test_cnt) speed test
  in
  CCList.map (make_test `Quick true) pos_head
  @ CCList.map (make_test `Quick false) neg_head

let () = add_tests "Feature.Head" head

(* Feature.Tail *)

let pos_tail =
  [
    (*two variables*)
    ("'a -> 'a -> bool", "'a -> 'b list");
    (*one variable*)
    ("'a list -> int list", "'a -> 'b list");
    ("float * int * int list -> bool", "'a");
    ("bool * 'a -> int list * int", "bool -> (int -> int list)");
    (*no variable*)
    ("int -> int -> int", "int * int -> int");
    ("float", "int");
  ]

let neg_tail =
  [
    (*one variable*)
    ("'a * int -> 'a list", "float");
    (*no variable*)
    ("float", "int -> int");
    ("int -> (int -> int)", "int -> float");
  ]

let tail =
  let make_test speed res (str1, str2) =
    let env_query = Type.Env.make Query in
    let env = Type.Env.make Data in
    let test () =
      let module Ft = Index.Feature.Tail in
      let ty1 = Type.of_string env_query str1 in
      let ty2 = Type.of_string env str2 in
      let feat1 = Ft.compute ty1 and feat2 = Ft.compute ty2 in
      Logs.debug (fun m ->
          m "%a for type %a : %a @." Index.Feature.pp
            (module Ft)
            Type.pp ty1 Ft.pp feat1);
      Logs.debug (fun m ->
          m "%a for type %a : %a @." Index.Feature.pp
            (module Ft)
            Type.pp ty2 Ft.pp feat2);
      let name = Fmt.str "%s ≡ %s" str1 str2 in
      Alcotest.(check bool) name res @@ Ft.compatible ~query:feat1 ~data:feat2
    in
    incr test_cnt;
    Alcotest.test_case (CCInt.to_string !test_cnt) speed test
  in
  CCList.map (make_test `Quick true) pos_tail
  @ CCList.map (make_test `Quick false) neg_tail

let () = add_tests "Feature.Tail" tail

(* Feature.Constructors *)

let pos_const =
  [
    (*two variables*)
    ("'a -> 'a -> bool", "'a -> 'b list");
    ("'a", "int -> 'a");
    (*one variable*)
    ("int list -> int list", "'a -> 'b list");
    ("float * int * int list -> bool", "'a");
    ("bool * ('a -> int list) -> int", "(bool -> int) -> int list");
    (*no variable*)
    ("int -> int -> int", "int * int -> int");
    ("int -> int list", "int list -> int");
  ]

let neg_const =
  [
    (*one variable*)
    ("'a * int -> 'a list", "float");
    (*no variable*)
    ("float", "int -> int");
    ("int -> (int -> int)", "int -> float");
    ("float * float -> float", "(float -> float) -> float");
  ]

let const =
  let make_test speed res (str1, str2) =
    let env_query = Type.Env.make Query in
    let env = Type.Env.make Data in
    let test () =
      let module Ft = Index.Feature.Constructors in
      let ty1 = Type.of_string env_query str1 in
      let ty2 = Type.of_string env str2 in
      let feat1 = Ft.compute ty1 and feat2 = Ft.compute ty2 in
      Logs.debug (fun m ->
          m "%a for type %a : %a @." Index.Feature.pp
            (module Ft)
            Type.pp ty1 Ft.pp feat1);
      Logs.debug (fun m ->
          m "%a for type %a : %a @." Index.Feature.pp
            (module Ft)
            Type.pp ty2 Ft.pp feat2);
      let name = Fmt.str "%s ≡ %s" str1 str2 in
      Alcotest.(check bool) name res @@ Ft.compatible ~query:feat1 ~data:feat2
    in
    incr test_cnt;
    Alcotest.test_case (CCInt.to_string !test_cnt) speed test
  in
  CCList.map (make_test `Quick true) pos_const
  @ CCList.map (make_test `Quick false) neg_const

let () = add_tests "Feature.Constructors" const

(* do test *)

let () =
  Alcotest.run ~quick_only:true (* Change for slow tests *)
    ~argv:Sys.argv "features" !all_tests
