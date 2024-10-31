let all_tests = ref []
let test_cnt = ref 0
let add_tests name tests = all_tests := !all_tests @ [ (name, tests) ]

(* Unification with type aliases *)

let type_aliases =
  [
    ("t1", ["a"], "int * 'a");
    ("t2", [], "float t1");
    ("t3", [], "(x t1) t1");
  ]

let pos_tests = [
  ("int * int", "int t1");
  ("t2", "float * 'a");
  ("t3", "x * int * int");
]

let neg_tests = [
  ("int * int", "float t1");
  ("t2", "int * x");
]

let env = Type.Env.make ()

let alias_tbl =
  let h = LongIdent.HMap.create 1 in
  List.iter (fun (s, params, ty) ->
      let vars, ty = Type.of_string' env ty in
      let params =
        List.map (fun s -> Utils.String.HMap.find vars s) params
      in
      let lid = Longident.Lident s in
      TypeAlias.add env h lid params ty)
    type_aliases;
  h

let tests =
  let make_test speed res (str1, str2) =
    let test () =
      let ty1 = TypeAlias.expand_type env alias_tbl @@ Type.of_string env str1 in
      let ty2 = TypeAlias.expand_type env alias_tbl @@ Type.of_string env str2 in
      let name = Fmt.str "%s ≡ %s" str1 str2 in
      Alcotest.(check bool) name res @@ Acic.unifiable env ty1 ty2
    in
    incr test_cnt ;
    Alcotest.test_case (CCInt.to_string ! test_cnt) speed test
  in
  CCList.map (make_test `Quick true) pos_tests @
  CCList.map (make_test `Quick false) neg_tests

let () = add_tests "Unification type alias" tests

(* do test *)

let () = Alcotest.run
    ~quick_only:false (* Change for slow tests *)
    ~argv:Sys.argv
    "TypeAlias"
    !all_tests

