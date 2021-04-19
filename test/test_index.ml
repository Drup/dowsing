let lid_testable = Alcotest.testable LongIdent.pp LongIdent.equal

let all_tests = ref []
let test_cnt = ref 0
let add_tests name bdgs queries =
  let idx = Index.make [] in
  let env = Index.get_env idx in
  bdgs |> CCList.iter (fun (id, ty) ->
    let ty = Type.of_string env ty in
    Index.add idx ty @@ LongIdent.Lident id
  ) ;
  let tests =
    queries |> CCList.map (fun (ty, res) ->
      let ty = Type.of_string env ty in
      let res = CCList.map (fun id -> LongIdent.Lident id) res in
      let test () =
        ty
        |> Index.find idx env
        |> Iter.map (fun (_, Index.{ lid }) -> lid)
        |> Iter.to_list
        |> Alcotest.(check @@ list lid_testable) "" res
      in
      Alcotest.test_case (CCInt.to_string ! test_cnt) `Quick test
    )
  in
  all_tests := ! all_tests @ [ name, tests ]

(* test 1 *)

let () = add_tests "trivial"
  [
    "int", "int" ;
    "float", "float" ;
  ] [
    "int", [ "int" ] ;
    "'a", [ "float" ; "int" ] ;
  ]

(* do test *)

let () = Alcotest.run "index" ! all_tests
