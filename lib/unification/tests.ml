(******************** unifiability tests ********************)

let test (str1, str2) =
  let env = Type.Env.make () in
  let ty1 = Type.of_string env str1 in
  let ty2 = Type.of_string env str2 in
  assert (Unification.unifiable env [ ty1, ty2 ])

let tests = [|
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
|]

let () = Array.iter test tests

(******************** non-unifiability tests ********************)

let test (str1, str2) =
  let env = Type.Env.make () in
  let ty1 = Type.of_string env str1 in
  let ty2 = Type.of_string env str2 in
  assert (not @@ Unification.unifiable env [ ty1, ty2 ])

let tests = [|
  "int", "int -> int" ;
  "int", "int * int" ;
  "int", "int list" ;
  "'a -> 'b", "'a * 'b" ;
  "'a -> 'b -> 'c", "'a -> 'b * 'c" ;
|]

let () = Array.iter test tests
