open Longident
open Type

let test_equal ty1 ty2 =
  assert (ty1 = ty2)

let int = Constr (Lident "int", [||])

(******************** of_string tests ********************)

let test (str, ty) =
  test_equal (of_string (Env.make ()) str) ty

let tests = [|
  "int", int ;
  "int -> int", Arrow (Set.of_list [ int ], int) ;
  "unit -> int", int ;
  "int * int", Tuple (Set.of_list [ int ; int ]) ;
  "int * int -> int", Arrow (Set.of_list [ int ; int ], int) ;
  "int -> int -> int", Arrow (Set.of_list [ int ; int ], int) ;
|]

let () = Array.iter test tests
