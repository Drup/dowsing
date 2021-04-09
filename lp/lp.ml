let n1, p1 = 0, 2
let n2, p2 = 1, 2

let n1, n2, p1, p2 =
  let aux i = Lp.c @@ CCFloat.of_int i in
  aux n1, aux n2, aux p1, aux p2

let v1 = Lp.var "a" ~integer:true ~lb:0. ~ub:10.
let v2 = Lp.var "b" ~integer:true ~lb:0. ~ub:10.

let problem =
  let open Lp in
  let obj = minimize @@ c 0. in
  let t1 = n1 ++ (p1 *~ v1) in
  let t2 = n2 ++ (p2 *~ v2) in
  let c = t1 =~ t2 in
  make obj [ c ]

let solve () =
  let timer = Timer.make () in
  Timer.start timer ;
  let sol = Lp_glpk.solve problem in
  Timer.stop timer ;
  begin match sol with
  | Ok (obj, xs) ->
      Printf.printf "objective: %.2f\n" obj ;
      Printf.printf "x: %.2f y: %.2f\n"
        (Lp.PMap.find v1 xs) (Lp.PMap.find v2 xs) ;
  | Error msg ->
      print_endline msg
  end ;
  Printf.printf "time: %g\n" @@ Timer.get timer

let () =
  assert (Lp.validate problem) ;
  solve ()
