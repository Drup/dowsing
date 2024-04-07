module P = Db.Internals.Poset
module Idx = Db.DefaultIndex

let () = Logs.set_reporter (Logs.format_reporter ())
(* Logs.set_level @@ Some Logs.Debug *)

let env = Common.Type.Env.make ()
let x = P.init env

let _from_list string_types =
  let types =
    CCList.mapi (fun i s -> TypeId.mk i @@ Type.of_string env s) string_types
  in
  let rec aux tl =
    match tl with
    | [] -> ()
    | t :: q ->
        let _node = P.add x t in
        (* P.xdot x; *)
        aux q
  in
  aux types

let () =
  let () =
    _from_list
      [
        "int";
        "float";
        "int -> int";
        "int -> 'a";
        "'a -> 'b";
        "'a * 'c -> 'b";
        "int -> float list -> 'a";
        "float list -> int -> float list";
        "float -> int";
        "float -> float list";
        "'c -> int";
        "float -> 'a";
        "int -> ('a -> unit) -> 'a list -> unit";
        "unit";
        "'a -> 'a list";
        "(int -> 'a) -> float";
      ]
  in
  P.xdot x
