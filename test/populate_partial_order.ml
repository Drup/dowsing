module P = Index.Poset
module Idx = (val Index.(make Feature.all))

let () = Logs.set_reporter (Logs.format_reporter ())
(* Logs.set_level @@ Some Logs.Debug *)

let env = Common.Type.Env.make Data
let x = P.init env

let add t =
  (* Format.printf "%a@." Common.TypeId.pp t; *)
  P.add x t

let _from_index () =
  let idx = Idx.load @@ Fpath.v Sys.argv.(1) in
  Idx.T.iterid idx.trie add

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

(* let () =
   _from_index ();
   Format.printf "Size: %i@." (P.size x);
   Format.printf "Number of edges : %i@." (P.connectivity x);
   P.xdot x *)

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
        (* "int -> float list -> 'a" *)
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
