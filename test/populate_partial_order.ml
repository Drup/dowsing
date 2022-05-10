module P = Index.Poset
module Idx = (val Index.(make Feature.all))

let e = Common.Type.Env.make Data
let x = P.init e

let add t =
  Format.printf "%a@." Common.TypeId.pp t;
  P.add x t

let _from_index () =
  let idx = Idx.load @@ Fpath.v Sys.argv.(1) in
  Idx.T.iterid idx.trie add

let _from_list string_types =
  let types =
    CCList.mapi (fun i s -> TypeId.mk i @@ Type.of_string e s) string_types in
  let rec aux tl =
    match tl with
    | [] -> ()
    | t :: q ->
        let _node = P.add x t in
        P.xdot x;
        aux q
  in
  aux types

let () =
  _from_index ();
  Format.printf "Size: %i@." (P.size x);
  P.xdot x

(* let () =
 *    let () =
 *      _from_list
 *        [
 *          "int";
 *          "float";
 *          "int -> int";
 *          "int -> 'a";
 *          "'a -> 'b";
 *          "int * float -> 'a";
 *          "'c -> int";
 *          "float -> 'a";
 *          "int -> ('a -> unit) -> 'a list -> unit";
 *          "unit";
 *        ]
 *    in
 *    P.xdot x *)
