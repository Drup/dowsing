module P = Index__Poset
module Index = (val Index.(make Feature.all))

let e = Common.Type.Env.make Data
let x = P.init e

let add i (t, _) =
  let t = Common.TypeId.mk i t in
  Format.printf "%i: %a@." i Common.TypeId.pp t;
  ignore @@ P.add x t

let _from_index () =
  let idx = Index.load @@ Fpath.v Sys.argv.(1) in
  Index.iter idx |> Iter.iteri add

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

(* let () =
 *   _from_index ();
 *   Format.printf "Size: %i@." (P.size x);
 *   P.xdot x *)

let () =
   let () =
     _from_list
       [
         "'a";
         "int";
         "float";
         "int -> int";
         "int -> 'a";
         "'a -> 'b";
         "int * float -> 'a";
         "'c -> int";
         "float -> 'a";
         "int -> ('a -> unit) -> 'a list -> unit";
         "unit";
       ]
   in
   P.xdot x
