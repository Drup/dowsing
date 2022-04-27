module P = Index__Poset.Poset
module Index = (val Index.(make Feature.all))

let e = Common.Type.Env.make Data
let x = P.init e

let add i (t, _) =
  Format.printf "%i: %a@." i Common.Type.pp t;
  ignore @@ P.add x t

let _from_index () =
  let idx = Index.load @@ Fpath.v Sys.argv.(1) in
  Index.iter idx |> Iter.iteri add

let _from_list string_types =
  let types = CCList.map (Type.of_string e) string_types in
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
  let () =
    _from_list
      [
        (* "'a"; *)
        "int";
        "float";
        "int -> int";
        "int -> 'a";
        "'a -> 'b";
        "int * float -> 'a";
        "'c -> int";
      ]
  in
  P.xdot x
