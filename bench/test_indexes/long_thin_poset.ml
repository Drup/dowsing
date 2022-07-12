module P = Index.Poset
module Idx = (val Index.(make Feature.all))

let () = Logs.set_reporter (Logs.format_reporter ())
(* Logs.set_level @@ Some Logs.Debug *)

let long_thin_list ~depth ~connect_comp =
  let rec aux_depth constr k l =
    if k = 0 then l
    else
      match l with
      | [] -> aux_depth constr (k - 1) [ constr ^ " -> 'a" ]
      | h :: t ->
          let str = constr ^ " * " ^ h in
          aux_depth constr (k - 1) (str :: h :: t)
  in
  let rec aux_comp i types =
    if i = 0 then types
    else
      let types = aux_depth ("c" ^ Int.to_string i) depth [] @ types in
      aux_comp (i - 1) types
  in
  aux_comp connect_comp []

let info_from_list l =
  let add_info i str_ty =
    Logs.debug (fun m -> m "Converting to type : %s @." str_ty);
    let out_ty = Type.outcome_of_string str_ty in
    let lid = LongIdent.Lident (Int.to_string i) in
    { Package.orig_lid = lid; lid; out_ty }
  in
  CCList.mapi add_info l |> Iter.of_list

let make_index l =
  let env = Common.Type.Env.make Data in
  let t = Idx.make env in
  let infos = ("No package", Fpath.v "No package", info_from_list l) in
  Idx.import t [ infos ];
  t

let () =
  let t = make_index @@ long_thin_list ~depth:10 ~connect_comp:3 in
  Idx.save t @@ Fpath.v "long_thin_poset.db"
