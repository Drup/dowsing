module P = Index.Poset
module Idx = (val Index.(make Feature.all))

let types =
  [
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
    "'a -> 'a list";
    "(int -> 'a) -> float";
  ]

let info_from_list l =
  let add_info i str_ty =
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
  let t = make_index types in
  Idx.save t @@ Fpath.v "idx_from_list.db"
