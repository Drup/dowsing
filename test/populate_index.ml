let () =
  Logs.set_reporter (Logs.format_reporter ());
  Logs.set_level @@ Some Logs.Info

let types =
  [
    "int";
    "float";
    "int -> int";
    "int -> 'a";
    "'a -> 'b";
    "int -> float list -> 'a";
    "float list -> int -> float list";
    "float -> int";
    "float -> float list";
    "'c -> int";
    "float -> 'a";
    "int -> ('a -> unit) -> 'a list -> unit";
    "unit";
    "'a -> 'a list";
    "(int -> 'a) -> float"
  ]

let info_from_list l =
  let add_info i str_ty =
    let ty = Type.outcome_of_string str_ty in
    let lid = LongIdent.Lident (Int.to_string i) in
    { Db.Entry. lid; ty; pkg = "test" ; source_file = Fpath.v "test" }
  in
  CCList.mapi add_info l |> Iter.of_list

let make_db l =
  let env = Common.Type.Env.make () in
  Db.create ~with_poset:true env @@ info_from_list l

let () =
  let t = make_db types in
  Db.save t @@ Fpath.v "idx_from_list.db"
