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

let mk_entry i str_ty =
  let ty = Type.outcome_of_string str_ty in
  let lid = LongIdent.Lident (Int.to_string i) in
  { Db.Entry. lid; desc = Val ty; pkg = "test" ; source_file = Fpath.v "test" }

let make_db l =
  let env = Common.Type.Env.make () in
  let db = Db.create ~with_poset:true env in
  List.iteri (fun i ty -> Db.add env db @@ mk_entry i ty) l;
  db

let () =
  let t = make_db types in
  Db.save t @@ Fpath.v "idx_from_list.db"
