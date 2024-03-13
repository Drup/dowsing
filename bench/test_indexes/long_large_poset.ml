let () =
  Logs.set_reporter (Logs.format_reporter ());
  Logs.set_level @@ Some Logs.Info

let long_large_list ~depth ~const_width =
  let types = ref [] in
  let rec aux str k =
    if k = 0 then types := str :: !types
    else if k = depth then (
      for i = 0 to const_width - 1 do
        let str = "c" ^ Int.to_string i in
        aux str (k - 1)
      done;
      let str = "'a0" in
      aux str (k - 1))
    else (
      for i = 0 to const_width - 1 do
        let str = "( " ^ str ^ " -> c" ^ Int.to_string i ^ " )" in
        aux str (k - 1)
      done;
      let str = "( " ^ str ^ " -> 'a" ^ Int.to_string (depth - k + 1) ^ " )" in
      aux str (k - 1))
  in
  aux "" depth;
  !types

let info_from_list l =
  let add_info i str_ty =
    Logs.debug (fun m -> m "Converting to type : %s @." str_ty);
    let ty = Type.outcome_of_string str_ty in
    let lid = LongIdent.Lident (Int.to_string i) in
    {Db.Entry. lid; ty; pkg = "bench"; source_file = Fpath.v "bench" }
  in
  CCList.mapi add_info l |> Iter.of_list

let make_index l =
  let env = Common.Type.Env.make Data in
  Db.create env @@ info_from_list l

let () =
  let t = make_index @@ long_large_list ~depth:4 ~const_width:2 in
  Db.save t @@ Fpath.v "long_large_poset.db"
