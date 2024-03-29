module P = Db.Internals.Poset
module Idx = Db.DefaultIndex

let () = Logs.set_reporter (Logs.format_reporter ())
(* Logs.set_level @@ Some Logs.Debug *)

let short_large_list ~connect_comp =
  let rec aux_comp i types =
    if i = 0 then types
    else
      let str_const = "c" ^ Int.to_string i in
      let types =
        [
          "'a -> " ^ str_const;
          "'a list -> " ^ str_const;
          str_const ^ " -> " ^ str_const;
        ]
        @ types
      in
      aux_comp (i - 1) types
  in
  aux_comp connect_comp []

let info_from_list l =
  let add_info i str_ty =
    Logs.debug (fun m -> m "Converting to type : %s @." str_ty);
    let ty = Type.outcome_of_string str_ty in
    let lid = LongIdent.Lident (Int.to_string i) in
    {Db.Entry. lid; ty; pkg = "bench"; source_file = Fpath.v "bench" }
  in
  CCList.mapi add_info l |> Iter.of_list

let make_index l =
  let env = Common.Type.Env.make () in
  Db.create ~with_poset:true env @@ info_from_list l

let () =
  let t = make_index @@ short_large_list ~connect_comp:5 in
  Db.save t @@ Fpath.v "short_large_poset.db"
