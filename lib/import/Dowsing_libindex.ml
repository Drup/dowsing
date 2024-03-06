
(* temporary fix to avoid issues with LibIndex and name mangling *)
let () = Printtyp.Naming_context.enable false

let iter pkg_dir k =
  pkg_dir
  |> Fpath.to_string
  |> (fun l -> [l])
  |> LibIndex.Misc.unique_subdirs
  |> LibIndex.load ~qualify:true
  |> LibIndex.all
  |> CCList.iter @@ fun info ->
    match info.LibIndex.kind with
    | LibIndex.Value ->
        let orig_lid = LongIdent.of_list @@ info.orig_path @ [ info.name ] in
        let lid = LongIdent.of_list @@ info.path @ [ info.name ] in
        let [@warning "-8"] Outcometree.Osig_value out_ty = Option.get info.ty in
        let ty = out_ty.oval_type in
        k (orig_lid, {Index.Info. lid ; ty ; pkg_dir})
    | _ -> ()
