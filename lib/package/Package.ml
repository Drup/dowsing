exception Error of String.t
let error pkg = raise @@ Error pkg

let () = Findlib.init ()

let wrap pkgs =
  try
    pkgs ()
    |> CCList.filter_map (fun pkg ->
      let pkg_dir = Findlib.package_directory pkg in
      if Sys.file_exists pkg_dir
      then Some (pkg, Fpath.v pkg_dir)
      else None
    )
  with
  | Findlib.No_such_package (pkg, _)
  | Findlib.Package_loop pkg ->
      error pkg

let find pkgs =
  wrap (fun () -> Findlib.package_deep_ancestors [] pkgs)

let find_all () =
  wrap Findlib.list_packages'

type info = {
  orig_lid : LongIdent.t ;
  lid : LongIdent.t ;
  out_ty : Outcometree.out_type ;
}

(* temporary fix to avoid issues with LibIndex and name mangling *)
let () = Printtyp.Naming_context.enable false

let iter pkg_dir k =
  [ Fpath.to_string pkg_dir ]
  |> LibIndex.Misc.unique_subdirs
  |> LibIndex.load
  |> LibIndex.all
  |> CCList.iter @@ fun info ->
    match info.LibIndex.kind with
    | LibIndex.Value ->
        let orig_lid = LongIdent.of_list @@ info.orig_path @ [ info.name ] in
        let lid = LongIdent.of_list @@ info.path @ [ info.name ] in
        let [@warning "-8"] Outcometree.Osig_value out_ty = Option.get info.ty in
        let out_ty = out_ty.oval_type in
        k { orig_lid ; lid ; out_ty }
    | _ -> ()
