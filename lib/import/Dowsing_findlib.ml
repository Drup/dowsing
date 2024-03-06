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
