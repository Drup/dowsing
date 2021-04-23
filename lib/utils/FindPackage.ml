exception Error of String.t

let error pkg = raise @@ Error pkg

let () = Findlib.init ()

let wrap pkgs =
  try
    pkgs ()
    |> CCList.map Findlib.package_directory
    (* we need this because [Findlib.list_packages'] is faulty: it gives some unknown packages *)
    |> CCList.filter Sys.file_exists
    |> CCList.sort_uniq ~cmp:CCString.compare
  with
  | Findlib.No_such_package (pkg, _) ->
      error pkg

let find pkgs =
  wrap (fun () -> Findlib.package_deep_ancestors [] pkgs)

let find_all () =
  wrap Findlib.list_packages'
