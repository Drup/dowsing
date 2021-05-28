exception Error of String.t
let error pkg = raise @@ Error pkg

let () = Findlib.init ()

let wrap pkgs =
  try
    pkgs ()
    |> Iter.of_list
    |> Iter.map Findlib.package_directory
    (* we need this because [Findlib.list_packages'] is faulty: it gives some unknown packages *)
    |> Iter.filter Sys.file_exists
    |> Iter.sort_uniq ~cmp:CCString.compare
    |> Iter.map Fpath.v
    |> Iter.to_list
  with
  | Findlib.No_such_package (pkg, _) ->
      error pkg

let find pkgs =
  wrap (fun () -> Findlib.package_deep_ancestors [] pkgs)

let find_all () =
  wrap Findlib.list_packages'
