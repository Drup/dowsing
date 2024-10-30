open Cmdliner

let typ env =
  let parse str =
    try Ok (Type.of_string env str)
    with Invalid_argument _ -> Error (`Msg "ill-formed type")
  in
  Arg.conv (parse, Type.pp)

let scheme env =
  let parse str =
    try Ok Scheme.(to_type env @@ of_string env str)
    with Invalid_argument _ -> Error (`Msg "ill-formed type")
  in
  Arg.conv (parse, Type.pp)

let meas_kind =
  let parse str =
    try Ok (Measure.Kind.of_string str)
    with Invalid_argument _ -> Error (`Msg "illegal measure")
  in
  Arg.conv (parse, Measure.Kind.pp)

let path =
  Arg.conv Fpath.(of_string, pp)

let file =
  let parse str =
    Fpath.of_string str
    |> CCResult.flat_map Bos.OS.Path.must_exist
  in
  Arg.conv (parse, Fpath.pp)
