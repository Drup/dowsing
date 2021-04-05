module LIdMap = LongIdent.HMap

type key = LongIdent.t

type info = {
  ty : Type.t ;
}

type t = info LIdMap.t

let get = LIdMap.get
let add = LIdMap.add
let iter = LIdMap.iter
let make () = LIdMap.create 17

let make pkg_dirs =
  let env = Type.Env.make () in
  let idx = make () in
  pkg_dirs
  |> LibIndex.Misc.unique_subdirs
  |> LibIndex.load
  |> LibIndex.all
  |> List.iter (fun info ->
    match info.LibIndex.kind with
    | LibIndex.Value ->
        let [@warning "-8"] Outcometree.Osig_value out_ty = Option.get info.ty in
        let out_ty = out_ty.oval_type in
        let ty = Type.of_outcometree env out_ty in
        add idx (LongIdent.of_list @@ info.path @ [ info.name ]) { ty }
    | _ -> ()) ;
  idx

let load file_name =
  CCIO.with_in file_name Marshal.from_channel

let save self file_name =
  CCIO.with_out file_name @@ fun out -> Marshal.to_channel out self []
