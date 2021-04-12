module LIdMap = LongIdent.HMap

type key = LongIdent.t

type info = {
  ty : Type.t ;
}

type infos = info LIdMap.t

type t = {
  env : Type.Env.t ;
  infos : infos ;
}

let get_env (t : t) = t.env
let get_infos (t : t) = t.infos

let get, add =
  let (%) = CCFun.(%) in
  LIdMap.get % get_infos,
  LIdMap.add % get_infos

let iter t fn = LIdMap.iter fn @@ get_infos t

let make () = {
  env = Type.Env.make () ;
  infos = LIdMap.create 17 ;
}

let make pkg_dirs =
  let t = make () in
  let env = get_env t in
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
        add t (LongIdent.of_list @@ info.path @ [ info.name ]) { ty }
    | _ -> ()
  ) ;
  t

module Archive = struct

  type index = t

  type t = {
    var_gen : Variable.Gen.t ;
    var_names : String.t Variable.HMap.t ;
    infos : infos ;
  }

  let of_index (idx : index) = {
    var_gen = idx.env.var_gen ;
    var_names = idx.env.var_names ;
    infos = idx.infos ;
  }

  let to_index t : index = {
    env = Type.Env.make () ~var_gen:t.var_gen ~var_names:t.var_names ;
    infos = t.infos ;
  }

  let load file : t =
    CCIO.with_in file Marshal.from_channel

  let save (t : t) file =
    CCIO.with_out file @@ fun out -> Marshal.to_channel out t []

end

let load file = Archive.(to_index @@ load file)
let save t file = Archive.(save (of_index t) file)
