module LIdHMap = LongIdent.HMap

type key = LongIdent.t

type info = {
  ty : Type.t ;
}

type infos = info LIdHMap.t

type t = {
  env : Type.Env.t ;
  infos : infos ;
}

let get_env (t : t) = t.env
let get_infos (t : t) = t.infos

let get, add =
  let (%) = CCFun.(%) in
  LIdHMap.get % get_infos,
  LIdHMap.add % get_infos

let iter t fn = LIdHMap.iter fn @@ get_infos t

let make () = {
  env = Type.Env.make () ;
  infos = LIdHMap.create 17 ;
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

let filter t ty =
  let by_head =
    let hd_kind = Type.(kind @@ head ty) in
    fun ty' ->
      let hd_kind' = Type.(kind @@ head ty') in
      hd_kind' = Type.Kind.Var || hd_kind' = hd_kind
  in
  let by_root_var_cnt =
    let root_var_cnt = Type.(size Size.RootVarCount ty) in
    let tl_len = Type.(size Size.TailLength ty) in
    fun ty' ->
      let root_var_cnt' = Type.(size Size.RootVarCount ty') in
      let tl_len' = Type.(size Size.TailLength ty') in
      match root_var_cnt, root_var_cnt' with
      | 0, 0 -> tl_len = tl_len'
      | _, 0 -> tl_len <= tl_len'
      | 0, _ -> tl_len >= tl_len'
      | _ -> true
  in
  t.infos |> LIdHMap.filter_map_inplace (fun _ ({ ty = ty' } as info) ->
    let ok = by_head ty' && by_root_var_cnt ty' in
    CCOpt.return_if ok info
  )

module type Node = sig

  type t

  val make : Unit.t -> t
  val add : t -> info -> Unit.t
  val get : t -> Type.t -> info Iter.t

end

module Leaf = struct

  type t = info List.t ref

  let make () = ref []
  let add t info = t := info :: ! t
  let get t _ = Iter.of_list ! t

end

module ByHead (Child : Node) = struct

  type t = Child.t Type.Kind.Map.t ref

  let make () =
    ref Type.Kind.Map.empty

  let add t info =
    let kind = Type.(kind @@ head info.ty) in
    t := ! t |> Type.Kind.Map.update kind (function
      | None ->
          let child = Child.make () in
          Child.add child info ;
          Some child
      | Some child ->
          Child.add child info ;
          Some child
    )

  let get_child t kind =
    Type.Kind.Map.get_or kind ! t ~default:(Child.make ())

  let get t ty =
    let kind = Type.(kind @@ head ty) in
    if kind = Type.Kind.Var then
      Type.Kind.Map.fold (fun _ child -> Iter.append @@ Child.get child ty) ! t Iter.empty
    else
      let child = get_child t Type.(kind @@ head ty) in
      let var_child = get_child t Type.Kind.Var in
      Iter.append (Child.get child ty) (Child.get var_child ty)

end

module Tree = ByHead (Leaf)

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
