module Trie =
  Trie.Make (
    Trie.Node (Feature.ByHead') (
      Trie.Node (Feature.TailLength) (
        Trie.Leaf
      )
    )
  )

type info = {
  lid : LongIdent.t ;
}

type t = {
  env : Type.Env.t ;
  mutable infos : info Trie.t ;
}

let iter_libindex env pkg_dirs k =
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
        let lid = LongIdent.of_list @@ info.path @ [ info.name ] in
        k (ty, { lid })
    | _ -> ()
  )

let add t (ty, info) =
  t.infos <- Trie.add ty info t.infos

let make pkg_dirs =
  let env = Type.Env.make () in
  let t = { env ; infos = Trie.empty } in
  iter_libindex env pkg_dirs
  |> Iter.iter @@ add t ;
  t

let add t ty lid =
  add t (ty, { lid })

let get_env t = t.env

let iter t =
  Trie.iter @@ t.infos

let iter_with t ty =
  Trie.iter_with ty t.infos

let find t env ty =
  iter_with t ty
  |> Iter.filter_map
    (fun (ty', info) ->
       match Unification.unify env ty ty' with
       | Some unif -> Some (ty', info, unif)
       | None -> None
    )

module Archive = struct

  let of_index = CCFun.id
  let to_index = CCFun.id

  let load file : t =
    CCIO.with_in file Marshal.from_channel

  let save (t : t) file =
    CCIO.with_out file @@ fun out -> Marshal.to_channel out t []

end

let load file = Archive.(to_index @@ load file)
let save t = Archive.(save @@ of_index t)
