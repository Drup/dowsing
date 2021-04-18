module Trie =
  Trie.Make (
    Trie.Node (Feature.ByHead) (
      Trie.Node (Feature.TailLength) (
        Trie.Leaf
      )
    )
  )

type info = LongIdent.t

type t = {
  env : Type.Env.t ;
  infos : info Trie.t ;
}

let iter_libindex pkg_dirs env k =
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
        k (ty, lid)
    | _ -> ()
  )

let make pkg_dirs =
  let env = Type.Env.make () in
  let it = iter_libindex pkg_dirs env in
  let infos =
    Iter.fold (fun trie (ty, info) ->
      Trie.add ty info trie
    ) Trie.empty it
  in
  { env ; infos }

let get_env t = t.env

let iter, iteri =
  let aux fn t = fn @@ t.infos in
  aux Trie.iter, aux Trie.iteri

let iter', iteri' =
  let aux fn t env ty = fn env ty @@ t.infos in
  aux Trie.iter', aux Trie.iteri'

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
