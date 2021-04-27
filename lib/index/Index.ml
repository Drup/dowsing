module Trie =
  Trie.Make (
    Trie.Node (Feature.Head') (
      Trie.Node (Feature.Tail') (
        Trie.Leaf
      )
    )
  )

type info = {
  lid : LongIdent.t ;
}
let compare_info i1 i2 = LongIdent.compare i1.lid i2.lid
let pp_info fmt p = LongIdent.pp fmt p.lid

type t = {
  mutable infos : info Trie.t ;
}

type iter = (Type.t * info list) Iter.t
type iter' = (Type.t * info list * Unification.Subst.t) Iter.t

let iter_libindex hcons pkgs_dirs k =
  pkgs_dirs
  |> LibIndex.Misc.unique_subdirs
  |> LibIndex.load
  |> LibIndex.all
  |> List.iter (fun info ->
    match info.LibIndex.kind with
    | LibIndex.Value ->
        let [@warning "-8"] Outcometree.Osig_value out_ty = Option.get info.ty in
        let out_ty = out_ty.oval_type in
        let env = Type.Env.from_hashcons `Data hcons in
        let ty = Type.of_outcometree env out_ty in
        let lid = LongIdent.of_list @@ info.path @ [ info.name ] in
        k (ty, { lid })
    | _ -> ()
  )

let add t (ty, info) =
  t.infos <- Trie.add ty info t.infos

let make pkgs_dirs =
  let hcons = Type.Hashcons.make () in
  let t = { infos = Trie.empty } in
  iter_libindex hcons pkgs_dirs
  |> Iter.iter @@ add t ;
  t

let add t ty lid =
  add t (ty, { lid })

let iter t = Trie.iter t.infos
let iter_with t ty = Trie.iter_with ty t.infos

let find, find_with =
  let aux iter t env ty =
    iter t ty
    |> Iter.filter_map (fun (ty', info) ->
       match Unification.unify env ty ty' with
       | Some unif -> Some (ty', info, unif)
       | None -> None
    )
  in
  aux (fun t _ -> iter t), aux iter_with

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

module Explorer = struct

  exception Error
  let error () = raise Error

  type index = t

  type t = {
    idx_sz : Int.t ;
    its : iter Stack.t ;
  }

  let make idx =
    let it = iter idx in
    let idx_sz = Iter.length it in
    let its = Stack.create () in
    Stack.push it its ;
    { idx_sz ; its }

  let iter t =
    Stack.top t.its

  let select t filt =
    let filt = CCPair.fst_map filt in
    Stack.push (Iter.filter filt @@ iter t) t.its

  let unselect t =
    if Stack.length t.its = 1 then
      error () ;
    ignore @@ Stack.pop t.its

  let pp fmt t =
    let sz = Iter.length @@ iter t in
    Fmt.pf fmt "{ explored subset: %i (%g %%) }"
      sz
      (CCFloat.of_int sz /. CCFloat.of_int t.idx_sz *. 100.)

end
