module Cell = Cell

module Trie =
  Trie.Node (Feature.Head') (
    Trie.Node (Feature.Tail') (
      Trie.Leaf
    )
  )

module StringHMap = CCHashtbl.Make (CCString)
module StringSet = CCSet.Make (CCString)

type t = {
  hcons : Type.Hashcons.t ;
  mutable trie : Trie.t ;
  pkgs_dirs : String.t StringHMap.t ;
  cells : (Int.t * Cell.t Type.Map.t) StringHMap.t ;
}

type iter = (Type.t * Cell.t) Iter.t
type iter_with_unifier = (Type.t * Cell.t * Unification.Subst.t) Iter.t

let make () = {
  hcons = Type.Hashcons.make () ;
  trie = Trie.empty ;
  pkgs_dirs = StringHMap.create 17 ;
  cells = StringHMap.create 17 ;
}

let remove t pkg =
  let pkg_dir = StringHMap.find t.pkgs_dirs pkg in
  snd @@ StringHMap.find t.cells pkg_dir
  |> Type.Map.iter (fun ty _ -> t.trie <- Trie.remove ty t.trie) ;
  StringHMap.remove t.pkgs_dirs pkg ;
  StringHMap.update t.cells ~k:pkg_dir ~f:(fun _ -> function
    | None -> assert false
    | Some (cnt, cells) ->
        if cnt = 1
        then None
        else Some (cnt - 1, cells)
  )

(* temporary fix to avoid issues with LibIndex and name mangling *)
let () = Printtyp.Naming_context.enable false

let iter_libindex hcons pkg_dir k =
  [ pkg_dir ]
  |> LibIndex.Misc.unique_subdirs
  |> LibIndex.load
  |> LibIndex.all
  |> CCList.iter @@ fun info ->
    match info.LibIndex.kind with
    | LibIndex.Value ->
        let [@warning "-8"] Outcometree.Osig_value out_ty = Option.get info.ty in
        let out_ty = out_ty.oval_type in
        let env = Type.Env.make Data ~hcons in
        let ty = Type.of_outcometree env out_ty in
        let lid = LongIdent.of_list @@ info.path @ [ info.name ] in
        let orig_lid = LongIdent.of_list @@ info.orig_path @ [ info.name ] in
        k (ty, orig_lid, Info.{ lid ; out_ty })
    | _ -> ()

let add =
  let aux t cells (ty, lid, info) =
    t.trie <- Trie.add ty t.trie ;
    Type.Map.update ty (CCFun.compose (Cell.update lid info) CCOpt.return) cells
  in
  fun t pkg pkg_dir ->
    if StringHMap.mem t.pkgs_dirs pkg then
      remove t pkg ;
    let pkg_dir = Fpath.to_string pkg_dir in
    let cells =
      iter_libindex t.hcons pkg_dir
      |> Iter.fold (aux t) Type.Map.empty
    in
    StringHMap.add t.pkgs_dirs pkg pkg_dir ;
    StringHMap.update t.cells ~k:pkg_dir ~f:(fun _ -> function
      | None -> Some (1, cells)
      | Some (cnt, _) -> Some (cnt + 1, cells)
    )

let iter t = Trie.iter t.trie
let iter_with t ty = Trie.iter_with ty t.trie

let find, find_with =
  let aux iter t env ty =
    iter t ty
    |> Iter.filter_map @@ fun ty' ->
       Unification.unify env ty ty'
       |> CCOpt.map @@ CCPair.make ty'
  in
  aux (fun t _ -> iter t), aux iter_with

let wrap ~to_type ~merge ?(filt = CCFun.const true) t iter =
  iter
  |> Iter.flat_map (fun elt ->
    let ty = to_type elt in
    t.cells
    |> StringHMap.to_iter
    |> Iter.filter_map (fun (pkg_dir, (_, cells)) ->
      if filt pkg_dir then
        Type.Map.get ty cells
        |> CCOpt.map @@ merge elt
      else None
    )
  )

let filt t pkgs =
  let set = ref StringSet.empty in
  pkgs |> CCList.iter (fun pkg ->
    let pkg_dir = StringHMap.find t.pkgs_dirs pkg in
    set := StringSet.add pkg_dir ! set
  ) ;
  fun pkg -> StringSet.mem pkg ! set

let iter, iter_with =
  let aux ?pkgs t iter =
    let filt = CCOpt.map (filt t) pkgs in
    wrap t iter ~to_type:CCFun.id ~merge:CCPair.make ?filt
  in
  (fun ?pkgs t -> aux ?pkgs t @@ iter t),
  (fun ?pkgs t ty -> aux ?pkgs t @@ iter_with t ty)

let find, find_with =
  let aux find ?pkgs t env ty =
    let filt = CCOpt.map (filt t) pkgs in
    find t env ty
    |> wrap t ~to_type:fst ~merge:(fun (ty, unif) cell -> ty, cell, unif) ?filt
  in
  aux find, aux find_with

module Archive = struct

  let of_index = CCFun.id
  let to_index = CCFun.id

  let load file : t =
    CCIO.with_in (Fpath.to_string file) Marshal.from_channel

  let save (t : t) file =
    CCIO.with_out (Fpath.to_string file) @@ fun out ->
      Marshal.to_channel out t []

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
