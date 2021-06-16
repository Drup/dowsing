module Cell = Cell

module Trie =
  Trie.Node (Feature.Head') (
    Trie.Node (Feature.Tail') (
      Trie.Leaf
    )
  )

type t = {
  hcons : Type.Hashcons.t ;
  mutable trie : Trie.t ;
  pkgs_dirs : Fpath.t String.HMap.t ;
  mutable cells : (Int.t * Cell.t Type.Map.t) Fpath.Map.t ;
}

type iter = (Type.t * Cell.t) Iter.t
type iter_with_unifier = (Type.t * Cell.t * Subst.t) Iter.t

let make () = {
  hcons = Type.Hashcons.make () ;
  trie = Trie.empty ;
  pkgs_dirs = String.HMap.create 17 ;
  cells = Fpath.Map.empty ;
}

let remove t pkg =
  let pkg_dir = String.HMap.find t.pkgs_dirs pkg in
  t.cells
  |> Fpath.Map.get pkg_dir
  |> snd
  |> Type.Map.iter (fun ty _ -> t.trie <- Trie.remove ty t.trie) ;
  String.HMap.remove t.pkgs_dirs pkg ;
  t.cells <-
    t.cells |> Fpath.Map.update pkg_dir @@ function
      | None -> assert false
      | Some (cnt, cells) ->
          if cnt = 1
          then None
          else Some (cnt - 1, cells)

let add =
  let aux t cells Package.{ orig_lid ; lid ; out_ty } =
    let env = Type.Env.make Data ~hcons:t.hcons in
    let ty = Type.of_outcometree env out_ty in
    let info = Info.{ lid ; out_ty } in
    t.trie <- Trie.add ty t.trie ;
    Type.Map.update ty (CCFun.compose (Cell.update orig_lid info) CCOpt.return) cells
  in
  fun t pkg pkg_dir ->
    if String.HMap.mem t.pkgs_dirs pkg then
      remove t pkg ;
    let cells =
      Package.iter [ pkg_dir ]
      |> Iter.fold (aux t) Type.Map.empty
    in
    String.HMap.add t.pkgs_dirs pkg pkg_dir ;
    t.cells <-
      t.cells |> Fpath.Map.update pkg_dir @@ function
        | None -> Some (1, cells)
        | Some (cnt, _) -> Some (cnt + 1, cells)

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

let wrap ~to_type ~merge ?(pkg_filt = CCFun.const true) t iter =
  iter
  |> Iter.flat_map (fun elt ->
    let ty = to_type elt in
    (fun fn -> Fpath.Map.iter (CCFun.curry fn) t.cells)
    |> Iter.filter_map (fun (pkg_dir, (_, cells)) ->
      if pkg_filt pkg_dir then
        Type.Map.get ty cells
        |> CCOpt.map @@ merge elt
      else None
    )
  )

let pkg_filt t pkgs =
  let set = ref Fpath.Set.empty in
  pkgs |> CCList.iter (fun pkg ->
    let pkg_dir = String.HMap.find t.pkgs_dirs pkg in
    set := Fpath.Set.add pkg_dir !set
  ) ;
  fun pkg -> Fpath.Set.mem pkg !set

let iter, iter_with =
  let aux ?pkgs t iter =
    let pkg_filt = CCOpt.map (pkg_filt t) pkgs in
    wrap t iter ~to_type:CCFun.id ~merge:CCPair.make ?pkg_filt
  in
  (fun ?pkgs t -> aux ?pkgs t @@ iter t),
  (fun ?pkgs t ty -> aux ?pkgs t @@ iter_with t ty)

let find, find_with =
  let aux find ?pkgs t env ty =
    let pkg_filt = CCOpt.map (pkg_filt t) pkgs in
    find t env ty
    |> wrap t ~to_type:fst ~merge:(fun (ty, unif) cell -> ty, cell, unif) ?pkg_filt
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

  let pp ppf t =
    let sz = Iter.length @@ iter t in
    Fmt.pf ppf "{ explored subset: %i (%g %%) }"
      sz
      (CCFloat.of_int sz /. CCFloat.of_int t.idx_sz *. 100.)

end
