module Feature = Feature
module Trie = Trie
module Info = Info
module Cell = Cell

module type S = IndexIntf.S

module Make (Trie : Trie.NODE) : S = struct

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
      Type.Map.update ty
        (CCFun.compose (Cell.update orig_lid info) CCOption.return) cells
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

  (** Iterators *)

  let mem_pkgs t pkgs =
    match pkgs with
    | None ->
      fun _ -> true
    | Some pkgs -> 
      let set = CCList.fold_left (fun set pkg ->
          let pkg_dir = String.HMap.find t.pkgs_dirs pkg in
          Fpath.Set.add pkg_dir set
        ) Fpath.Set.empty pkgs
      in
      fun pkg -> Fpath.Set.mem pkg set

  let filter_with_pkgs ~to_type ~merge ?pkgs t iter =
    let pkg_filt = mem_pkgs t pkgs in
    iter
    |> Iter.flat_map (fun elt ->
        let ty = to_type elt in
        let it k = Fpath.Map.iter (fun p c -> k (p, c)) t.cells in
        it 
        |> Iter.filter_map (fun (pkg_dir, (_, cells)) ->
            if pkg_filt pkg_dir then
              Type.Map.get ty cells
              |> CCOption.map @@ merge elt
            else None
          )
      )

  let filter_with_unification env ty it =
    Iter.filter_map (fun ty' ->
        Unification.unify env ty ty'
        |> CCOption.map @@ CCPair.make ty'
      ) it

  let iter ?pkgs t =
    Trie.iter t.trie
    |> filter_with_pkgs t ?pkgs ~to_type:CCFun.id ~merge:CCPair.make

  let iter_compatible ?pkgs t ty =
    let _range, it = Trie.iter_compatible ty t.trie in
    filter_with_pkgs t ?pkgs ~to_type:CCFun.id ~merge:CCPair.make it
  
  let find_exhaustive ?pkgs t env ty = 
    Trie.iter t.trie
    |> filter_with_unification env ty
    |> filter_with_pkgs t ?pkgs
      ~to_type:fst ~merge:(fun (ty, unif) cell -> ty, cell, unif)

  let find ?pkgs t env ty =
    let _range, it = Trie.iter_compatible ty t.trie in
    it
    |> filter_with_unification env ty
    |> filter_with_pkgs t ?pkgs
      ~to_type:fst ~merge:(fun (ty, unif) cell -> ty, cell, unif)


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
  let refresh t =
    let _ = Trie.refresh ~start:0 t.trie in
    ()
  
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
      let _iter = Stack.pop t.its in
      ()

    let pp ppf t =
      let sz = Iter.length @@ iter t in
      Fmt.pf ppf "{ explored subset: %i (%g %%) }"
        sz
        (CCFloat.of_int sz /. CCFloat.of_int t.idx_sz *. 100.)

  end

end

let make feats = (module Make (val Trie.make feats) : S)

module Poset = Poset 
