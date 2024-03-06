module Feature = Feature
module Trie = Trie
module Info = Info
module Poset = Poset
module MatchFeat = MatchFeat
module Doc = Doc

module type S = IndexIntf.S

module Logs = (val Logs.(src_log @@ Src.create __MODULE__))

let _info = Logs.info
let _debug = Logs.debug

module Make (TX : Trie.NODE) : S = struct
  module T = TX

  type t = {
    hcons : Type.Hashcons.t;
    mutable trie : T.t;
    pkgs_dirs : Fpath.t String.HMap.t;
    content : Doc.t;
    index_by_type : Doc.ID.Set.t Type.HMap.t;
    mutable poset : Poset.t;
  }

  type iter = (Doc.entry * Type.t) Iter.t
  type iter_with_unifier = (Doc.entry * (Type.t * Subst.t)) Iter.t

  let make env =
    {
      hcons = Type.Hashcons.make ();
      trie = T.empty;
      pkgs_dirs = String.HMap.create 17;
      content = Doc.empty;
      index_by_type = Type.HMap.create 17;
      poset = Poset.init env;
    }

  (** [add pkg pkg_dir] adds the package [pkg] available in path [pkg_dir].

      Removes the old content of [pkg] if present.
  *)
  let add t (pkg, pkg_dir, it) =
    let aux t (lid, (info : Info.t)) =
      let env = Type.Env.make Data ~hcons:t.hcons in
      let ty = Type.of_outcometree env info.ty in
      t.trie <- T.add ty t.trie;
      let id = Doc.add t.content lid info in
      Type.HMap.update t.index_by_type ~k:ty ~f:(fun _ -> function
          | None ->
            Some (Doc.ID.Set.singleton id)
          | Some ids ->
            Some (Doc.ID.Set.add id ids)
        )
    in
    if String.HMap.mem t.pkgs_dirs pkg then
      Fmt.failwith
        "Package %s is already present in the index. \
         Please regenerate the index" pkg ;
    Iter.iter (aux t) it;
    String.HMap.add t.pkgs_dirs pkg pkg_dir

  let refresh t =
    let _ = T.refresh ~start:0 t.trie in
    ()

  let regenerate_poset ?(with_feat = true) t =
    let e = Type.Env.make ~hcons:t.hcons Data in
    let p = Poset.init e in
    T.iterid t.trie
    (* Do not eta-reduce (Not sure why) *)
    |> Iter.iter (fun ty -> Poset.add ~with_feat p ty);
    Poset.annotate_top p;
    t.poset <- p;
    (* Poset.xdot t.poset; *)
    ()

  let import ?(with_poset = true) ?(with_feat = true) t l =
    List.iter (add t) l;
    refresh t;
    _info (fun m ->
        m "@[%i @ types to insert in the index @]"
          (Iter.length @@ T.iterid t.trie));
    if with_poset then regenerate_poset ~with_feat t

  (** Iterators *)

  let mem_pkgs t pkgs =
    match pkgs with
    | None -> fun _ -> true
    | Some pkgs ->
        let set =
          CCList.fold_left
            (fun set pkg ->
              let pkg_dir = String.HMap.find t.pkgs_dirs pkg in
              Fpath.Set.add pkg_dir set)
            Fpath.Set.empty pkgs
        in
        fun pkg -> Fpath.Set.mem pkg set

  let filter_with_pkgs ~to_type ?pkgs t iter =
    let pkg_filt = mem_pkgs t pkgs in
    iter
    |> Iter.flat_map (fun elt ->
        let ty = to_type elt in
        let ids = Type.HMap.find t.index_by_type ty in
        Doc.ID.Set.to_iter ids
        |> Iter.filter_map (fun id ->
            let info = Doc.find t.content id in
            if pkg_filt info.pkg_dir && not (Info.is_internal info) then
              Some (info, elt)
            else None))

  let filter_with_poset t env ty range =
    Poset.check t.poset env ~query:ty ~range

  let filter_with_unification env ty it =
    Iter.filter_map
      (fun ty' -> Acic.unify env ty ty' |> CCOption.map @@ CCPair.make ty')
      it

  let iter ?pkgs t : iter =
    T.iter t.trie
    |> filter_with_pkgs t ?pkgs ~to_type:CCFun.id

  let iter_compatible ?pkgs t ty : iter =
    let _range, it = T.iter_compatible ty t.trie in
    filter_with_pkgs t ?pkgs ~to_type:CCFun.id it

  let find_exhaustive ?pkgs t env ty : iter_with_unifier =
    T.iter t.trie
    |> filter_with_unification env ty
    |> filter_with_pkgs t ?pkgs ~to_type:fst

  let find_with_trie ?pkgs t env ty : iter_with_unifier =
    let _range, it = T.iter_compatible ty t.trie in
    it
    |> filter_with_unification env ty
    |> filter_with_pkgs t ?pkgs ~to_type:fst

  let find ?pkgs t env ty : iter_with_unifier =
    let range = T.range_compatible ty t.trie in
    _info (fun m -> m "%a@." TypeId.Range.pp range);
    (* Poset.xdot t.poset ~range; *)
    filter_with_poset t env ty range
    |> filter_with_pkgs t ?pkgs ~to_type:fst

  let pp_metrics fmt t =
    Fmt.pf fmt
      "@[<v>Entries: %i@,\
       Size: %iko@,\
       Trie size: %iko@,\
       Poset size: %iko@,\
       Metadata: %iko" (Poset.size t.poset)
      (Obj.reachable_words (Obj.repr t) / 1024)
      (Obj.reachable_words (Obj.repr t.trie) / 1024)
      (Obj.reachable_words (Obj.repr t.poset) / 1024)
      (Obj.reachable_words (Obj.repr t.content) / 1024)

  module Archive = struct
    let of_index = CCFun.id
    let to_index = CCFun.id
    let load file : t = CCIO.with_in (Fpath.to_string file) Marshal.from_channel

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
    type t = { idx_sz : Int.t; its : iter Stack.t }

    let make idx =
      let it = iter idx in
      let idx_sz = Iter.length it in
      let its = Stack.create () in
      Stack.push it its;
      { idx_sz; its }

    let iter t = Stack.top t.its

    let select t filt =
      let filt = CCPair.fst_map filt in
      Stack.push (Iter.filter filt @@ iter t) t.its

    let unselect t =
      if Stack.length t.its = 1 then error ();
      let _iter = Stack.pop t.its in
      ()

    let pp ppf t =
      let sz = Iter.length @@ iter t in
      Fmt.pf ppf "{ explored subset: %i (%g %%) }" sz
        (CCFloat.of_int sz /. CCFloat.of_int t.idx_sz *. 100.)
  end
end

let make feats = (module Make ((val Trie.make feats)) : S)
