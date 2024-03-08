
module type S = TypeIndexIntf.S

module Logs = (val Logs.(src_log @@ Src.create __MODULE__))

let _info = Logs.info
let _debug = Logs.debug

module Make (TX : Trie.NODE) : S = struct
  module T = TX

  type t = {
    hcons : Type.Hashcons.t;
    mutable trie : T.t;
    index_by_type : Content.ID.Set.t Type.HMap.t;
    mutable poset : Poset.t;
  }

  type iter = (Content.ID.t * Type.t) Iter.t
  type iter_with_unifier = (Content.ID.t * (Type.t * Subst.t)) Iter.t

  let create env = {
    hcons = Type.Hashcons.make ();
    trie = T.empty;
    index_by_type = Type.HMap.create 17;
    poset = Poset.init env;
  }

  (** [add pkg pkg_dir] adds the package [pkg] available in path [pkg_dir].

      Removes the old content of [pkg] if present.
  *)
  let add t (id, ty) =
    let env = Type.Env.make Data ~hcons:t.hcons in
    let ty = Type.of_outcometree env ty in
    t.trie <- T.add ty t.trie;
    Type.HMap.update t.index_by_type ~k:ty ~f:(fun _ -> function
        | None ->
          Some (Content.ID.Set.singleton id)
        | Some ids ->
          Some (Content.ID.Set.add id ids)
      )

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
    Iter.iter (add t) l;
    refresh t;
    _info (fun m ->
        m "@[%i @ types to insert in the index @]"
          (Iter.length @@ T.iterid t.trie));
    if with_poset then regenerate_poset ~with_feat t

  (** Iterators *)

  let insert_ids ~to_type t it k =
    it (fun elt ->
        let ty = to_type elt in
        let ids = Type.HMap.find t.index_by_type ty in
        Content.ID.Set.iter (fun id -> k (id, elt)) ids
      )

  let filter_with_poset t env ty range =
    Poset.check t.poset env ~query:ty ~range

  let filter_with_unification env ty it =
    Iter.filter_map
      (fun ty' -> Acic.unify env ty ty' |> CCOption.map @@ CCPair.make ty')
      it

  let iter t : iter =
    T.iter t.trie
    |> insert_ids t ~to_type:Fun.id

  let iter_compatible t ty : iter =
    let _range, it = T.iter_compatible ty t.trie in
    it
    |> insert_ids t ~to_type:Fun.id

  let find_exhaustive t env ty : iter_with_unifier =
    T.iter t.trie
    |> filter_with_unification env ty
    |> insert_ids t ~to_type:fst

  let find_with_trie t env ty : iter_with_unifier =
    let _range, it = T.iter_compatible ty t.trie in
    it
    |> filter_with_unification env ty
    |> insert_ids t ~to_type:fst

  let find t env ty : iter_with_unifier =
    let range = T.range_compatible ty t.trie in
    _info (fun m -> m "%a@." TypeId.Range.pp range);
    (* Poset.xdot t.poset ~range; *)
    filter_with_poset t env ty range
    |> insert_ids t ~to_type:fst

  let pp_metrics fmt t =
    Fmt.pf fmt
      "@[<v>Entries: %i@,\
       Size: %iko@,\
       Trie size: %iko@,\
       Poset size: %iko" (Poset.size t.poset)
      (Obj.reachable_words (Obj.repr t) / 1024)
      (Obj.reachable_words (Obj.repr t.trie) / 1024)
      (Obj.reachable_words (Obj.repr t.poset) / 1024)

  type serial = t
  let to_serial = CCFun.id
  let of_serial = CCFun.id

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
