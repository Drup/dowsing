
module type S = TypeIndexIntf.S

module Logs = (val Logs.(src_log @@ Src.create __MODULE__))

let _info = Logs.info
let _debug = Logs.debug

module Make (I : Set.OrderedType) = struct
  module T = Trie.Default
  module ID = struct
    include I
    module Set = CCSet.Make(I)
  end

  type t = {
    mutable trie : T.t;
    index_by_type : (ID.Set.t * TypeId.t) Type.HMap.t;
    mutable poset : Poset.t option;
  }

  type iter = (ID.t * Type.t) Iter.t
  type iter_with_unifier = (ID.t * (Type.t * Subst.t)) Iter.t

  let create ~with_poset env =
    let index_by_type = Type.HMap.create 17 in
    let trie = T.empty in
    let poset = if with_poset then Some (Poset.init env) else None in
    { hcons = env.hcons; trie ; index_by_type ; poset }

  let size t = Type.HMap.length t.index_by_type

  let add t (entry, ty) =
    let env = Type.Env.make ~hcons:t.hcons () in
    let ty = Type.of_outcometree env ty in
    match Type.HMap.get t.index_by_type ty with
    | None ->
      let id = size t in
      let tyid = TypeId.mk id ty in
      let entries = ID.Set.singleton entry in
      Type.HMap.add t.index_by_type ty (entries, tyid);
      t.trie <- T.add tyid t.trie;
      begin match t.poset with
        | None -> ()
        | Some poset ->
          let range = T.checker ty t.trie in
          Poset.add poset tyid ~range
      end
    | Some (entries, tyid) ->
      let entries = ID.Set.add entry entries in
      Type.HMap.replace t.index_by_type ty (entries, tyid)

  let import t l =
    _info (fun m ->
        m "@[%i @ types to insert in the index @]"
          (Iter.length l));
    Iter.iter (add t) l

  (** Iterators *)

  let insert_ids ~to_type t it k =
    it (fun elt ->
        let ty = to_type elt in
        let elts, _ = Type.HMap.find t.index_by_type ty in
        ID.Set.iter (fun id -> k (id, elt)) elts
      )

  let filter_with_unification env ty it =
    Iter.filter_map
      (fun ty' ->
         Acic.unify env ty ty'
         |> CCOption.map @@ CCPair.make ty')
      it

  let iter t : iter =
    T.iter t.trie
    |> insert_ids t ~to_type:Fun.id

  let iter_compatible t ty : iter =
    let it = T.search ty t.trie in
    it
    |> insert_ids t ~to_type:Fun.id

  let find_exhaustive t env ty : iter_with_unifier =
    T.iter t.trie
    |> filter_with_unification env ty
    |> insert_ids t ~to_type:fst

  let find_with_trie t env ty : iter_with_unifier =
    let it = T.search ty t.trie in
    it
    |> filter_with_unification env ty
    |> insert_ids t ~to_type:fst

  let find t env ty : iter_with_unifier =
    match t.poset with
    | None ->
      find_with_trie t env ty
    | Some poset -> 
      let range = T.checker ty t.trie in
      Poset.check poset env ~query:ty ~range
      |> insert_ids t ~to_type:fst

  let pp_metrics fmt t =
    Fmt.pf fmt
      "@[<v>Entries: %i@,\
       Total Size: %iko@,\
       Trie size: %iko@,\
       Poset size: %iko"
      (Type.HMap.length t.index_by_type)
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
