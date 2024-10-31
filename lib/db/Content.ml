module ID : sig
  type t
  val compare : t -> t -> int
  module Tbl : sig
    type key = t
    type 'a t
    val create : unit -> 'a t
    val get : key -> 'a t -> 'a
    val add : 'a -> 'a t -> key
    val iteri : (key -> 'a -> unit) -> 'a t -> unit
    val size : _ t -> int
  end
  module Set : CCSet.S with type elt = t
end = struct
  type t = int
  let compare = Int.compare
  module Tbl = struct
    type key = t
    type 'a t = 'a CCVector.vector
    let create () = CCVector.create ()
    let get i t = CCVector.get t i
    let add v t =
      CCVector.push t v;
      CCVector.size t - 1
    let iteri = CCVector.iteri
    let size = CCVector.size
  end
  module Set = CCSet.Make(CCInt)
end


type t = {
  (* Actual context of the Database, indexed by unique identifiers.
     TODO: Ideally, the entries should be mmap-ed *)
  entries : Entry.t ID.Tbl.t ;
  (* Index by longident *)
  values_by_lid : ID.t LongIdent.HMap.t ;
  types_by_lid : ID.t LongIdent.HMap.t ;
}

let create () = {
  entries = ID.Tbl.create () ;
  values_by_lid = LongIdent.HMap.create 17 ;
  types_by_lid = LongIdent.HMap.create 17 ;
}

let size t = ID.Tbl.size t.entries

let find t id =
  ID.Tbl.get id t.entries

let find_value t lid =
  let id = LongIdent.HMap.find t.values_by_lid lid in
  ID.Tbl.get id t.entries

let find_type t lid =
  let id = LongIdent.HMap.find t.types_by_lid lid in
  ID.Tbl.get id t.entries

let add t (v : Entry.t) =
  let map = match v.desc with
    | Val _ -> t.values_by_lid | Type _ -> t.types_by_lid
  in
  match LongIdent.HMap.get map v.lid with
  | Some id ->
    (* TODO Probably should do a warning here *)
    id
  | None ->
    let id = ID.Tbl.add v t.entries in
    LongIdent.HMap.add map v.lid id;
    id

let iteri t f =
  ID.Tbl.iteri (fun k v -> f (k,v)) t.entries

let mem_pkgs pkgs =
  match pkgs with
  | None -> fun _ -> true
  | Some pkgs ->
    let set =
      CCList.fold_left
        (fun set pkg -> String.Set.add pkg set)
        String.Set.empty pkgs
    in
    fun pkg -> String.Set.mem pkg set

let resolve_all ?pkgs t ids =
  let pkg_filt = mem_pkgs pkgs in
  ids
  |> Iter.filter_map (fun (id, x) ->
      let info = find t id in
      if pkg_filt info.pkg && not (Entry.is_internal info) then
        Some (info, x)
      else None)
