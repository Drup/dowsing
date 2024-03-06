type entry = Info.t

module ID : sig
  type t
  module Tbl : sig
    type key = t
    type 'a t
    val create : unit -> 'a t
    val get : key -> 'a t -> 'a
    val add : 'a -> 'a t -> key
  end
  module Set : CCSet.S with type elt = t
end = struct
  type t = int
  module Tbl = struct
    type key = t
    type 'a t = 'a CCVector.vector
    let create () = CCVector.create ()
    let get i t = CCVector.get t i
    let add v t =
      CCVector.push t v;
      CCVector.size t - 1
  end
  module Set = CCSet.Make(CCInt)
end


type t = {
  (* Actual context of the Database, indexed by unique identifiers.
     TODO: Ideally, the entries should be mmap-ed *)
  entries : entry ID.Tbl.t ;
  (* Index by longident *)
  index_by_lid : ID.t LongIdent.HMap.t ;
}

let empty = {
  entries = ID.Tbl.create () ;
  index_by_lid = LongIdent.HMap.create 17 ;
}

let find t id =
  ID.Tbl.get id t.entries

let find_lid t lid =
  let id = LongIdent.HMap.find t.index_by_lid lid in
  ID.Tbl.get id t.entries

let add t lid v =
  if LongIdent.HMap.mem t.index_by_lid lid then
    Fmt.failwith "The key %a is already present in the database"
      LongIdent.pp lid
  else
    let id = ID.Tbl.add v t.entries in
    LongIdent.HMap.add t.index_by_lid lid id;
    id
