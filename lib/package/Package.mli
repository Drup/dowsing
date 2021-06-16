exception Error of String.t

val find : String.t List.t -> (String.t * Fpath.t) List.t
val find_all : Unit.t -> (String.t * Fpath.t) List.t

type info = {
  orig_lid : LongIdent.t ;
  lid : LongIdent.t ;
  out_ty : Outcometree.out_type ;
}

val iter : Fpath.t -> info Iter.t
