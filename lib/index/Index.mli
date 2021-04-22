type info = {
  lid : LongIdent.t ;
}

type t

val make : String.t List.t -> t
val add : t -> Type.t -> LongIdent.t -> Unit.t

val get_env : t -> Type.Env.t

val iter : t -> (Type.t * info) Iter.t
val iter_with : t -> Type.t -> (Type.t * info) Iter.t
val find : t -> Type.Env.t -> Type.t -> (Type.t * info * Unification.Subst.t) Iter.t

val load : String.t -> t
val save : t -> String.t -> Unit.t
