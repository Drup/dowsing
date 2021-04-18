type info = {
  lid : LongIdent.t ;
}

type t

val make : String.t List.t -> t
val get_env : t -> Type.Env.t

val iter : t -> info Iter.t
val iteri : t -> (Type.t * info) Iter.t
val iter' : t -> Type.Env.t -> Type.t -> info Iter.t
val iteri' : t -> Type.Env.t -> Type.t -> (Type.t * info) Iter.t

val load : String.t -> t
val save : t -> String.t -> Unit.t
