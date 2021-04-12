type key = LongIdent.t

type info = {
  ty : Type.t ;
}

type t

val make : String.t List.t -> t
val get_env : t -> Type.Env.t
val get : t -> key -> info Option.t
val add : t -> key -> info -> Unit.t
val iter : t -> (key -> info -> Unit.t) -> Unit.t
val filter : t -> Type.t -> Unit.t

val load : String.t -> t
val save : t -> String.t -> Unit.t
