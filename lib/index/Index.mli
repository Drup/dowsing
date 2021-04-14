module Key = LongIdent

type info = {
  key : Key.t ;
  ty : Type.t ;
}

type t

val make : String.t List.t -> t
val get_env : t -> Type.Env.t
val get : t -> Key.t -> info Option.t
val add : t -> Key.t -> info -> Unit.t
val iter : t -> (Key.t -> info -> Unit.t) -> Unit.t

val load : String.t -> t
val save : t -> String.t -> Unit.t
