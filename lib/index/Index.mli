type key = LongIdent.t

type info = {
  ty : Type.t ;
}

type t

val make : String.t List.t -> t
val get : t -> key -> info Option.t
val add : t -> key -> info -> Unit.t
val iter : (key -> info -> Unit.t) -> t -> Unit.t

val load : String.t -> t
val save : t -> String.t -> Unit.t
