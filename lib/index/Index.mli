type info = {
  lid : LongIdent.t ;
}

type t

type iter = (Type.t * info) Iter.t
type iter' = (Type.t * info * Unification.Subst.t) Iter.t

val make : String.t List.t -> t
val add : t -> Type.t -> LongIdent.t -> Unit.t

val get_env : t -> Type.Env.t

val iter : t -> iter
val iter_with : t -> Type.t -> iter

val find : t -> Type.Env.t -> Type.t -> iter'
val find_with : t -> Type.Env.t -> Type.t -> iter'

val load : String.t -> t
val save : t -> String.t -> Unit.t

module Explorer : sig

  type index = t
  type t

  val make : index -> t
  val iter : t -> iter
  val select : t -> (Type.t -> Bool.t) -> Unit.t
  val unselect : t -> Unit.t

  val pp : t Fmt.t [@@ocaml.toplevel_printer]

end
