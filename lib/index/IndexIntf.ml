module type S = sig

  type t

  type iter = (Type.t * Cell.t) Iter.t
  type iter_with_unifier = (Type.t * Cell.t * Subst.t) Iter.t

  val make : Unit.t -> t
  val remove : t -> String.t -> unit
  val add : t -> String.t -> Fpath.t -> unit

  val iter : ?pkgs:(String.t List.t) -> t -> iter
  val iter_compatible : ?pkgs:(String.t List.t) -> t -> Type.t -> iter

  val find : ?pkgs:(String.t List.t) -> t -> Type.Env.t -> Type.t -> iter_with_unifier
  val find_exhaustive : ?pkgs:(String.t List.t) -> t -> Type.Env.t -> Type.t -> iter_with_unifier

  val load : Fpath.t -> t
  val save : t -> Fpath.t -> Unit.t

  val refresh : t -> unit
  
  module Explorer : sig

    type index = t
    type t

    val make : index -> t
    val iter : t -> iter
    val select : t -> (Type.t -> Bool.t) -> Unit.t
    val unselect : t -> Unit.t

    val pp : t Fmt.t [@@ocaml.toplevel_printer]

  end

end
