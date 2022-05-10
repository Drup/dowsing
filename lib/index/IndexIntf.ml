module type S = sig

  type t

  type iter = (Type.t * Cell.t) Iter.t
  type iter_with_unifier = (Type.t * Cell.t * Subst.t) Iter.t

  val make : unit -> t

  (** [import t l] adds all the package pairs [pkg, pkg_dir] in [l]
      to the index [t].

      Removes old contents from packages if present.
  *)
  val import : t -> (String.t * Fpath.t) list -> unit

  (** [iter t] iterates over all types. *)
  val iter : ?pkgs:(String.t List.t) -> t -> iter
    
  (** [iter_compatible t ty] iterates over all types whose features 
      are compatible with [ty]. *)
  val iter_compatible : ?pkgs:(String.t List.t) -> t -> Type.t -> iter

  (** [find t env ty] returns all types which unifies with [ty],
      using the various filtering methods.
  *)
  val find : ?pkgs:(String.t List.t) -> t -> Type.Env.t -> Type.t -> iter_with_unifier
    
  (** [find_exhaustive t env ty] returns all types which unifies with [ty]
      through exhaustive search over all types.
  *)
  val find_exhaustive : ?pkgs:(String.t List.t) -> t -> Type.Env.t -> Type.t -> iter_with_unifier

  val load : Fpath.t -> t
  val save : t -> Fpath.t -> unit
  
  module Explorer : sig

    type index = t
    type t

    val make : index -> t
    val iter : t -> iter
    val select : t -> (Type.t -> Bool.t) -> unit
    val unselect : t -> unit

    val pp : t Fmt.t [@@ocaml.toplevel_printer]

  end

end
