module type S = sig
  module T : Trie.NODE

  type t = private {
    hcons : Type.Hashcons.t;
    mutable trie : T.t;
    pkgs_dirs : Fpath.t String.HMap.t;
    content : Doc.t;
    index_by_type : Doc.ID.Set.t Type.HMap.t;
    mutable poset : Poset.t;
  }

  type iter = (Info.t * Type.t) Iter.t
  type iter_with_unifier = (Info.t * (Type.t * Subst.t)) Iter.t

  val make : Type.Env.t -> t

  val import :
    ?with_poset:bool ->
    ?with_feat:bool ->
    t ->
    (String.t * Fpath.t * (Longident.t * Info.t) Iter.t) list ->
    unit
  (** [import_package t l] adds all the package in [l] to the index [t].
      A package is a triplet [pkg, pkg_dir, iter] where [iter] is an
      iterator of items to index.

      Removes old contents from packages if present.
  *)

  val iter : ?pkgs:String.t List.t -> t -> iter
  (** [iter t] iterates over all types. *)

  val iter_compatible : ?pkgs:String.t List.t -> t -> Type.t -> iter
  (** [iter_compatible t ty] iterates over all types whose features 
      are compatible with [ty]. *)

  val find :
    ?pkgs:String.t List.t -> t -> Type.Env.t -> Type.t -> iter_with_unifier
  (** [find t env ty] returns all types which unifies with [ty],
      using the various filtering methods (Trie + Poset).
  *)

  val find_with_trie :
    ?pkgs:String.t List.t -> t -> Type.Env.t -> Type.t -> iter_with_unifier
  (** [find t env ty] returns all types which unifies with [ty],
      using only the Trie filtering.
  *)

  val find_exhaustive :
    ?pkgs:String.t List.t -> t -> Type.Env.t -> Type.t -> iter_with_unifier
  (** [find_exhaustive t env ty] returns all types which unifies with [ty]
      through exhaustive search over all types.
  *)

  val load : Fpath.t -> t
  val save : t -> Fpath.t -> unit
  val pp_metrics : t Fmt.t

  module Explorer : sig
    type index = t
    type t

    val make : index -> t
    val iter : t -> iter
    val select : t -> (Info.t -> Bool.t) -> unit
    val unselect : t -> unit
    val pp : t Fmt.t [@@ocaml.toplevel_printer]
  end
end
