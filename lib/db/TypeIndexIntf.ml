module type S = sig
  module T : Trie.S
  module Elt : sig
    type t
    val compare : t -> t -> int
    module Set : Set.S with type elt = t
  end

  type t = private {
    hcons : Type.Hashcons.t;
    mutable trie : T.t;
    index_by_type : (Elt.Set.t * TypeId.t * Automorphism.all) Type.HMap.t;
    mutable poset : Poset.t option;
  }

  type iter = (Elt.t * Type.t) Iter.t
  type iter_with_unifier = (Elt.t * (Type.t * Subst.t)) Iter.t

  val create :
    with_poset:bool -> Type.Env.t -> t

  val import :
    t ->
    (Elt.t * Outcometree.out_type) Iter.t ->
    unit
  (** [import infos l] adds all the package in [l] to the index [t].
      A package is a triplet [pkg, pkg_dir, iter] where [iter] is an
      iterator of items to index.

      Removes old contents from packages if present.
  *)

  val iter : t -> iter
  (** [iter t] iterates over all types. *)

  val iter_compatible : t -> Type.t -> iter
  (** [iter_compatible t ty] iterates over all types whose features 
      are compatible with [ty]. *)

  val find : t -> Type.Env.t -> Type.t -> iter_with_unifier
  (** [find t env ty] returns all types which unifies with [ty],
      using the various filtering methods (Trie + Poset).
  *)

  val find_with_trie : t -> Type.Env.t -> Type.t -> iter_with_unifier
  (** [find t env ty] returns all types which unifies with [ty],
      using only the Trie filtering.
  *)

  val find_exhaustive : t -> Type.Env.t -> Type.t -> iter_with_unifier
  (** [find_exhaustive t env ty] returns all types which unifies with [ty]
      through exhaustive search over all types.
  *)

  type serial
  val to_serial : t -> serial
  val of_serial : serial -> t
  
  val pp_metrics : t Fmt.t

  module Explorer : sig
    type index = t
    type t

    val make : index -> t
    val iter : t -> iter
    val select : t -> (Elt.t * Type.t -> Bool.t) -> unit
    val unselect : t -> unit
    val pp : t Fmt.t [@@ocaml.toplevel_printer]
  end
end
