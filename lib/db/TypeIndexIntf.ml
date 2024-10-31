module type S = sig
  module T : Trie.S
  module ID : sig
    type t
    val compare : t -> t -> int
    module Set : Set.S with type elt = t
  end

  type t = private {
    env : Type.Env.t ;
    mutable trie : T.t;
    index_by_type : (ID.Set.t * TypeId.t) Type.HMap.t;
    mutable poset : Poset.t option;
    type_decls : (Variable.var array * Type.t) LongIdent.HMap.t;
  }

  type iter = (ID.t * Type.t) Iter.t
  type iter_with_unifier = (ID.t * (Type.t * Subst.t)) Iter.t

  val create :
    with_poset:bool -> Type.Env.t -> t

  val add :
    t ->
    ID.t -> Type.t ->
    unit
  (** [add index id ty] adds the association [ty -> id] to the index
      [index].
  *)

  val add_type_decl :
    t ->
    LongIdent.t -> Variable.t list -> Type.t ->
    unit

  val iter : t -> iter
  (** [iter t] iterates over all types. *)

  val iter_compatible : t -> Type.t -> iter
  (** [iter_compatible t ty] iterates over all types whose features 
      are compatible with [ty]. *)

  val find :
    ?filter:[ `Default | `None | `OnlyTrie] ->
    t -> Type.Env.t -> Type.t -> iter_with_unifier
  (** [find t env ty] returns all types which unifies with [ty].
      
      Use [filter] to select a filtering techniques:
      - [`Default] uses the most efficient techniques.
      - [`None] relies on exhaustive search without filters.
      - [`OnlyTrie] only relies on the Trie index.
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
    val select : t -> (ID.t * Type.t -> Bool.t) -> unit
    val unselect : t -> unit
    val pp : t Fmt.t [@@ocaml.toplevel_printer]
  end
end
