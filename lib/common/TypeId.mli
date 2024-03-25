(** A type with its unique identifier. 

    The unicity of the identifier is enforced by the caller. 
    Consult the {!Index.Trie} module for a reliable way
    to create unique typeids. 
*)

type id = int
type t = private { id : id; ty : Type.t }

val mk : int -> Type.t -> t
(** [mk id ty] creates a typeid. Should be used very carefully to ensure unicity. *)

(** {2 Accessors} *)

val ty : t -> Type.t

(** {2 Utilities} *)

val compare : t -> t -> id
val equal : t -> t -> bool
val hash : t -> int

module Set : CCSet.S with type elt = t
module Map : CCMap.S with type key = t
module Tbl : CCHashtbl.S with type key = t

module Range : sig
  include Diet.INTERVAL_SET with type elt = id

  val singleton : id -> t
  val of_set : Set.t -> t
end

val check : t -> Range.t -> bool
val pp : t Fmt.t
