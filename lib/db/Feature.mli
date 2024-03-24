module type S = sig
  type t

  val name : String.t
  val pp : t Fmt.t
  val compute : Type.t -> t
  val compare : t -> t -> int
  val compatible : query:t -> data:t -> Bool.t
end

module Head : S
module Tail : S
module Constructors : S

val all : (module S) List.t
val pp : (module S) Fmt.t

val compatible : Type.t -> Type.t -> bool
