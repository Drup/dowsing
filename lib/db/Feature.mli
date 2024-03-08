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
val all_names : String.t List.t
val to_string : (module S) -> String.t
val of_string : String.t -> (module S)
val pp : (module S) Fmt.t
