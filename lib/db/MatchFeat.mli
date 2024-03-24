module type S = sig
  type t
  val pp : t Fmt.t

  val name : String.t
  val compute : Type.t -> t
  val compare : t CCOrd.t
  val compatible : t -> t -> Acic.hint
end

module Const : S

val all : (module S) List.t
val pp : (module S) Fmt.t

val compatible : Type.t -> Type.t -> Acic.hint

val compare : Type.Env.t -> Type.t -> Type.t -> Acic.ord
