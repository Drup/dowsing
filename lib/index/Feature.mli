module type S = sig

  type t

  val compute : Type.t -> t
  val compare : t CCOrd.t
  val compatible : query:t -> data:t -> Bool.t

end

module Head : S
module Tail : S
