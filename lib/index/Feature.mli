module type S = sig

  type t

  val compute : Type.t -> t
  val compare : t -> t -> Int.t
  val compatible : t -> t -> Bool.t

end

module ByHead : S
module ByHead' : S
module TailLength : S
