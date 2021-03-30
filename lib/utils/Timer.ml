type t = Float.t ref

let make () = ref 0.

let get self = ! self

let tick self =
  self := Unix.gettimeofday ()

let tock self =
  self := Unix.gettimeofday () -. ! self
