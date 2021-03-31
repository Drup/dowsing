type t = Float.t ref

let make () = ref 0.

let get self = ! self

let start self =
  self := Unix.gettimeofday ()

let stop self =
  self := Unix.gettimeofday () -. ! self
