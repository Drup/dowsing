type t = Float.t ref

let make () = ref 0.

let get t = !t

let start t =
  t := Unix.gettimeofday ()

let stop t =
  t := Unix.gettimeofday () -. !t
