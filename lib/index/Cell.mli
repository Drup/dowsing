type t

val empty : t
val update : Longident.t -> Info.t -> t Option.t -> t
val pp : t Fmt.t [@@ocaml.toplevel_printer]
