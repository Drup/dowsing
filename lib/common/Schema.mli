type t

val of_string : Type.Env.t -> String.t -> t
val to_type : t -> Type.t
val pp : t Fmt.t [@@ocaml.toplevel_printer]
