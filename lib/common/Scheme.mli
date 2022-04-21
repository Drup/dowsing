type t

val of_string : Type.Env.t -> String.t -> t
val to_type : Type.Env.t -> t -> Type.t
val pp : t Fmt.t [@@ocaml.toplevel_printer]
