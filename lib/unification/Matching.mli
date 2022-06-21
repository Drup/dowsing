type ord = Uncomparable | Smaller | Bigger | Equal

val pp_ord : Format.formatter -> ord -> unit

val compare :
  ?compat_leq:bool -> ?compat_geq:bool -> Type.Env.t -> Type.t -> Type.t -> ord
