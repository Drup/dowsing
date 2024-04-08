type ord = Incomparable | Smaller | Bigger | Matching_equiv

val pp_ord : Format.formatter -> ord -> unit

type hint = Incompatible | Maybe_bigger | Maybe_smaller | Unsure

val pp_hint : hint Fmt.t
val combine_hint : hint -> hint -> hint
val compare : ?hint:hint -> Type.Env.t -> Type.t -> Type.t -> ord
