type ord = Uncomparable | Smaller | Bigger | Matching_equiv

val pp_ord : Format.formatter -> ord -> unit

type hint = Uncompatible | Not_smaller | Not_bigger | Unsure

val pp_hint : hint Fmt.t
val combine_hint : hint -> hint -> hint
val compare : ?hint:hint -> Type.Env.t -> Type.t -> Type.t -> ord
