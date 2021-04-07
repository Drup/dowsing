(** Hullot trees.

    Allow to quickly iterates through bitsets that must
    be both "large enough" and "small enough".

    Parametrized by a bitvector module.
*)


module type S = sig
  type bitset

  (** [iter ~len ~big ~small] returns the sequence of
     bitset [bs] of size [n] such that each [large bs]
     and [small bs] are both true.

     We assume that:
     - if [small s] holds, then for all subset [bs'] of [bs],
     [small bs'] holds,
     - if [large s] holds, then for all superset [bs'] of [bs],
     [large bs'] holds.
  *)
  val iter : len:int -> small:(bitset -> bool) -> large:(bitset -> bool) -> bitset Iter.t
end

include S with type bitset = Bitv.t

module Default : S with type bitset = Bitv.t

module Make (M : Bitv.S) : S with type bitset = M.t
