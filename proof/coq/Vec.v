From Coq Require Vector.

Include Vector.

Lemma map_replace A B :
  forall {n} (vec : t A n) i (f : A -> B) a,
  map f (replace vec i a) = replace (map f vec) i (f a)
.
Proof.
  intros.
  induction vec.
  - inversion i.
  - simpl.
Admitted.
