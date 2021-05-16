Require Import Typ.

Fixpoint head ty :=
  match ty with
  | _ --> ty => head ty
  | _ => ty
  end
.

Lemma equiv_head :
  forall {ty1 ty2},
  ~ IsArrow ty1 ->
  ~ IsArrow ty2 ->
  ty1 ~ ty2 ->
  head (ty1) ~ head (ty2)
.
Proof.
  intros *.
  intros not_arrow1 not_arrow2.
  induction 1.
  + destruct H ; simpl ; apply Ax.
    - apply ProdAssoc.
    - apply ProdComm.
    - destruct ty ; simpl ; try apply ProdUnit.
      destruct not_arrow2.
      exists ty1, ty2.
      reflexivity.
    - destruct not_arrow1.
      exists (ty1 ** ty2), ty3.
      reflexivity.
  + exact Refl.
  +

Conjecture head_subst :
  forall ty sigma,
  head (subst sigma ty) = head (subst sigma (head ty))
.

Lemma equiv_cons_inversion' :
  forall {ty1 ty2 f},
  IsCons f ty1 \/ IsCons f ty2 ->
  ty1 ~ ty2 ->
    IsCons f ty1 /\ (IsCons f ty2 \/ IsProd ty2) \/
    IsCons f ty2 /\ (IsCons f ty1 \/ IsProd ty1)
.
Proof.
  intros *.
  intros either_is_cons.
  induction 1.
  - destruct H ; destruct either_is_cons ; destruct H ; try discriminate H.
    right.
    split.
    exists x. assumption.
    right. exists Unit, ty. reflexivity.
  - left.
    destruct either_is_cons ; (split ; [assumption | left ; assumption]).
  -

Lemma equiv_cons_inversion' :
  forall {ty1 ty2 f},
  ~ IsProd ty1 ->
  ~ IsProd ty2 ->
  IsCons f ty1 \/ IsCons f ty2 ->
  ty1 ~ ty2 ->
  IsCons f ty1 /\ IsCons f ty2
.
Proof.
  intros *.
  intros not_prod1 not_prod2 either_is_cons.
  induction 1.
  - destruct H ; destruct either_is_cons ; destruct H ; try discriminate H.
    destruct not_prod1.
    exists Unit, ty.
    reflexivity.
  - destruct either_is_cons ; split ; assumption.
  -

Lemma equiv_cons_inversion :
  forall {f1 f2 tys1 tys2},
  Cons f1 tys1 ~ Cons f2 tys2 ->
  f1 = f2
.

Lemma byhead_cons :
  forall {ty1 ty2 f1 f2},
  IsCons f1 (head ty1) ->
  IsCons f2 (head ty2) ->
  ty1 ◊ ty2 ->
  f1 = f2
.
Proof.
  intros *.
  intro [tys1 head1_eq_f1].
  intros [tys1 head1_eq_f1].
  intros [tys2 head2_eq_f2].
  intros [sigma equiv].
  apply equiv_head in equiv.
  rewrite (head_subst ty1) in equiv.
  rewrite (head_subst ty2) in equiv.
  rewrite head1_eq_f1 in equiv.
  rewrite head2_eq_f2 in equiv.
  simpl in equiv.
  apply (equiv_cons_inversion equiv).
Qed.
