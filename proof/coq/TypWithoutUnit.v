From Coq.Arith Require Import PeanoNat.
From Coq.Arith Require Plus.

Require Signature.
Require Var.
Require Vec.

Inductive Typ : Set :=
  | Var : Var.Var -> Typ
  | Prod : Typ -> Typ -> Typ
  | Arrow : Typ -> Typ -> Typ
  | Cons : forall f, Vec.t Typ (Signature.arity f) -> Typ
.

Infix "**" := Prod (at level 20, right associativity).
Infix "-->" := Arrow (at level 30, right associativity).

Definition IsVar ty :=
  exists v, ty = Var v
.

Definition IsProd ty :=
  exists ty1 ty2, ty = ty1 ** ty2
.

Definition IsArrow ty :=
  exists ty1 ty2, ty = ty1 --> ty2
.

Definition IsCons f ty :=
  exists tys, ty = Cons f tys
.

Definition Subst := Var.Var -> Typ
.

Fixpoint subst sigma ty :=
  match ty with
  | Var v       => sigma v
  | ty1 ** ty2  => subst sigma ty1 ** subst sigma ty2
  | ty1 --> ty2 => subst sigma ty1 --> subst sigma ty2
  | Cons f tys  => Cons f (Vec.map (subst sigma) tys)
  end
.

Reserved Infix "≈" (at level 50).

Inductive Isomorphic : Typ -> Typ -> Prop :=
  | ProdAssoc : forall ty1 ty2 ty3,
      ty1 ** (ty2 ** ty3) ≈
      (ty1 ** ty2) ** ty3
  | ProdComm : forall ty1 ty2,
      ty1 ** ty2 ≈
      ty2 ** ty1
  | Curry : forall ty1 ty2 ty3,
      ty1 ** ty2 --> ty3 ≈
      ty1 --> ty2 --> ty3

where "ty1 ≈ ty2" := (Isomorphic ty1 ty2)
.

Reserved Infix "~" (at level 50).

Inductive Equivalent : Typ -> Typ -> Prop :=
  | Ax : forall {ty1 ty2},
      ty1 ≈ ty2 ->
      ty1 ~ ty2
  | Refl : forall {ty},
      ty ~ ty
  | Trans : forall {ty1 ty2 ty3},
      ty1 ~ ty2 ->
      ty2 ~ ty3 ->
      ty1 ~ ty3
  | Sym : forall {ty1 ty2},
      ty1 ~ ty2 ->
      ty2 ~ ty1
  | CongProdLeft : forall {ty1 ty2 ty1'},
      ty1 ~ ty1' ->
      Prod ty1 ty2 ~ Prod ty1' ty2
  | CongProdRight : forall {ty1 ty2 ty2'},
      ty2 ~ ty2' ->
      Prod ty1 ty2 ~ Prod ty1 ty2'
  | CongArrowLeft : forall {ty1 ty2 ty1'},
      ty1 ~ ty1' ->
      Prod ty1 ty2 ~ Prod ty1' ty2
  | CongArrowRight : forall {ty1 ty2 ty2'},
      ty2 ~ ty2' ->
      Prod ty1 ty2 ~ Prod ty1 ty2'
  | CongCons : forall {f i tys ty_i'},
      Vec.nth tys i ~ ty_i' ->
      Cons f tys ~ Cons f (Vec.replace tys i ty_i')

where "ty1 ~ ty2" := (Equivalent ty1 ty2)
.

Definition Unifiable ty1 ty2 :=
  exists sigma, subst sigma ty1 ~ subst sigma ty2
.

Infix "◊" := Unifiable (at level 50).

Lemma equiv_subst :
  forall {ty1 ty2}, ty1 ~ ty2 ->
  forall sigma, subst sigma ty1 ~ subst sigma ty2
.
Proof.
  intros.
  induction H.
  - destruct H ; simpl ; apply Ax ; constructor.
  - apply Refl.
  - apply @Trans with (subst sigma ty2) ; assumption.
  - apply Sym ; assumption.
  - apply CongProdLeft. assumption.
  - apply CongProdRight. assumption.
  - apply CongArrowLeft. assumption.
  - apply CongArrowRight. assumption.
  - simpl.
    rewrite Vec.map_replace.
    apply CongCons.
    rewrite (Vec.nth_map (subst sigma) tys i i eq_refl).
    assumption.
Qed.

(*
   feature 1: head
*)

Fixpoint head ty :=
  match ty with
  | _ --> ty => head ty
  | _ => ty
  end
.

Lemma equiv_head :
  forall {ty1 ty2},
  ty1 ~ ty2 ->
  head (ty1) ~ head (ty2)
.
Proof.
  induction 1.
  - destruct H ; simpl.
    + apply Ax. constructor.
    + apply Ax. constructor.
    + exact Refl.
  - apply Refl.
  - apply @Trans with (ty2 := head ty2) ; assumption.
  - apply Sym. assumption.
  - apply CongProdLeft. assumption.
  - apply CongProdRight. assumption.
  - apply CongProdLeft. assumption.
  - apply CongProdRight. assumption.
  - apply CongCons. assumption.
Qed.

Lemma head_subst :
  forall ty sigma,
  head (subst sigma ty) = head (subst sigma (head ty))
.
Proof.
  induction ty ; reflexivity || assumption.
Qed.

Lemma cons_equiv_cons :
  forall {ty1 ty2}, ty1 ~ ty2 ->
  forall {f}, IsCons f ty1 \/ IsCons f ty2 ->
  IsCons f ty1 /\ IsCons f ty2
.
Proof.
  intros until f. intros either_is_cons.
  induction H.
  - destruct either_is_cons ; destruct H ; destruct H0 ; discriminate.
  - destruct either_is_cons ; split ; assumption.
  - destruct either_is_cons.
    + assert (IsCons f ty1 \/ IsCons f ty2). left. assumption.
      specialize (IHEquivalent1 H2).
      destruct IHEquivalent1.
      assert (IsCons f ty2 \/ IsCons f ty3). left. assumption.
      specialize (IHEquivalent2 H5).
      destruct IHEquivalent2.
      split ; assumption.
    + assert (IsCons f ty2 \/ IsCons f ty3). right. assumption.
      specialize (IHEquivalent2 H2).
      destruct IHEquivalent2.
      assert (IsCons f ty1 \/ IsCons f ty2). right. assumption.
      specialize (IHEquivalent1 H5).
      destruct IHEquivalent1.
      split ; assumption.
  - apply (proj1 (and_comm (IsCons f ty1) (IsCons f ty2))).
    apply IHEquivalent.
    apply (proj1 (or_comm (IsCons f ty2) (IsCons f ty1))).
    assumption.
  - destruct either_is_cons ; destruct H0 ; discriminate.
  - destruct either_is_cons ; destruct H0 ; discriminate.
  - destruct either_is_cons ; destruct H0 ; discriminate.
  - destruct either_is_cons ; destruct H0 ; discriminate.
  - destruct either_is_cons ; destruct H0 ; inversion H0 ; split.
    + exists tys. reflexivity.
    + exists (Vec.replace tys i ty_i'). reflexivity.
    + exists tys. reflexivity.
    + exists (Vec.replace tys i ty_i'). reflexivity.
Qed.

Lemma equiv_cons_inversion :
  forall {f1 f2 tys1 tys2},
  Cons f1 tys1 ~ Cons f2 tys2 ->
  f1 = f2
.
Proof.
  intros.
  remember (Cons f1 tys1) as ty1.
  remember (Cons f2 tys2) as ty2.
  destruct H.
  - destruct H ; discriminate.
  - rewrite Heqty1 in Heqty2.
    inversion Heqty2.
    reflexivity.
  - assert (IsCons f1 ty1 /\ IsCons f1 ty2).
      apply cons_equiv_cons.
      assumption.
      left. exists tys1. assumption.
    apply proj2 in H1.
    destruct H1.
    assert (IsCons f2 ty2 /\ IsCons f2 ty3).
      apply cons_equiv_cons.
      assumption.
      right. exists tys2. assumption.
    apply proj1 in H2.
    destruct H2.
    rewrite H1 in H2.
    inversion H2.
    reflexivity.
  - assert (IsCons f1 ty1 /\ IsCons f1 ty2).
      apply cons_equiv_cons.
      assumption.
      right. exists tys1. assumption.
    apply proj1 in H0.
    destruct H0.
    rewrite H0 in Heqty2.
    inversion Heqty2.
    reflexivity.
  - discriminate.
  - discriminate.
  - discriminate.
  - discriminate.
  - transitivity f.
    + inversion Heqty1. reflexivity.
    + inversion Heqty2. reflexivity.
Qed.

Theorem feature_head :
  forall {ty1 ty2}, ty1 ◊ ty2 ->
  forall {f1}, IsCons f1 (head ty1) ->
  forall {f2}, IsCons f2 (head ty2) ->
  f1 = f2
.
Proof.
  intros * [sigma equiv] * [tys1 head1_eq_f1] * [tys2 head2_eq_f2].
  apply equiv_head in equiv.
  rewrite (head_subst ty1) in equiv.
  rewrite (head_subst ty2) in equiv.
  rewrite head1_eq_f1 in equiv.
  rewrite head2_eq_f2 in equiv.
  apply (equiv_cons_inversion equiv).
Qed.

(*
   feature 2: tail
*)

Fixpoint mu_cons' f ty :=
  match ty with
  | Var _       => 0
  | ty1 ** ty2  => mu_cons' f ty1 + mu_cons' f ty2
  | _ --> _     => 0
  | Cons f' _   =>
      match Signature.dec f f' with
      | left _  => 1
      | right _ => 0
      end
  end
.

Fixpoint mu_cons f ty :=
  match ty with
  | ty1 --> ty2 => mu_cons' f ty1 + mu_cons f ty2
  | _           => 0
  end
.

Fixpoint mu_var' ty :=
  match ty with
  | Var _       => 1
  | ty1 ** ty2  => mu_var' ty1 + mu_var' ty2
  | _           => 0
  end
.

Fixpoint mu_var ty :=
  match ty with
  | Var _       => 1
  | ty1 --> ty2 => mu_var' ty1 + mu_var ty2
  | _           => 0
  end
.

Definition Simple ty := mu_var ty = 0
.

Definition Simple' ty := mu_var' ty = 0
.

Lemma mu_cons_equiv :
  forall {ty1 ty2}, ty1 ~ ty2 ->
  forall f, mu_cons f ty1 = mu_cons f ty2
.
Proof.
  intros.
  induction H ; try reflexivity.
  - destruct H ; simpl ; try reflexivity.
    symmetry. apply Nat.add_assoc.
  - transitivity (mu_cons f ty2).
    assumption.
    assumption.
  - symmetry.
    assumption.
Qed.

Lemma simple_not_var :
  forall {ty}, Simple ty -> ~ IsVar ty
.
Proof.
  intros * ty_simple [v ty_eq_var].
  subst.
  inversion ty_simple.
Qed.

Lemma mu_cons'_subst_simple' :
  forall {ty}, Simple' ty ->
  forall f sigma, mu_cons' f (subst sigma ty) = mu_cons' f ty
.
Proof.
  intros.
  unfold Simple' in H.
  induction ty ; simpl ; simpl in H.
  - discriminate.
  - apply Plus.plus_is_O in H.
    destruct H.
    specialize (IHty1 H).
    specialize (IHty2 H0).
    rewrite IHty1.
    rewrite IHty2.
    reflexivity.
  - reflexivity.
  - reflexivity.
Qed.

Lemma mu_cons_subst_simple :
  forall {ty}, Simple ty ->
  forall f sigma, mu_cons f (subst sigma ty) = mu_cons f ty
.
Proof.
  intros.
  induction ty ; simpl.
  - exfalso.
    apply (simple_not_var H).
    exists v. reflexivity.
  - reflexivity.
  - unfold Simple in H. simpl in H.
    apply Plus.plus_is_O in H.
    destruct H.
    specialize (IHty2 H0).
    rewrite IHty2.
    rewrite (mu_cons'_subst_simple' H).
    reflexivity.
  - reflexivity.
Qed.

Lemma mu_cons'_subst :
  forall ty f sigma,
  mu_cons' f (subst sigma ty) >= mu_cons' f ty
.
Proof.
  intros.
  induction ty ; simpl.
  - apply le_0_n.
  - apply Plus.plus_le_compat.
    assumption.
    assumption.
  - apply le_0_n.
  - apply le_n.
Qed.

Lemma mu_cons_subst :
  forall ty f sigma,
  mu_cons f (subst sigma ty) >= mu_cons f ty
.
Proof.
  intros.
  induction ty ; simpl.
  - apply le_0_n.
  - apply le_0_n.
  - apply Plus.plus_le_compat.
    apply mu_cons'_subst.
    apply IHty2.
  - apply le_n.
Qed.

Theorem feature_tail :
  forall {ty1}, Simple ty1 ->
  forall {ty2}, ty1 ◊ ty2 ->
  forall f, mu_cons f ty1 >= mu_cons f ty2
.
Proof.
  intros * ty1_simple * [sigma equiv] *.
  apply mu_cons_equiv with f in equiv.
  rewrite (mu_cons_subst_simple ty1_simple f sigma) in equiv.
  pose proof (mu_cons_subst ty2 f sigma).
  rewrite <- equiv in H.
  exact H.
Qed.
