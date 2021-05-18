Require Signature.
Require Var.
Require Vec.

Inductive Typ : Set :=
  | Var : Var.Var -> Typ
  | Unit : Typ
  | Prod : Typ -> Typ -> Typ
  | Arrow : Typ -> Typ -> Typ
  | Cons : forall f, Vector.t Typ (Signature.arity f) -> Typ
.

Infix "**" := Prod (at level 20, right associativity).
Infix "-->" := Arrow (at level 30, right associativity).

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
  | Unit        => Unit
  | ty1 ** ty2  => subst sigma ty1 ** subst sigma ty2
  | ty1 --> ty2 => subst sigma ty1 --> subst sigma ty2
  | Cons f tys  => Cons f (Vector.map (subst sigma) tys)
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
  | ProdUnit : forall ty,
      Unit ** ty ≈
      ty
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
      Vector.nth tys i ~ ty_i' ->
      Cons f tys ~ Cons f (Vector.replace tys i ty_i')

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
