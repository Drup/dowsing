Require List.

Require Signature.
Require Var.
Require Vec.

Inductive NTyp : Set :=
  | Var : Var.Var -> NTyp
  | Tuple : list NTyp -> NTyp
  | Arrow : list NTyp -> NTyp
  | Cons : forall f, Vec.t NTyp (Signature.arity f) -> NTyp
.

Infix "-->" := Arrow (at level 30, right associativity).

Reserved Infix "~" (at level 50).
Reserved Infix "≈" (at level 50).

Inductive Equivalent : NTyp -> NTyp -> Prop :=
  | Refl : forall {ty},
      ty ~ ty
  | Trans : forall {ty1 ty2 ty3},
      ty1 ~ ty2 ->
      ty2 ~ ty3 ->
      ty1 ~ ty3
  | Sym : forall {ty1 ty2},
      ty1 ~ ty2 ->
      ty2 ~ ty1
  | CongTuple : forall {tys1 tys2},
      tys1 ≈ tys2 ->
      Tuple tys1 ~ Tuple tys2
  | CongArrowTail : forall {tys tys' ty},
      tys ≈ tys' ->
      tys --> ty ~ tys' --> ty
  | CongArrowHead : forall {tys ty ty'},
      ty ~ ty' ->
      tys --> ty ~ tys --> ty
  | CongCons : forall {f i tys ty_i'},
      Vec.nth tys i ~ ty_i' ->
      Cons f tys ~ Cons f (Vec.replace tys i ty_i')

with EquivalentList : list NTyp -> list NTyp -> Prop :=
  | CongList : forall {i tys ty_i'},
      List.nth NTyp i tys ~ ty_i' ->
      EquivalentList tys (List.replace tys i ty_i')

where "ty1 ~ ty2" := (Equivalent ty1 ty2)
.
