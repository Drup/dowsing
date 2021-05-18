Require Signature.
Require Typ.
Require Var.
Require Vec.

Inductive NTypNonUnit : Set :=
  | Var : Var.Var -> NTypNonUnit
  | Prod : NTypNonUnit -> NTypNonUnit -> NTypNonUnit
  | Arrow : NTyp -> NTyp -> NTypNonUnit
  | Cons : forall f, Vec.t NTyp (Signature.arity f) -> NTypNonUnit

with NTyp : Set :=
  | Unit : NTyp
  | NonUnit : NTypNonUnit -> NTyp
.

Fixpoint normalize ty :=
  match ty with
  | Typ.Var v => NonUnit (Var v)
  | Typ.Unit => Unit
  | Typ.Prod ty1 ty2 =>
      match normalize ty1, normalize ty2 with
      | Unit, Unit => Unit
      | Unit, nty => nty
      | nty, Unit => nty
      | NonUnit nty1, NonUnit nty2 => NonUnit (Prod nty1 nty2)
      end
  | Typ.Arrow ty1 ty2 => NonUnit (Arrow (normalize ty1) (normalize ty2))
  | Typ.Cons f tys => NonUnit (Cons f (Vec.map normalize tys))
  end
.

(* Theorem normalize_correctness : *)
(*   forall ty1 ty2, *)
(*   Typ.Equivalent ty1 ty2 <-> normalize ty1 ~ normalize ty2 *)
(* . *)
(* Proof. *)
(* Admitted. *)

(*
   feature 1: head
*)

Fixpoint head ty :=
  match ty with
  | NonUnit (Arrow _ nty) => head nty
  | _ => ty
  end
.

Theorem feature_head :
  forall {ty1 ty2},
  normalize ty1 ◊ normalize ty2 ->
  forall {f1}, IsCons f1 (head (normalize ty1)) ->
  forall {f2}, IsCons f2 (head (normalize ty2)) ->
  f1 = f2
.
Proof.
Admitted.
