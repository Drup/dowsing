Require Export Typ.
Require Vec.

Fixpoint normalize ty :=
  match ty with
  | Var _           => ty
  | Unit            => Unit
  | ty1 ** ty2 =>
      match normalize ty1, normalize ty2 with
      | Unit, Unit  => Unit
      | Unit, ty    => ty
      | ty, Unit    => ty
      | ty1, ty2    => ty1 ** ty2
      end
  | ty1 --> ty2     => normalize ty1 --> normalize ty2
  | Cons f tys      => Cons f (Vec.map normalize tys)
  end
.

Theorem normalize_correctness :
  forall ty1 ty2,
  ty1 ~ ty2 <-> normalize ty1 ~ normalize ty2
.
Proof.
Admitted.

(*
   feature 1: head
*)

Fixpoint head ty :=
  match ty with
  | _ --> ty => head ty
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
