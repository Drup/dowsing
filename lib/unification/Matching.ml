let test env t1 ~freeze:t2 =
  Unification.unifiable env t1 (Type.freeze_variables env t2)

type ord = Uncomparable | Smaller | Bigger | Equal

let pp_ord fmt c =
  match c with
  | Uncomparable -> Format.fprintf fmt "Uncomparable"
  | Smaller -> Format.fprintf fmt "Smaller"
  | Bigger -> Format.fprintf fmt "Bigger"
  | Equal -> Format.fprintf fmt "Equal"

let compare ?(compat_leq = true) ?(compat_geq = true) env (t1 : Type.t)
    (t2 : Type.t) =
  match (compat_leq, compat_geq) with
  | false, false -> Uncomparable
  | true, false -> (
      match test env ~freeze:t1 t2 with
      | true -> Smaller
      | false -> Uncomparable
    )
  | false, true -> (
      match test env t1 ~freeze:t2 with
      | true -> Bigger
      | false -> Uncomparable
    )
  | true, true -> (
      let b1 = test env ~freeze:t1 t2
      and b2 = test env t1 ~freeze:t2 in
      match (b1, b2) with
      | true, true -> Equal
      | false, false -> Uncomparable
      | true, false -> Smaller
      | false, true -> Bigger)
