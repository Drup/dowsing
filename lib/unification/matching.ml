type ord = Uncomparable | Smaller | Bigger | Equal

let pp_ord fmt c =
  match c with
  | Uncomparable -> Format.fprintf fmt "Uncomparable"
  | Smaller -> Format.fprintf fmt "Smaller"
  | Bigger -> Format.fprintf fmt "Bigger"
  | Equal -> Format.fprintf fmt "Equal"

let compare ?(compat_leq = true) ?(compat_geq = true) env (t1 : Type.t)
    (t2 : Type.t) =
  let t1f = Type.freeze_variables env t1 in
  let t2f = Type.freeze_variables env t2 in
  match (compat_leq, compat_geq) with
  | false, false -> Uncomparable
  | true, false -> (
      match Unification.unifiable env t1f t2 with
      | true -> Smaller
      | false -> Uncomparable
    )
  | false, true -> (
      match Unification.unifiable env t1 t2f with
      | true -> Bigger
      | false -> Uncomparable
    )
  | true, true -> (
      let b1 = Unification.unifiable env t1f t2
      and b2 = Unification.unifiable env t1 t2f in
      match (b1, b2) with
      | true, true -> Equal
      | false, false -> Uncomparable
      | true, false -> Smaller
      | false, true -> Bigger)
