let test env t1 ~freeze:t2 =
  let t2 = Type.freeze_variables env t2 in
  Logs.debug (fun m ->
      m "@[<v 2>Checking matching between@ @[%a@]@;<-2> and@ @[%a@]@]" Type.pp
        t1 Type.pp t2);
  Unification.unifiable env t1 t2

type ord = Incomparable | Smaller | Bigger | Matching_equiv

let pp_ord fmt c =
  match c with
  | Incomparable -> Format.fprintf fmt "Incomparable"
  | Smaller -> Format.fprintf fmt "Smaller"
  | Bigger -> Format.fprintf fmt "Bigger"
  | Matching_equiv -> Format.fprintf fmt "Equal"

type hint = Incompatible | Maybe_bigger | Maybe_smaller | Unsure

let combine_hint h1 h2 =
  match (h1, h2) with
  | Incompatible, _
  | _, Incompatible
  | Maybe_smaller, Maybe_bigger
  | Maybe_bigger, Maybe_smaller ->
      Incompatible
  | Maybe_smaller, Maybe_smaller
  | Maybe_smaller, Unsure
  | Unsure, Maybe_smaller ->
      Maybe_smaller
  | Maybe_bigger, Maybe_bigger
  | Maybe_bigger, Unsure
  | Unsure, Maybe_bigger ->
      Maybe_bigger
  | Unsure, Unsure -> Unsure

let pp_hint fmt = function
  | Maybe_smaller -> Fmt.pf fmt "Maybe_smaller"
  | Maybe_bigger -> Fmt.pf fmt "Maybe_bigger"
  | Unsure -> Fmt.pf fmt "Unsure"
  | Incompatible -> Fmt.pf fmt "Incompatible"

let compare ?(hint = Unsure) env (t1 : Type.t) (t2 : Type.t) =
  Logs.debug (fun m ->
      m "Trying match %a %a with hint %a" Type.pp t1 Type.pp t2 pp_hint hint);
  match hint with
  | Unsure ->
    let is_smaller = test env ~freeze:t1 t2 in
    let is_bigger = test env t1 ~freeze:t2 in
    begin match (is_smaller, is_bigger) with
      | true, true -> Matching_equiv
      | false, false -> Incomparable
      | true, false -> Smaller
      | false, true -> Bigger
    end
  | Maybe_smaller ->
    if test env ~freeze:t1 t2 then Smaller else Incomparable
  | Maybe_bigger ->
    if test env t1 ~freeze:t2 then Bigger else Incomparable
  | Incompatible -> Incomparable
