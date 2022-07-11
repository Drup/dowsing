let test env t1 ~freeze:t2 =
  let t2 = Type.freeze_variables env t2 in
  Logs.debug (fun m ->
      m "@[<v 2>Checking matching between@ @[%a@]@;<-2> and@ @[%a@]@]" Type.pp
        t1 Type.pp t2);
  Unification.unifiable env t1 t2

type ord = Uncomparable | Smaller | Bigger | Matching_equiv

let pp_ord fmt c =
  match c with
  | Uncomparable -> Format.fprintf fmt "Uncomparable"
  | Smaller -> Format.fprintf fmt "Smaller"
  | Bigger -> Format.fprintf fmt "Bigger"
  | Matching_equiv -> Format.fprintf fmt "Equal"

type hint = Uncompatible | Not_smaller | Not_bigger | Unsure

let combine_hint h1 h2 =
  match (h1, h2) with
  | Uncompatible, _
  | _, Uncompatible
  | Not_bigger, Not_smaller
  | Not_smaller, Not_bigger ->
      Uncompatible
  | Not_bigger, Not_bigger | Not_bigger, Unsure | Unsure, Not_bigger ->
      Not_bigger
  | Not_smaller, Not_smaller | Not_smaller, Unsure | Unsure, Not_smaller ->
      Not_smaller
  | Unsure, Unsure -> Unsure

let pp_hint fmt = function
  | Not_bigger -> Fmt.pf fmt "Not_bigger"
  | Not_smaller -> Fmt.pf fmt "Not_smaller"
  | Unsure -> Fmt.pf fmt "Unsure"
  | Uncompatible -> Fmt.pf fmt "Uncompatible"

let compare ?(hint = Unsure) env (t1 : Type.t) (t2 : Type.t) =
  Logs.debug (fun m ->
      m "Trying match %a %a with hint %a" Type.pp t1 Type.pp t2 pp_hint hint);
  (* let hint = Unsure in *)
  let _maybe_smaller, _maybe_bigger =
    match hint with
    | Unsure -> (true, true)
    | Not_bigger -> (true, false)
    | Not_smaller -> (false, true)
    | Uncompatible -> (false, false)
  in
  let is_smaller = (*maybe_smaller &&*) test env ~freeze:t1 t2 in
  let is_bigger = (*maybe_bigger &&*) test env t1 ~freeze:t2 in
  match (is_smaller, is_bigger) with
  | true, true -> Matching_equiv
  | false, false -> Uncomparable
  | true, false -> Smaller
  | false, true -> Bigger
