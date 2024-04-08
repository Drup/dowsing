
(** A set of cycles *)
type t = Variable.t list list
type all = t array
let pp =
  Fmt.Dump.list @@
  CCList.pp ~pp_start:(Fmt.any "{") ~pp_stop:(Fmt.any "}") Variable.pp
let compare = CCList.compare @@ CCList.compare Variable.compare

module Perm = struct
  type t = Variable.t Variable.Map.t

  let pp = Variable.Map.pp Variable.pp Variable.pp
  let compare = Variable.Map.compare Variable.compare

  let as_subst env t : Subst.t = Variable.Map.map (Type.var env) t

  let is_auto env ty perm =
    Type.equal ty (Subst.apply env (as_subst env perm) ty)

  let simplify =
    Variable.Map.filter (fun v v' -> not @@ Variable.equal v v')

  let rec as_cycles perm =
    match Variable.Map.choose_opt perm with
    | None -> []
    | Some (v0,_) ->
      let l = ref [] in
      let rec follow v perm =
        l := v :: !l ;
        let v' = Variable.Map.find v perm in
        let perm = Variable.Map.remove v perm in
        if Variable.equal v' v0 then perm else follow v' perm
      in
      let perm' = follow v0 perm in
      !l :: as_cycles perm'

  let as_cycles perm = List.sort (CCList.compare Variable.compare) @@ as_cycles perm
  
end
  
(** Compile an automorphism to a filter that only accept one
    member of the equivalent class by imposing an order on
    the equivalent variables.
*)

let rec eval_cycle cycle subst =
  match cycle with
  | [] | [_] -> true
  | h :: (h' :: _ as t) ->
    let x = subst h and x' = subst h' in
    let c = Type.compare x x' in
    c < 0 || (c = 0 && eval_cycle t subst)

let eval_automorphim cycles =
  let rec aux cycles subst = match cycles with
    | [] -> true
    | cycle :: rest ->
      eval_cycle (cycle) subst && aux rest subst
  in
  aux cycles

let eval autos subst =
  CCArray.exists (fun l -> eval_cycle l subst) autos

(** Very naive enumeration of automorphisms *)

open Iter.Infix

let rec insert x l = match l with
  | [] -> Iter.return [x]
  | y :: tl ->
    Iter.append
      (insert x tl >|= fun tl' -> y :: tl')
      (Iter.return (x :: l)) ;;
let rec permute l = match l with
  | [] -> Iter.return []
  | x :: tl -> permute tl >>= insert x
let mk_permutation l l' =
  List.fold_left2 (fun m k v -> Variable.Map.add v k m) Variable.Map.empty l l'

let enumerate env ty =
  let vars = Iter.to_list @@ Type.iter_vars ty in
  permute vars
  |> Iter.map (mk_permutation vars)
  |> Iter.filter (Perm.is_auto env ty)
  |> Iter.map Perm.simplify
  |> Iter.filter (CCFun.negate Variable.Map.is_empty)
  |> Iter.map Perm.as_cycles
  |> Iter.sort_uniq ~cmp:compare
  |> Iter.to_array
