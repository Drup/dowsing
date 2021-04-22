module type S = sig

  type t

  val compute : Type.t -> t
  (* TODO: We should enforce that compatible is monotonic with respect to compare *)
  val compare : t -> t -> Int.t
  val compatible : query:t -> data:t -> Bool.t

end

module Head : S = struct

  type t = Type.Kind.t

  let compute ty = Type.(kind @@ head ty)

  let compare = Type.Kind.compare

  let compatible ~query:(t1 : Type.Kind.t) ~data:(t2 : Type.Kind.t) =
    match t1, t2 with
    | Var, _ | _, Var -> true
    | _ -> Type.Kind.equal t1 t2

end

module Head' : S = struct

  type t = Type.Kind'.t

  let compute ty = Type.(kind' @@ head ty)

  let compare = Type.Kind'.compare

  let compatible ~query:(t1 : Type.Kind'.t) ~data:(t2 : Type.Kind'.t) =
    match t1, t2 with
    | Var, _ | _, Var -> true
    | _ -> Type.Kind'.equal t1 t2

end

module Tail : S = struct

  type order = Eq | GEq

  type t = {
    (* type has at least one spine variable? *)
    ord : order ;
    (* number of tail spine non-variables *)
    cnt : Int.t ;
  }

  let compute ty =
    let spine_var_cnt = Measure.make SpineVarCount ty in
    let ord = if spine_var_cnt = 0 then Eq else GEq in
    let cnt = Measure.make TailSpineNonVarCount ty in
    { ord ; cnt }

  let compare t1 t2 =
    CCOrd.(compare t1.ord t2.ord
      <?> (int, t1.cnt, t2.cnt))

  let compatible ~query:t1 ~data:t2 =
    match t1.ord, t2.ord with
    | Eq,  Eq  -> t1.cnt  = t2.cnt
    | Eq,  GEq -> t1.cnt <= t2.cnt
    | GEq, Eq  -> t1.cnt >= t2.cnt
    | GEq, GEq -> true

end

module Tail' = struct

  type order = Eq | GEq

  type t = {
    (* type has at least one spine variable? *)
    ord : order ;
    (* number of tail spine non-variables *)
    cnt : Int.t ;
    (* kinds of tail spine non-variables *)
    tl : Type.Kind'.MSet.t ;
  }

  let compute ty =
    let spine_var_cnt = Measure.make SpineVarCount ty in
    let ord = if spine_var_cnt = 0 then Eq else GEq in
    let cnt = ref 0 in
    let tl =
      Type.MSet.fold (fun ty kinds ->
        let kind = Type.kind' ty in
        if kind <> Var then begin
          incr cnt ;
          Type.Kind'.MSet.add kinds kind
        end else
          kinds
      ) (Type.tail ty) Type.Kind'.MSet.empty
    in
    { ord ; cnt = ! cnt ; tl }

  let compare t1 t2 =
    CCOrd.(compare t1.ord t2.ord
      <?> (int, t1.cnt, t2.cnt)
      <?> (Type.Kind'.MSet.compare, t1.tl, t2.tl))

  let compatible ~query:t1 ~data:t2 =
    match t1.ord, t2.ord with
    | Eq,  Eq  -> t1.cnt  = t2.cnt && Type.Kind'.MSet.equal    t1.tl t2.tl
    | Eq,  GEq -> t1.cnt >= t2.cnt && Type.Kind'.MSet.contains t1.tl t2.tl
    | GEq, Eq  -> t2.cnt >= t1.cnt && Type.Kind'.MSet.contains t2.tl t1.tl
    | GEq, GEq -> true

end
