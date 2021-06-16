module type S = sig

  type t

  val compute : Type.t -> t
  val compare : t CCOrd.t
  val compatible : query:t -> data:t -> Bool.t

end

(*
   Strict type equality.
   Mostly used to obtain equivalence classes modulo AC.
   Obviously, we can't say anything about compatibility
*)
module TypeEq : S = struct

  type t = Type.t

  let compute x = x
  let compare = Type.compare
  let compatible ~query:_ ~data:_ = true

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

  type t = {
    (* type has at least one spine variable? *)
    has_var : Bool.t ;
    (* number of tail spine non-variables *)
    cnt : Int.t ;
  }

  let compute ty =
    let has_var = Measure.make SpineVarCount ty > 0 in
    let cnt = Measure.make TailSpineNonVarCount ty in
    { has_var ; cnt }

  let compare t1 t2 =
    CCOrd.(bool t1.has_var t2.has_var
      <?> (int, t1.cnt, t2.cnt))

  let compatible ~query:t1 ~data:t2 =
    match t1.has_var, t2.has_var with
    | false,  false -> t1.cnt  = t2.cnt
    | false,  true  -> t1.cnt <= t2.cnt
    | true,   false -> t1.cnt >= t2.cnt
    | true,   true  -> true

end

module Tail' = struct

  type t = {
    (* type has at least one spine variable? *)
    has_var : Bool.t ;
    (* number of tail spine non-variables *)
    cnt : Int.t ;
    (* kinds of tail spine non-variables *)
    tl : Type.Kind'.MSet.t ;
  }

  let compute ty =
    let has_var = Measure.make SpineVarCount ty > 0 in
    let cnt = ref 0 in
    let tl =
      Type.NSet.fold (fun ty kinds ->
        let kind = Type.kind' ty in
        if kind <> Var then begin
          incr cnt ;
          Type.Kind'.MSet.add kinds kind
        end else
          kinds
      ) (Type.tail ty) Type.Kind'.MSet.empty
    in
    { has_var ; cnt = !cnt ; tl }

  let compare t1 t2 =
    CCOrd.(bool t1.has_var t2.has_var
      <?> (int, t1.cnt, t2.cnt)
      <?> (Type.Kind'.MSet.compare, t1.tl, t2.tl))

  let compatible ~query:t1 ~data:t2 =
    match t1.has_var, t2.has_var with
    | false,  false -> t1.cnt  = t2.cnt && Type.Kind'.MSet.equal    t1.tl t2.tl
    | false,  true  -> t1.cnt >= t2.cnt && Type.Kind'.MSet.contains t1.tl t2.tl
    | true,   false -> t2.cnt >= t1.cnt && Type.Kind'.MSet.contains t2.tl t1.tl
    | true,   true  -> true

end

module Constructors : S = struct

  type t = {
    mutable has_var : Bool.t ;
    mutable constrs : Int.t Type.Kind'.Map.t ;
  }

  let compute ty =
    let t = {
      has_var = false ;
      constrs = Type.Kind'.Map.empty ;
    } in
    Type.iter ty (fun ty ->
      let kind = Type.kind' ty in
      match kind with
      | Var ->
          t.has_var <- true
      | Constr _ | Arrow | Tuple ->
          t.constrs <-
            Type.Kind'.Map.update kind (function
              | None -> Some 1
              | Some cnt -> Some (cnt + 1)
            ) t.constrs
      | _ -> ()
    ) ;
    t

  let compare t1 t2 =
    CCOrd.(Bool.compare t1.has_var t2.has_var
      <?> (compare, t1.constrs, t2.constrs))

  let compatible =
    let aux t1 t2 =
      Type.Kind'.Map.for_all (fun kind cnt2 ->
        let cnt1 = Type.Kind'.Map.get_or kind t1.constrs ~default:0 in
        cnt1 >= cnt2
      ) t2.constrs
    in
    fun ~query:t1 ~data:t2 ->
      match t1.has_var, t2.has_var with
      | false, true  -> aux t1 t2
      | false, false -> aux t1 t2
      | true,  false -> aux t2 t1
      | true,  true  -> true

end
