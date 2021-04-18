module type S = sig

  type t

  val compute : Type.t -> t
  (* TODO: We should enforce that compatible is monotonic with respect to compare *)
  val compare : t -> t -> Int.t
  val compatible : t -> t -> Bool.t

end

module ByHead : S = struct

  type t = Type.Kind.t

  let compute ty = Type.(kind @@ head ty)

  let compare = Type.Kind.compare

  let compatible t1 t2 =
    match t1, t2 with
    | Type.Kind.Var, _ | _, Type.Kind.Var -> true
    | _ -> Type.Kind.equal t1 t2

end

module ByHead' : S = struct

  type t =
    | Var
    | Constr of LongIdent.t
    | Arrow
    | Tuple
    | Other

  let compute ty =
    match Type.head ty with
    | Var _ -> Var
    | Constr (lid, _) -> Constr lid
    | Arrow _ -> Arrow
    | Tuple _ -> Tuple
    | Other _ -> Other

  let to_int = function
    | Var -> 0
    | Constr _ -> 1
    | Arrow -> 2
    | Tuple -> 3
    | Other -> 4

  let compare t1 t2 =
    match t1, t2 with
    | Var, Var
    | Arrow, Arrow
    | Tuple, Tuple
    | Other, Other -> 0
    | Constr lid1, Constr lid2 -> LongIdent.compare lid1 lid2
    | _ -> CCInt.compare (to_int t1) (to_int t2)

  let compatible t1 t2 =
    match t1, t2 with
    | Var, _ | _, Var -> true
    | _ -> compare t1 t2 = 0

end

module TailLength : S = struct

  type order = Eq | GEq

  type t = {
    ord : order ;
    len : Int.t ;
  }

  let compute ty =
    let spine_var_cnt = Type.(size Size.SpineVarCount ty) in
    let ord = if spine_var_cnt = 0 then Eq else GEq in
    let len = Type.(size Size.TailLength ty) in
    { ord ; len }

  let compare t1 t2 =
    CCOrd.(compare t1.ord t2.ord
    <?> (int, t1.len, t2.len))

  let compatible t1 t2 =
    match t1.ord, t2.ord with
    | Eq,  Eq  -> t1.len  = t2.len
    | Eq,  GEq -> t1.len <= t2.len
    | GEq, Eq  -> t1.len >= t2.len
    | GEq, GEq -> true

end
