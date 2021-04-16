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

  let compatible src tgt =
    match src, tgt with
    | Type.Kind.Var, _ | _, Type.Kind.Var -> true
    | _ -> Type.Kind.equal src tgt

end

module TailLength : S = struct

  type order = Eq | GEq

  type t = {
    ord : order ;
    len : Int.t ;
  }

  let compute ty =
    let root_var_cnt = Type.(size Size.RootVarCount ty) in
    let ord = if root_var_cnt = 0 then Eq else GEq in
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
