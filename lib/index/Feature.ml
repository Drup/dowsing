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
    | Type.Kind.Var, _
    | _, Type.Kind.Var -> true
    | _ -> Type.Kind.equal t1 t2

end

module ByHead' : S = struct

  type t = Type.Kind'.t

  let compute ty = Type.(kind' @@ head ty)

  let compare = Type.Kind'.compare

  let compatible t1 t2 =
    match t1, t2 with
    | Type.Kind'.Var, _
    | _, Type.Kind'.Var -> true
    | _ -> Type.Kind'.equal t1 t2

end

module TailLength : S = struct

  type order = Eq | GEq

  type t = {
    ord : order ;
    len : Int.t ;
  }

  let compute ty =
    let spine_var_cnt = Measure.size SpineVarCount ty in
    let ord = if spine_var_cnt = 0 then Eq else GEq in
    let len = Measure.size TailLength ty in
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

(* module BySpine : S = struct *)

(*   type t = { *)
(*     hd : Type.Kind'.t ; *)
(*     tl : Type.Kind'.MSet.t ; *)
(*   } *)

(*   let compute ty = *)
(*     let hd = Type.(kind' @@ head ty) in *)
(*     let tl = *)
(*     Type.MSet.fold (fun ty kinds -> *)
(*       Type.Kind'.MSet.add kinds @@ Type.kind' ty *)
(*     ) (Type.tail ty) Type.Kind'.MSet.empty *)
(*     in *)
(*     { hd ; tl } *)

(*   let compare t1 t2 = *)
(*     CCOrd.(Type.Kind'.compare t1.hd t2.hd *)
(*     <?> (Type.Kind'.MSet.compare, t1.tl, t2.tl)) *)

(*   let compatible t1 t2 = *)
(*     let is_var = (=) Type.Kind'.Var in *)
(*     let var_cnt1, var_cnt2 = *)
(*       let aux acc _ kind = acc + CCBool.to_int @@ is_var kind in *)
(*       let aux t = Type.Kind'.MSet.fold t.tl 0 aux in *)
(*       aux t1, aux t2 *)
(*     in *)
(*     match is_var t1.hd, is_var t2.hd with *)
(*     | true, true -> *)
(*     | true, false *)
(*     | false, true -> *)
(*     | false, false -> *)

(* end *)
