module type S = sig
  type t

  val compute : Type.Env.t -> Type.t -> t

  (** TODO: We should enforce that compatible is monotonic with respect to
      compare *)
  val compare : t -> t -> int
  val compatible : source:t -> target:t -> bool 
end

module ByHead = struct
  type t = Type.Kind.t
  let compute _env ty = Type.(kind @@ head ty)
  let compare = Type.Kind.compare
  let compatible ~source ~target = match source, target with
    | Type.Kind.Var, _ | _, Type.Kind.Var -> true
    | _ -> Type.Kind.equal source target
      
end 

module TailLength = struct
  type order = Eq | GEq
  type t = { order : order ; length : int }

  let compute _env ty =
    let root_var_cnt = Type.(size Size.RootVarCount ty) in
    let length = Type.(size Size.TailLength ty) in
    let order = if root_var_cnt = 0 then Eq else GEq in
    { order ; length }

  let compare tl1 tl2 =
    CCOrd.(compare tl1.order tl2.order <?> (int, tl1.length, tl2.length))
  
  let compatible ~source:tl1 ~target:tl2 =
    match tl1.order, tl2.order with
    | Eq,  Eq  -> tl1.length = tl2.length
    | Eq,  GEq -> tl1.length <= tl2.length
    | GEq, Eq  -> tl1.length >= tl2.length
    | GEq, GEq -> true
end
