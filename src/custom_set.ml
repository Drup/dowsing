
module type S = sig
  type elt
  type t
  val add : elt -> t -> t
  val singleton : elt -> t
  val union : t -> t -> t
  val compare : t CCOrd.t
  val of_seq : elt Sequence.t -> t
  val to_seq : t -> elt Sequence.t
  val map : (elt -> elt) -> t -> t
end

module Array (E: Set.OrderedType) = struct
  type elt = E.t
  type t = E.t array
  let compare = CCArray.compare E.compare
  let singleton x = [|x|]
  let of_seq = Sequence.to_array
  let to_seq = Sequence.of_array
  let map f x =
    let a = Array.map f x in
    Array.sort E.compare a ;
    a

  let union a1 a2 =
    let l1 = Array.length a1 in
    let l2 = Array.length a2 in
    let l = l1 + l2 in
    if l = 0 then [||]
    else begin
      let init_el = if l1 > 0 then a1.(0) else a2.(0) in
      let a = Array.make l init_el in
      let i1 = ref 0 and i2 = ref 0 in
      for i = 0 to l - 1 do
        let ex =
          if !i1 >= l1 then let e = a2.(!i2) in (incr i2 ; e)
          else if !i2 >= l2 then let e = a1.(!i1) in (incr i1 ; e)
          else begin
            let e1 = a1.(!i1) and e2 = a2.(!i2) in
            let c = E.compare e1 e2 in
            if c >= 0 then begin
              incr i1; e1
            end else begin
              incr i2 ; e2
            end
          end
        in
        a.(i) <- ex
      done;
      a
    end

  let add x a = union [|x|] a
end
