
module type S = sig
  type elt
  type t
  val compare : t CCOrd.t
  val of_seq : elt Sequence.t -> t
  val to_seq : t -> elt Sequence.t
  val pp : elt CCFormat.printer -> t CCFormat.printer
end

module Array (E: Set.OrderedType) = struct
  type elt = E.t
  type t = E.t array
  let compare = CCArray.compare E.compare

  let of_seq x =
    let a = Sequence.to_array x in
    Array.sort E.compare a ;
    a

  let to_seq = Sequence.of_array

  let pp ppf fmt a =
    match a with
    | [||] -> CCFormat.string fmt "()"
    | [| x |] -> ppf fmt x
    | _ ->
      CCFormat.fprintf fmt "(@ %a@ )"
        CCFormat.(array ~sep:(return ", ") ppf)
        a

  let as_array x = x
end
