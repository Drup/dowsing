
type info = {
  lid : Typexpr.P.t ;
  source : string ;
}

let compare x1 x2 =
  let (<?>) = CCOrd.(<?>) in
  String.compare x1.source x2.source
  <?> (Typexpr.P.compare, x1.lid, x2.lid)

module NFMap = CCMultiMap.Make(Typexpr)(struct
    type t = info
    let compare = compare
  end)

type t = NFMap.t

let load file : t =
  CCIO.with_in file Marshal.from_channel

let save file (db:t) : unit =
  CCIO.with_out file (fun oc -> Marshal.to_channel oc db [])
