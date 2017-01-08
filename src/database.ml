open Typexpr

module Info = struct

  type data = {
    lid : P.t ;
    source : string ;
  }

  type t = {
    ty : Outcometree.out_type ;
    data : data ;
    aliases : data list ;
  }

  (* Inefficient, but it shouldn't matter *)
  module M = CCMap.Make(P)
  type map = t M.t

  let add_new lid data ty map =
    if P.compare data.lid lid = 0 then
      M.add lid { ty ; data ; aliases = [] } map
    else
      let odata = { lid ; source = data.source } in
      M.add lid { ty ; data = odata ; aliases = [data] } map

  let add map lid data ty =
    match M.get lid map with
    | None ->
      add_new lid data ty map
    | Some t ->
      M.add lid {t with aliases = data :: t.aliases } map

  let create lid data ty = add_new lid data ty M.empty

  let pp =
    let pp_item ppf t =
      match t.aliases with
      | [] ->
        Format.fprintf ppf "@[<2>%a:@ %a@]@."
          P.pp t.data.lid
          !Oprint.out_type t.ty
      | l ->
        Format.fprintf ppf "@[<v2>@[<2>%a:@ %a@]@ %a@]@."
          P.pp t.data.lid
          !Oprint.out_type t.ty
          (Format.pp_print_list (fun ppf x -> P.pp ppf x.lid)) l
    in
    CCFormat.vbox (fun ppf m -> M.values m (pp_item ppf))

end

module NFMap = CCMap.Make(Typexpr)

type t = Info.map NFMap.t
let add tbl (ty, lid, data, oty) : t =
  let ti = try
      let imap = NFMap.find ty tbl in
      Info.add imap lid data oty
    with Not_found ->
      Info.create lid data oty
  in
  NFMap.add ty ti tbl

let of_seq seq : t = Sequence.fold add NFMap.empty seq

let load file : t =
  CCIO.with_in file Marshal.from_channel

let save file (db:t) : unit =
  CCIO.with_out file (fun oc -> Marshal.to_channel oc db [])
