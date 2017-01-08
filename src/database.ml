module P = Typexpr.P

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

  type map = t P.Map.t

  let add_new lid data ty map =
    if P.compare data.lid lid = 0 then
      P.Map.add lid { ty ; data ; aliases = [] } map
    else
      let odata = { lid ; source = data.source } in
      P.Map.add lid { ty ; data = odata ; aliases = [data] } map

  let add map lid data ty =
    match P.Map.find lid map with
    | None ->
      add_new lid data ty map
    | Some t when lid = data.lid ->
      (* We inserted the lid earlier, thanks to an orig_lid, but we only
         get the actual data now. *)
      P.Map.add lid {t with data} map
    | Some t ->
      P.Map.add lid {t with aliases = data :: t.aliases } map

  let create lid data ty = add_new lid data ty P.Map.empty

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
    CCFormat.vbox (fun ppf m -> P.Map.iter_values (pp_item ppf) m)

end

module ByType = struct

  module M = CCMap.Make(Typexpr)

  type t = Info.map M.t

  let add tbl (ty, lid, data, oty) : t =
    let ti = try
        let imap = M.find ty tbl in
        Info.add imap lid data oty
      with Not_found ->
        Info.create lid data oty
    in
    M.add ty ti tbl

  let of_seq seq : t = Sequence.fold add M.empty seq

end
type map = ByType.t

module ByHead = struct

  type t = {
    var : map ;
    constr : map P.Map.t ;
    tup : map ;
    others : map ;
    unit : map ;
  }

  let empty = {
    var = ByType.M.empty ;
    constr = P.Map.empty ;
    tup = ByType.M.empty ;
    others = ByType.M.empty ;
    unit = ByType.M.empty ;
  }

  let add t ((ty, _, _, _) as k) =
    match Typexpr.Head.get ty with
    | Constr p ->
      let m = match P.Map.find p t.constr with
        | None -> ByType.M.empty
        | Some m -> m
      in
      let constr = P.Map.add p (ByType.add m k) t.constr in
      {t with constr}
    | Var -> {t with var = ByType.add t.var k}
    | Tuple -> {t with tup = ByType.add t.tup k}
    | Other -> {t with others = ByType.add t.others k}
    | Unit -> {t with unit = ByType.add t.unit k}

  let find t ty =
    match Typexpr.Head.get ty with
    | Constr p ->
      let m = P.Map.find_exn p t.constr in
      ByType.M.find ty m
    | Var -> ByType.M.find ty t.var
    | Tuple -> ByType.M.find ty t.tup
    | Other -> ByType.M.find ty t.others
    | Unit -> ByType.M.find ty t.unit

  let of_seq seq : t = Sequence.fold add empty seq

  let fuse t : ByType.t =
    let (<+>) = ByType.M.union (fun _ a _ -> Some a) in
    t.var <+> t.tup <+> t.others <+> t.unit
    |> fun x -> P.Map.fold_values (<+>) x t.constr

  let pp_stat ppf { var ; tup ; others ; unit ; constr } =
    let vars = ByType.M.cardinal var in
    let tup = ByType.M.cardinal tup in
    let others = ByType.M.cardinal others in
    let unit = ByType.M.cardinal unit in
    let constr_slots = P.Map.size constr in
    let constr_total =
      P.Map.to_seq_values constr
      |> Sequence.map ByType.M.cardinal
      |> Sequence.fold (+) 0
    in
    let total = vars + tup + others + unit + constr_total in
    Format.fprintf ppf
      "Total: %i@ @[<v2>Heads:@ Vars: %i@ Tuples: %i@ \
       Unit: %i@ Others: %i@ Constr: %i in %i buckets@ @]"
      total vars tup unit others constr_total constr_slots
end

type t = ByHead.t
let of_seq = ByHead.of_seq
let find = ByHead.find

let load file : t =
  CCIO.with_in file Marshal.from_channel

let save file (db:t) : unit =
  CCIO.with_out file (fun oc -> Marshal.to_channel oc db [])
