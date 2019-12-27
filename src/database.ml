module P = Typexpr.P

module Info = struct

  type 'a t = 'a list

  type 'a map = 'a t P.Map.t

  let add_new path data map =
    P.Map.add path [data] map

  let add map lid data =
    match P.Map.find lid map with
    | None ->
      add_new lid data map
    | Some t ->
      P.Map.add lid (data :: t) map

  let create lid data = add_new lid data P.Map.empty

  let pp eq ppdata_main ppdata =
    let pp_item ppf (path, aliases) =
      let find l = CCList.find_pred (eq path) l in
      let pp_opt ppf =
        CCOpt.iter @@ fun x ->
        Fmt.pf ppf ":@ %a" ppdata_main x
      in
      let pp_list ppf = function
        | [] -> ()
        | l -> Fmt.(prefix sp @@ list ppdata) ppf l
      in
      Format.fprintf ppf "@[<v2>@[<2>%a%a@]%a@]@."
        P.pp path
        pp_opt (find aliases)
        pp_list (CCList.filter (fun x -> not @@ eq path x) aliases)
    in
    Fmt.vbox (Fmt.iter_bindings P.Map.iter pp_item)

end

module Insert = struct

  type 'a t = {
    lid : P.t ;
    ty : Typexpr.t ;
    data : 'a ;
  }

end

module ByType = struct

  module M = CCMap.Make(Typexpr)

  type 'a t = 'a Info.map M.t

  let add tbl {Insert. ty ; lid ; data} : _ t =
    let ti = try
        let imap = M.find ty tbl in
        Info.add imap lid data
      with Not_found ->
        Info.create lid data
    in
    M.add ty ti tbl

  let of_seq seq : _ t = Iter.fold add M.empty seq

end
type 'a map = 'a ByType.t

module ByHead = struct

  type 'a t = {
    var : 'a map ;
    constr : 'a map P.Map.t ;
    tup : 'a map ;
    others : 'a map ;
    unit : 'a map ;
  }

  let empty = {
    var = ByType.M.empty ;
    constr = P.Map.empty ;
    tup = ByType.M.empty ;
    others = ByType.M.empty ;
    unit = ByType.M.empty ;
  }

  let add t k =
    match Typexpr.Head.get k.Insert.ty with
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

  let find t ty : _ Info.map =
    try
      match Typexpr.Head.get ty with
      | Constr p ->
        let m = P.Map.find_exn p t.constr in
        ByType.M.find ty m
      | Var -> ByType.M.find ty t.var
      | Tuple -> ByType.M.find ty t.tup
      | Other -> ByType.M.find ty t.others
      | Unit -> ByType.M.find ty t.unit
    with Not_found -> P.Map.empty

  let of_seq seq : 'a t = Iter.fold add empty seq

  let fuse t : 'a ByType.t =
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
      |> Iter.map ByType.M.cardinal
      |> Iter.fold (+) 0
    in
    let total = vars + tup + others + unit + constr_total in
    Format.fprintf ppf
      "Total: %i@ @[<v2>Heads:@ Vars: %i@ Tuples: %i@ \
       Unit: %i@ Others: %i@ Constr: %i in %i buckets@ @]"
      total vars tup unit others constr_total constr_slots
end

type 'a t = 'a ByHead.t
let of_seq = ByHead.of_seq
let find = ByHead.find

let load file : 'a t =
  CCIO.with_in file Marshal.from_channel

let save file (db : 'a t) : unit =
  CCIO.with_out file (fun oc -> Marshal.to_channel oc db [])
