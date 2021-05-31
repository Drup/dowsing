type t = {
  vars : Variable.t List.t ;
  ty : Type.t ;
}

let of_string env str =
  let pos = String.find ~sub:". " str in
  let bdgs, ty =
    str
    |> String.drop (if pos = -1 then 0 else pos + 2)
    |> Type.of_string' env
  in
  let vars =
    if pos = -1 then
      String.HMap.values_list bdgs
    else
      let str = String.trim @@ String.take pos str in
      let vars =
        try CCParse.(parse_string_exn @@ sep ~by:space U.word) str
        with CCParse.ParseError _ -> invalid_arg "Schema.of_string"
      in
      let vars =
        try CCList.map (String.HMap.find bdgs) vars
        with Not_found -> invalid_arg "Schema.of_string"
      in
      vars
  in
  let vars = CCList.sort_uniq ~cmp:Variable.compare vars in
  { vars ; ty }

let to_type t =
  let subst = Variable.HMap.create @@ CCList.length t.vars in
  t.vars |> CCList.iter (fun var ->
    Variable.HMap.add subst var @@
      Type.constr (LongIdent.of_list [ Fmt.to_to_string Variable.pp' var ]) [||]
  ) ;
  Subst.apply subst t.ty

let pp ppf t =
  Fmt.pf ppf "@[<2>%a.@ %a@]"
    Fmt.(list ~sep:sp Variable.pp) t.vars
    Type.pp t.ty
