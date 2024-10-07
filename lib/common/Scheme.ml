type t = {
  vars : (Variable.t * Type.t) List.t ;
  ty : Type.t ;
}

type flags = NonArrow | NonTuple

type var_type = Frozen | Flags of Variable.Flags.t

let parse_var_type =
  let open CCParse in
  (string "^" >|= fun _ -> Frozen) <|>
  (many1 ((string ">" >|= fun _ -> NonArrow)
  <|> (string "*" >|= fun _ -> NonTuple))
  >|= fun l -> Flags (List.fold_left (fun flags m ->
                        match m with
                        | NonArrow -> Variable.Flags.(set non_arrow flags)
                        | NonTuple -> Variable.Flags.(set non_tuple flags))
                        Variable.Flags.empty l))

let parse_var =
  let open CCParse in
  let* var_type = parse_var_type in
  let+ name = U.word in
  (name, var_type)

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
      |> CCList.map (fun var -> (var, Type.frozen_var env var))
    else
      let str = String.trim @@ String.take pos str in
      let vars =
        try CCParse.(parse_string_exn @@ (
          let* l = sep ~by:space parse_var in
          let+ () = eoi in
          l)) str
        with CCParse.ParseError _ -> invalid_arg "Schema.of_string"
      in
      let vars =
        try
          CCList.map (fun (name, var_type) ->
            let var = String.HMap.find bdgs name in
            match var_type with
              | Frozen -> (var, Type.frozen_var env var)
              | Flags flags ->
                let fresh_var = Variable.Gen.gen flags env.var_gen in
                (var, Type.var env fresh_var)
          ) vars
        with Not_found -> invalid_arg "Schema.of_string"
      in
      vars
  in
  let vars = CCList.sort_uniq ~cmp:(fun (v1, _) (v2, _) -> Variable.compare v1 v2) vars in
  { vars ; ty }

let to_type env t =
  let subst =
    t.vars
    |> Variable.Map.of_list
  in
  Subst.apply env subst t.ty

let pp ppf t =
  Fmt.pf ppf "@[<2>%a.@ %a@]"
    Fmt.(list ~sep:sp (fun fmt (v, _) -> Variable.pp fmt v)) t.vars
    Type.pp t.ty
