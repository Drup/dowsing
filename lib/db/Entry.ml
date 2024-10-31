type desc =
  | Val of Outcometree.out_type
  | Type of {
      params : string option list ;
      manifest : Outcometree.out_type option ;
    }

type t = {
  lid : LongIdent.t ;
  desc : desc ;
  pkg : string ;
  source_file : Fpath.t ;
}

let compare t1 t2 =
  LongIdent.compare_humans t1.lid t2.lid

let is_internal info =
  info.lid
  |> LongIdent.to_iter
  |> Iter.exists @@ String.mem ~start:0 ~sub:"__"

let pp ppf { lid; desc; pkg; source_file = _ } =
  match desc with
  | Val ty ->
    Fmt.pf ppf "@[%s:%a :@ %a@]"
      pkg
      LongIdent.pp lid
      !Oprint.out_type ty
  | Type {params; manifest} ->
    let pp_params ppf =
      match params with
      | [] -> ()
      | _ ->
        Fmt.pf ppf "(%a) "
          (Fmt.list ~sep:Fmt.comma @@ Fmt.option ~none:(Fmt.any "_") Fmt.string)
          params
    in
    Fmt.pf ppf "@[%s:%t%a%a@]"
      pkg
      pp_params
      LongIdent.pp lid
      Fmt.(option (Fmt.any " =@ " ++ !Oprint.out_type)) manifest
      
