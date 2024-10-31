type desc =
  | Val of Outcometree.out_type

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
