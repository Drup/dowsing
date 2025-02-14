type t = {
  lid : LongIdent.t ;
  ty : Outcometree.out_type ;
  pkg : string ;
  source_file : Fpath.t ;
}

let compare t1 t2 =
  LongIdent.compare_humans t1.lid t2.lid

let is_internal info =
  info.lid
  |> LongIdent.to_iter
  |> Iter.exists @@ String.mem ~start:0 ~sub:"__"

let pp ppf t =
  Fmt.pf ppf "@[%s:%a :@ %a@]"
    t.pkg
    LongIdent.pp t.lid
    (Format_doc.compat !Oprint.out_type) t.ty
