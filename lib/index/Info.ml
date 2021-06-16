type t = {
  lid : LongIdent.t ;
  out_ty : Outcometree.out_type ;
}

let compare t1 t2 =
  LongIdent.compare_humans t1.lid t2.lid

let is_internal info =
  info.lid
  |> LongIdent.to_iter
  |> Iter.exists @@ String.mem ~start:0 ~sub:"__"

let pp ppf t =
  Fmt.pf ppf "@[%a :@ %a@]"
    LongIdent.pp t.lid
    !Oprint.out_type t.out_ty
