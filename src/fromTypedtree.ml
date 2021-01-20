module F = Typexpr.Raw
open Types

let to_longident = Untypeast.lident_of_path

let map_al f l =
  Iter.of_list l
  |> Iter.map f
  |> Iter.to_array

let rec to_typexpr_raw x = match x.desc with
  | Tvar x
  | Tunivar x -> F.var x
  | Tarrow (_,arg,ret,_) ->
    let arg = to_typexpr_raw arg in
    let ret = to_typexpr_raw ret in
    F.arrow arg ret
  | Ttuple tup ->
    let tup = Iter.of_list tup in
    F.tuple (Iter.map (to_typexpr_raw) tup |> Iter.to_list)
  | Tconstr (p,args,_) ->
    let lid = to_longident p in
    let args = map_al to_typexpr_raw args in
    F.constr lid args
  | Tlink t
  | Tsubst t
  | Tpoly (t,_) -> to_typexpr_raw t

  | Tobject (_,_)
  | Tfield (_,_,_,_)
  | Tnil
  | Tvariant _
  | Tpackage (_,_,_) ->
    F.unknown x.desc

let to_typexpr ~gen ~ht x =
  let raw = to_typexpr_raw x in
  Typexpr.normalize ~gen ~ht raw
