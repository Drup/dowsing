open Typexpr
module S = Iter


(** Import from Outcometree *)

let rec lid_of_outcometree
  : Outcometree.out_ident -> Longident.t =
  function
  | Oide_apply (id1,id2) ->
    Lapply (lid_of_outcometree id1, lid_of_outcometree id2)
  | Oide_dot (id, s) ->
    Ldot (lid_of_outcometree id, s)
  | Oide_ident {printed_name=s} ->
    Lident s

let rec of_outcometree_rec x : Raw.t =
  let open Outcometree in match x with
  | Otyp_arrow (_label, arg, ret) ->
    let arg' = of_outcometree_rec arg in
    let ret' = of_outcometree_rec ret in
    Raw.arrow arg' ret'
  | Otyp_constr (id, args) ->
    let a =
      S.to_array @@
      S.map of_outcometree_rec @@
      S.of_list args
    in
    Raw.constr (lid_of_outcometree id) a
  | Otyp_tuple tup ->
    let tup' =
      List.map of_outcometree_rec tup
    in
    Raw.tuple tup'
  | Otyp_var (_, s) -> Raw.var (Some s)
  | Otyp_alias (ty, _) -> of_outcometree_rec ty

  (* Not handled *)
  | Otyp_object _
  | Otyp_class _
  | Otyp_variant _
  | Otyp_module _
  | Otyp_attribute _

  (* Not simple types *)
  | Otyp_stuff _
  | Otyp_poly _
  | Otyp_abstract
  | Otyp_open
  | Otyp_manifest _
  | Otyp_record _
  | Otyp_sum _
    -> Raw.unknown x

let of_outcometree ~gen ~ht ~nametbl x =
  let t = of_outcometree_rec x in
  normalize ~gen ~ht ~nametbl t

(** Import from Parsetree *)

let rec of_parse_rec resolver x : Raw.t =
  let open Parsetree in match x.ptyp_desc with
  | Ptyp_arrow (_label, arg, ret) ->
    let arg' = of_parse_rec resolver arg in
    let ret' = of_parse_rec resolver ret in
    Raw.arrow arg' ret'
  | Ptyp_constr (id, args) ->
    let a =
      S.to_array @@
      S.map (of_parse_rec resolver) @@
      S.of_list args
    in
    let id, a = resolver id.txt a in
    Raw.constr id a
  | Ptyp_tuple tup ->
    let tup' =
      List.map (of_parse_rec resolver) tup
    in
    Raw.tuple tup'
  | Ptyp_any ->
    Raw.var None
  | Ptyp_var s ->
    Raw.var (Some s)

  | Ptyp_alias (ty, _)
  | Ptyp_poly (_, ty) -> of_parse_rec resolver ty

  (* Not handled *)
  | Ptyp_object _
  | Ptyp_class _
  | Ptyp_variant _
  | Ptyp_package _
  | Ptyp_extension _
    -> Raw.unknown x

let default_resolver id a = id, a

let of_parsetree ?(resolver=default_resolver) ~gen ~ht ~nametbl x =
  (* Format.eprintf "@[ptree: %a@]@." Pprintast.core_type x; *)
  let t = of_parse_rec resolver x in
  (* Format.eprintf "@[raw: %a@]@." Typexpr.pp_raw t; *)
  normalize ~gen ~ht ~nametbl t

let read ?resolver ~gen ~ht ~nametbl lexbuf =
  of_parsetree ?resolver ~gen ~ht ~nametbl @@
  Parse.core_type lexbuf
