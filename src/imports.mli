(** Various import functions *)

open Typexpr

(** Import from outcometree *)
val of_outcometree : 
  gen:Variables.gen ->
  ht:Hashcons.t ->
  nametbl:label Variables.HMap.t ->
  Outcometree.out_type -> t

(** Import from ocaml parser *)
val of_parsetree :
  ?resolver:(Longident.t -> Raw.t array -> Longident.t * Raw.t array) ->
  gen:Variables.gen ->
  ht:Hashcons.t ->
  nametbl:label Variables.HMap.t ->
  Parsetree.core_type -> t

(** Use ocaml parser to parse, and then import *)
val read :
  ?resolver:(Longident.t -> Raw.t array -> Longident.t * Raw.t array) ->
  gen:Variables.gen ->
  ht:Hashcons.t ->
  nametbl:label Variables.HMap.t ->
  Lexing.lexbuf -> Typexpr.t
