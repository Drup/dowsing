(** Various import functions *)

open Typexpr

val of_outcometree : 
  gen:Variables.gen ->
  ht:Hashcons.t ->
  nametbl:label Variables.HMap.t ->
  Outcometree.out_type -> t

val of_parsetree :
  ?resolver:(Longident.t -> Raw.t array -> Longident.t * Raw.t array) ->
  gen:Variables.gen ->
  ht:Hashcons.t ->
  nametbl:label Variables.HMap.t ->
  Parsetree.core_type -> t

val read :
  ?resolver:(Longident.t -> Raw.t array -> Longident.t * Raw.t array) ->
  gen:Variables.gen ->
  ht:Hashcons.t ->
  nametbl:label Variables.HMap.t ->
  Lexing.lexbuf -> Typexpr.t
