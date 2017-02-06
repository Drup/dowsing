(** Various import functions *)

open Typexpr

val of_outcometree : ?ht:HC.t -> Outcometree.out_type -> t

val of_parsetree :
  ?gen:Variables.gen ->
  ?ht:HC.t ->
  ?resolver:(Longident.t -> Raw.t array -> Longident.t * Raw.t array) ->
  Parsetree.core_type -> t

val read :
  ?gen:Variables.gen ->
  ?ht:HC.t ->
  ?resolver:(Longident.t -> Raw.t array -> Longident.t * Raw.t array) ->
  Lexing.lexbuf -> Typexpr.t
