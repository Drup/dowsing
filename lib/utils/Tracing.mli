include module type of Trace_core

val wrap_iter :
  ?__FUNCTION__:string ->
  __FILE__:string ->
  __LINE__:int ->
  ?data:(unit -> (string * user_data) list) ->
  ?count:int ref ->
  string ->
  'a Iter.t ->
  'a Iter.t
