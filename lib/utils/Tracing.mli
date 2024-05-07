include module type of Trace_core

module AC_sols : Bechamel.S.MEASURE with type witness = unit
module Arrow_sols : Bechamel.S.MEASURE with type witness = unit
module Timeout : Bechamel.S.MEASURE with type witness = unit

module Extension : sig
  type 'w t = 'w Bechamel.Measure.measure
  val ac_sols : AC_sols.witness t
  val arrow_sols : Arrow_sols.witness t
  val timeout : Timeout.witness t
end

module Instance : sig
  val ac_sols : Bechamel.Measure.witness
  val arrow_sols : Bechamel.Measure.witness
  val timeout : Bechamel.Measure.witness
end

val wrap_ac_sol : 'a Iter.t -> 'a Iter.t

val wrap_arrow_sol : 'a Iter.t -> 'a Iter.t

val timeout : unit -> unit

val get_nb_ac : unit -> int
val get_nb_arrow : unit -> int
val get_nb_timeout : unit -> int

val wrap_iter :
  ?__FUNCTION__:string ->
  __FILE__:string ->
  __LINE__:int ->
  ?data:(unit -> (string * user_data) list) ->
  ?count:int ref ->
  string ->
  'a Iter.t ->
  'a Iter.t
