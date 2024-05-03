include Trace_core

type data = { mutable ac_sols : int; mutable arrow_sols : int }

let data = { ac_sols = 0; arrow_sols = 0 }

module AC_sols = struct
  type witness = unit

  let load () = data.ac_sols <- 0
  let unload () = data.ac_sols <- 0
  let make () = data.ac_sols <- 0
  let get () = float_of_int data.ac_sols
  let label () = "ac-sols"
  let unit () = "sols"
end

module Arrow_sols = struct
  type witness = unit

  let load () = data.arrow_sols <- 0
  let unload () = data.arrow_sols <- 0
  let make () = data.arrow_sols <- 0
  let get () = float_of_int data.arrow_sols
  let label () = "arrow-sols"
  let unit () = "sols"
end


let wrap_ac_sol (i : 'a Iter.t) : 'a Iter.t =
 fun k ->
  i (fun x ->
      data.ac_sols <- data.ac_sols + 1;
      k x)

let wrap_arrow_sol (i : 'a Iter.t) : 'a Iter.t =
 fun k ->
  i (fun x ->
      data.arrow_sols <- data.arrow_sols + 1;
      k x)

let get_nb_ac () = data.ac_sols
let get_nb_arrow () = data.arrow_sols

module Extension = struct
  type 'w t = 'w Bechamel.Measure.measure

  let ac_sols = Bechamel.Measure.register (module AC_sols)
  let arrow_sols = Bechamel.Measure.register (module Arrow_sols)
end

module Instance = struct
  let ac_sols = Bechamel.Measure.instance (module AC_sols) Extension.ac_sols
  let arrow_sols = Bechamel.Measure.instance (module Arrow_sols) Extension.arrow_sols
end

let wrap_iter ?__FUNCTION__ ~__FILE__ ~__LINE__ ?data ?count name iter k =
  let k_name = "Continuation: " ^ name in
  let k =
    match count with
    | Some count ->
        fun x ->
          incr count;
          with_span ?__FUNCTION__ ~__FILE__ ~__LINE__
            ~data:(fun () -> [ ("n", `Int !count) ])
            k_name
            (fun _sp -> k x)
    | None ->
        fun x ->
          with_span ?__FUNCTION__ ~__FILE__ ~__LINE__ k_name (fun _sp -> k x)
  in
  with_span ?__FUNCTION__ ~__FILE__ ~__LINE__ ?data name (fun _sp -> iter k)
