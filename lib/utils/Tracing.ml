module Trace = Trace_core
include Trace

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
