module Logs = (val Logs.(src_log @@ Src.create __MODULE__))

type state = No_timeout | Timeout_until of float

let state = Atomic.make No_timeout

exception Timeout

let check () =
  match Atomic.get state with
  | No_timeout -> ()
  | Timeout_until f -> if Unix.gettimeofday () > f then raise Timeout else ()

let with_timeout time f =
  let started_timeout =
    Atomic.compare_and_set state No_timeout
      (Timeout_until (Unix.gettimeofday () +. time))
  in
  if not started_timeout then
    invalid_arg "with_timeout: Nested timeouts are not allowed"
  else
    match f () with
    | v ->
        Atomic.set state No_timeout;
        Ok v
    | exception Timeout ->
        Logs.debug (fun m -> m "Timeout");
        Atomic.set state No_timeout;
        Error ()
    | exception exn ->
        Atomic.set state No_timeout;
        raise exn
