module Logs = (val Logs.(src_log @@ Src.create __MODULE__))

type state =
  | No_timeout
  | Timeout_until of float

let state = ref No_timeout

exception Timeout

let check () =
  match !state with
  | No_timeout -> ()
  | Timeout_until f ->
      if Unix.gettimeofday () > f then raise Timeout else ()

let with_timeout time f =
  begin match !state with No_timeout -> () | Timeout_until _ ->
    invalid_arg "with_timeout: Nested timeouts are not allowed"
  end;
  state := Timeout_until (Unix.gettimeofday () +. time);
  match f () with
  | v ->
    state := No_timeout;
    Ok v
  | exception Timeout ->
    Logs.debug (fun m -> m "Timeout"); 
    state := No_timeout;
    Error ()
  | exception exn ->
    state := No_timeout;
    raise exn
