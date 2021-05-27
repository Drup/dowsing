exception Error of String.t
let error msg = raise @@ Error msg

let env = Type.Env.make Query

open Cmdliner

let conv_type =
  let parse_type str =
    try
      Ok (Type.of_string env str)
    with Syntaxerr.Error _ ->
      Error (`Msg "syntax error")
  in
  Arg.conv (parse_type, Type.pp)

let conv_meas_kind =
  let parse_meas_kind str =
    try
      Ok (Measure.Kind.of_string str)
    with Invalid_argument _ ->
      Error (`Msg "illegal measure")
  in
  Arg.conv (parse_meas_kind, Measure.Kind.pp)

type copts = {
  debug : Bool.t ;
}

let copts =
  let docs = Manpage.s_common_options in
  let debug =
    let doc = "Enable debug mode." in
    Arg.(value & flag & info [ "debug" ] ~docs ~doc)
  in
  let copts debug =
    Logs.set_level @@ Some (if debug then Logs.Debug else Logs.Info) ;
    { debug }
  in
  Term.(const copts $ debug)
