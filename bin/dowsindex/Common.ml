exception Error of String.t
let error msg = raise @@ Error msg

let env = Type.Env.make Query

open Cmdliner

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
