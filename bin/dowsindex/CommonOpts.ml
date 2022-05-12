exception Error of String.t

let error msg = raise @@ Error msg
let env_query = Type.Env.make Query
let env_data = Type.Env.make Data

open Cmdliner

type copts = { debug : Bool.t; idx : (module Index.S) }

let copts =
  let docs = Manpage.s_common_options in
  let debug =
    let doc = "Enable debug mode." in
    Arg.(value & flag & info [ "debug" ] ~docs ~doc)
  in
  let feats =
    let docv = "features" in
    let doc =
      Fmt.str "Set used features: %s." (Arg.doc_alts Index.Feature.all_names)
    in
    Arg.(
      value
      & opt Convs.feats Index.Feature.all
      & info [ "features" ] ~docs ~docv ~doc)
  in
  let copts debug feats =
    Logs.set_level @@ Some (if debug then Logs.Debug else Logs.Info);
    let idx = Index.make feats in
    { debug; idx }
  in
  Term.(const copts $ debug $ feats)
