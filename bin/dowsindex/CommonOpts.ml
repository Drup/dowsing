exception Error of String.t

let error msg = raise @@ Error msg
let env = Type.Env.make ()

open Cmdliner

type copts = { debug : Bool.t; }

let copts =
  let docs = Manpage.s_common_options in
  let debug =
    let doc = "Enable debug mode." in
    Arg.(value & flag & info [ "debug" ] ~docs ~doc)
  in
  (* let feats = *)
  (*   let docv = "features" in *)
  (*   let doc = *)
  (*     Fmt.str "Set used features: %s." (Arg.doc_alts Db.Feature.all_names) *)
  (*   in *)
  (*   Arg.( *)
  (*     value *)
  (*     & opt Convs.feats Db.Feature.all *)
  (*     & info [ "features" ] ~docs ~docv ~doc) *)
  (* in *)
  let copts debug =
    Logs.set_level @@ Some (if debug then Logs.Debug else Logs.Error);
    (* let idx = Db.make feats in *)
    { debug; }
  in
  Term.(const copts $ debug)
