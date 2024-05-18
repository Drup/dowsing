open Cmdliner
open Stdlib

let cmds = [
  CmdUnify.cmd ;
  CmdSave.cmd ;
  CmdStats.cmd ;
  CmdSearch.cmd ;
  CmdUnifiable.cmd ;
]

let main_cmd, main_info =
  let doc = "search OCaml functions using types" in
  Term.(ret (const (`Error (true, "no command")))),
  Cmd.info "dowsing" ~sdocs:Manpage.s_common_options ~doc

let () =
  exit @@
  Trace_tef.with_setup () (fun () ->
  Logs.(set_reporter @@ format_reporter ()) ;
  Cmd.eval @@ Cmd.group ~default:main_cmd main_info cmds)
