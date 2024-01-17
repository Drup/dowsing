open Cmdliner
open Stdlib

let cmds = [
  CmdUnify.cmd ;
  CmdSave.cmd ;
  CmdStats.cmd ;
  CmdSearch.cmd ;
]

let main_cmd, main_info =
  let doc = "search OCaml functions using types" in
  Term.(ret (const (`Error (true, "no command")))),
  Cmd.info "dowsindex" ~sdocs:Manpage.s_common_options ~doc

let () =
  Trace_tef.with_setup () (fun () ->
  Logs.(set_reporter @@ format_reporter ()) ;
  exit @@ Cmd.eval @@ Cmd.group ~default:main_cmd main_info cmds)
