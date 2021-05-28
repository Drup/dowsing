open Cmdliner

let cmds = [
  CmdUnify.cmd ;
  CmdSave.cmd ;
  CmdStats.cmd ;
  CmdSearch.cmd ;
]

let main_cmd =
  let doc = "search OCaml functions using types" in
  Term.(ret (const (`Error (true, "no command")))),
  Term.(info "dowsindex" ~exits:default_exits ~sdocs:Manpage.s_common_options ~doc)

let () =
  Logs.(set_reporter @@ format_reporter ()) ;
  Term.(exit @@ eval_choice main_cmd cmds)
