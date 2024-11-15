open Cmdliner

let cmds = [ Bench.stat; Compare.compare ]

let main_cmd, main_info =
  let doc = "Benchmark utils for Dowsing" in
  ( Term.(ret (const (`Error (true, "no command")))),
    Cmd.info "stat/main.exe" ~sdocs:Manpage.s_common_options ~doc )

let () =
  Logs.(set_reporter @@ format_reporter ());
  exit @@ Cmd.eval @@ Cmd.group ~default:main_cmd main_info cmds
