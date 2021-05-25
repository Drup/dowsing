let prog_name = "dowsindex"

module Args = struct

  module type Command = sig
      val name : String.t
      val usage : String.t
      val options : (Arg.key * Arg.spec * Arg.doc) List.t
      val anon_fun : String.t -> Unit.t
      val main : Unit.t -> Unit.t
  end

  let debug = ref false

  let options = [
    "--debug", Arg.Set debug, "\tEnable debug mode" ;
  ]

  module StringHMap = CCHashtbl.Make (CCString)

  let cmds = StringHMap.create 17

  let add_cmd ((module Cmd : Command) as cmd) =
    StringHMap.add cmds Cmd.name cmd

  let parse () =
    let error usage msg =
      Common.error () ~msg:(msg ^ ".\n" ^ usage)
    in
    let usage =
      cmds
      |> StringHMap.keys_list
      |> CCString.concat "|"
      |> Fmt.str "usage: %s {%s} <argument>..." prog_name
    in
    let error' = error usage in
    if CCArray.length Sys.argv < 2 then
      error' "unspecified subcommand" ;
    if CCArray.mem Sys.argv.(1) [| "-help" ; "--help" |] then
      Common.exit () ~msg:usage ;
    match StringHMap.get cmds Sys.argv.(1) with
    | None ->
        error' "illegal subcommand"
    | Some (module Cmd) ->
        let name = prog_name ^ " " ^ Cmd.name in
        let options = Arg.align @@ options @ Cmd.options in
        let usage = Fmt.str "usage: %s [<option>...] %s\noptions:" name Cmd.usage in
        Sys.argv.(1) <- "error" ;
        Arg.current := 1 ;
        Arg.parse options Cmd.anon_fun usage ;
        fun () ->
          try Cmd.main ()
          with Arg.Bad msg ->
            let usage = CCString.rtrim @@ Arg.usage_string options usage in
            error usage msg

end

let () = Args.add_cmd (module CmdUnify)
let () = Args.add_cmd (module CmdSave)
let () = Args.add_cmd (module CmdStats)
let () = Args.add_cmd (module CmdSearch)

let () =
  let main = Args.parse () in
  Logs.(set_reporter @@ format_reporter ()) ;
  Logs.set_level @@ Some (if ! Args.debug then Logs.Debug else Logs.Info) ;
  main ()
