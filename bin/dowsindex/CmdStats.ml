open CommonOpts

type opts = {
  copts : copts;
  meas_kind : Measure.Kind.t;
  with_feats : Bool.t;
  idx_file : Fpath.t;
  ty : Type.t;
  pkgs : String.t List.t;
}

let print_table ?(sep = 4) col_names rows =
  let col_cnt = CCArray.length col_names in
  let col_widths = CCArray.map String.length col_names in
  let rows = Iter.persistent rows in
  rows
  |> Iter.iter (fun row ->
         assert (CCArray.length row = col_cnt);
         for i = 0 to col_cnt - 1 do
           col_widths.(i) <- max col_widths.(i) @@ String.length row.(i)
         done);
  for i = 0 to col_cnt - 2 do
    col_widths.(i) <- col_widths.(i) + sep
  done;
  let print_hline =
    let width = CCArray.fold ( + ) 0 col_widths in
    let hline = String.make width '-' in
    fun () -> CCFormat.print_string hline
  in
  CCFormat.open_tbox ();
  print_hline ();
  CCFormat.printf "@\n";
  for i = 0 to col_cnt - 1 do
    CCFormat.set_tab ();
    CCFormat.printf "%-*s" col_widths.(i) col_names.(i)
  done;
  CCFormat.print_tab ();
  print_hline ();
  rows
  |> Iter.iter (fun row ->
         for i = 0 to col_cnt - 1 do
           CCFormat.print_tab ();
           CCFormat.print_string row.(i)
         done);
  CCFormat.print_tab ();
  print_hline ();
  CCFormat.close_tbox ()

let aux opts ?stats0 iter_idx =
  let iter_idx =
    iter_idx ()
  in
  let timer = Timer.make () in
  let tbl = ref Measure.Map.empty in
  iter_idx (fun ty ->
      Timer.start timer;
      ignore @@ Acic.unify env opts.ty ty;
      Timer.stop timer;
      let time = Timer.get timer in
      let meas = Measure.make opts.meas_kind ty in
      tbl :=
        !tbl
        |> Measure.Map.update meas @@ function
           | None -> Some (time, 1)
           | Some (time', cnt) -> Some (time +. time', cnt + 1));
  let total_time = ref 0. in
  let total_cnt = ref 0 in
  let rows =
    !tbl |> Measure.Map.to_iter
    |> Iter.map (fun (meas, (time, cnt)) ->
           total_time := !total_time +. time;
           total_cnt := !total_cnt + cnt;
           [|
             Fmt.to_to_string (Measure.pp opts.meas_kind) meas;
             Fmt.(to_to_string float) @@ (time *. 1e3);
             Fmt.(to_to_string float) @@ (time /. CCFloat.of_int cnt *. 1e6);
             CCInt.to_string cnt;
           |])
  in
  print_table
    [| "measure"; "total time (ms)"; "avg. time (μs)"; "# unif." |]
    rows;
  let total_time = !total_time in
  let total_cnt = !total_cnt in
  Fmt.pr "@,total time (s): %g" total_time;
  stats0
  |> CCOption.iter (fun (total_time', _) ->
         Fmt.pr " (%g %%)" @@ (total_time /. total_time' *. 100.));
  Fmt.pr "@,total # unif.: %i" total_cnt;
  let total_cnt = CCFloat.of_int total_cnt in
  stats0
  |> CCOption.iter (fun (_, total_cnt') ->
         Fmt.pr " (%g %%)" @@ (total_cnt /. total_cnt' *. 100.));
  (total_time, total_cnt)

let print_stat () =
  let stats =
    Array.map (Format.sprintf "%i")
    [|
      Tracing.get_nb_ac ();
      Tracing.get_nb_arrow ();
        Tracing.get_nb_timeout ();
    |]
  in
  Fmt.pr "@.Stats@.";
  print_table [|"AC sol"; "Arrow sol"; "Timeout"|]
  (Iter.singleton stats);
  Fmt.pr "@."

let aux opts iter_idx iter_idx_filt =
  let aux = aux opts in
  Fmt.pr "@[<v>";
  let stats0 = aux iter_idx in
  if opts.with_feats then (
    Fmt.pr "@,";
    ignore @@ aux ~stats0 iter_idx_filt);
  Fmt.pr "@]@.";
  print_stat ()

let main opts =
  let pkgs = if CCList.is_empty opts.pkgs then None else Some opts.pkgs in
  let iter_idx, iter_idx_filt =
    let db =
      try Db.load opts.idx_file
      with Sys_error _ ->
        error @@ Fmt.str "cannot open index file `%a'" Fpath.pp opts.idx_file
    in
    ( (fun () -> Db.iter db ?pkgs |> Iter.map snd),
      fun () -> Db.iter_compatible db opts.ty ?pkgs |> Iter.map snd )
  in
  aux opts iter_idx iter_idx_filt

let main copts meas_kind with_feats idx_file ty pkgs =
  try Ok (main { copts; meas_kind; with_feats; idx_file; ty; pkgs })
  with Error msg -> Error (`Msg msg)

open Cmdliner
open Cmd

let meas_kind =
  let docv = "measure" in
  let doc =
    Fmt.str "Set type measure: %s." (Arg.doc_alts Measure.Kind.all_names)
  in
  Arg.(
    value
    & opt Convs.meas_kind Measure.Kind.HeadKind
    & info [ "measure" ] ~docv ~doc)

let with_feats =
  let doc = "Test features." in
  Arg.(value & flag & info [ "with-features" ] ~doc)

let no_idx =
  let doc =
    "Do not use or compute index: retrieve functions directly from OPAM."
  in
  Arg.(value & flag & info [ "no-index" ] ~doc)

let idx_file =
  let docv = "file" in
  let doc = "Set index file." in
  Arg.(value & opt Convs.file Paths.idx_file & info [ "index" ] ~docv ~doc)

let ty =
  let docv = "type" in
  Arg.(
    required
    & pos ~rev:true 0 (some @@ Convs.scheme env) None
    & info [] ~docv)

let pkgs =
  let docv = "package" in
  Arg.(value & pos_left ~rev:true 0 string [] & info [] ~docv)

let cmd =
  let doc = "compute index statistics" in
  Cmd.v
    (info "stats" ~sdocs:Manpage.s_common_options ~doc)
    Term.(
      term_result
        (const main $ copts $ meas_kind $ with_feats $ idx_file $ ty
       $ pkgs))
