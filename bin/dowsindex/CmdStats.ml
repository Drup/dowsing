open Common

let pp_table ?(sep = 4) col_names rows =
  let col_cnt = CCArray.length col_names in
  let col_widths = CCArray.map String.length col_names in
  let rows = Iter.persistent rows in
  rows |> Iter.iter (fun row ->
    assert (CCArray.length row = col_cnt) ;
    for i = 0 to col_cnt - 1 do
      col_widths.(i) <- max col_widths.(i) @@ String.length row.(i)
    done
  ) ;
  for i = 0 to col_cnt - 2 do
    col_widths.(i) <- col_widths.(i) + sep
  done ;
  let print_hline =
    let width = CCArray.fold (+) 0 col_widths in
    let hline = String.make width '-' in
    fun () -> CCFormat.print_string hline
  in
  CCFormat.open_tbox () ;
  print_hline () ;
  CCFormat.printf "@\n" ;
  for i = 0 to col_cnt - 1 do
    CCFormat.set_tab () ;
    CCFormat.printf "%-*s" col_widths.(i) col_names.(i)
  done ;
  CCFormat.print_tab () ;
  print_hline () ;
  rows |> Iter.iter (fun row ->
    for i = 0 to col_cnt - 1 do
      CCFormat.print_tab () ;
      CCFormat.print_string row.(i)
    done
  ) ;
  CCFormat.print_tab () ;
  print_hline () ;
  CCFormat.close_tbox ()

let main _ meas_kind filt idx_file ty pkgs =
  let pkgs =
    if CCList.is_empty pkgs
    then None
    else Some pkgs
  in
  let idx =
    try
      Index.load idx_file
    with Sys_error _ ->
      error @@ Fmt.str "cannot open index file `%a'"
        Fpath.pp idx_file
  in
  let timer = Timer.make () in
  let aux ?stats0 iter_idx =
    let iter_idx =
      try iter_idx ()
      with Not_found -> error "unknown package"
    in
    let tbl = ref Measure.Map.empty in
    iter_idx (fun (ty', _) ->
      Timer.start timer ;
      ignore @@ Unification.unifiable env ty ty' ;
      Timer.stop timer ;
      let time = Timer.get timer in
      let meas = Measure.make meas_kind ty' in
      tbl := ! tbl |> Measure.Map.update meas @@ function
        | None -> Some (time, 1)
        | Some (time', cnt) -> Some (time +. time', cnt + 1)
    ) ;
    let total_time = ref 0. in
    let total_cnt = ref 0 in
    let rows =
      ! tbl
      |> Measure.Map.to_iter
      |> Iter.map (fun (meas, (time, cnt)) ->
        total_time := ! total_time +. time ;
        total_cnt := ! total_cnt + cnt ;
        [|
          Fmt.to_to_string (Measure.pp meas_kind) meas ;
          Fmt.(to_to_string float) @@ time *. 1e3 ;
          Fmt.(to_to_string float) @@ time /. CCFloat.of_int cnt *. 1e6 ;
          CCInt.to_string cnt ;
        |]
      )
    in
    pp_table [| "size" ; "total time (ms)" ; "avg. time (μs)" ; "# unif." |] rows ;
    let total_time = ! total_time in
    let total_cnt = ! total_cnt in
    Fmt.pr "@,total time: %g" total_time ;
    stats0 |> CCOpt.iter (fun (total_time', _) ->
      Fmt.pr " (%g %%)" @@ total_time /. total_time' *. 100.
    ) ;
    Fmt.pr "@,total # unif.: %i" total_cnt ;
    let total_cnt = CCFloat.of_int total_cnt in
    stats0 |> CCOpt.iter (fun (_, total_cnt') ->
      Fmt.pr " (%g %%)" @@ total_cnt /. total_cnt' *. 100.
    ) ;
    total_time, total_cnt
  in
  Fmt.pr "@[<v>" ;
  let stats0 = aux @@ fun () -> Index.iter idx ?pkgs in
  if filt then begin
    Fmt.pr "@," ;
    ignore @@ aux ~stats0 @@ fun () -> Index.iter_with idx ty ?pkgs
  end ;
  Fmt.pr "@]@."

let main copts meas_kind filt idx_file ty pkgs =
  try Ok (main copts meas_kind filt idx_file ty pkgs)
  with Error msg -> Error (`Msg msg)

open Cmdliner

let meas_kind =
  let docv = "measure" in
  let doc =
    Fmt.str "Set type measure: $(docv) must be %s."
      (Arg.doc_alts Measure.Kind.(CCList.map to_string all))
  in
  Arg.(value & opt Conv.meas_kind Measure.Kind.VarCount & info [ "measure" ] ~docv ~doc)

let filt =
  let doc = "Test feature filtering." in
  Arg.(value & flag & info [ "filter" ] ~doc)

let idx_file =
  let docv = "file" in
  let doc = "Set index file." in
  Arg.(value & opt Conv.file Paths.idx_file & info [ "index" ] ~docv ~doc)

let ty =
  let docv = "type" in
  Arg.(required & pos 0 (some Conv.typ) None & info [] ~docv)

let pkgs =
  let docv = "package" in
  Arg.(value & pos_right 0 string [] & info [] ~docv)

let cmd =
  let doc = "compute index statistics" in
  Term.(term_result (const main $ copts $ meas_kind $ filt $ idx_file $ ty $ pkgs)),
  Term.(info "stats" ~exits:default_exits ~sdocs:Manpage.s_common_options ~doc)
