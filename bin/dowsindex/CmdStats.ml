let timer = Timer.make ()

let pp_table ?(sep = 4) col_names rows =
  let col_cnt = CCArray.length col_names in
  let col_widths = CCArray.map CCString.length col_names in
  let rows = Iter.persistent rows in
  rows |> Iter.iter (fun row ->
    assert (CCArray.length row = col_cnt) ;
    for i = 0 to col_cnt - 1 do
      col_widths.(i) <- max col_widths.(i) @@ CCString.length row.(i)
    done
  ) ;
  for i = 0 to col_cnt - 2 do
    col_widths.(i) <- col_widths.(i) + sep
  done ;
  let print_hline =
    let width = CCArray.fold (+) 0 col_widths in
    let hline = CCString.make width '-' in
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

let name = "stats"
let usage = "<file> <type>"

let filter = ref false

let meas_kind =
  ref Measure.Kind.VarCount
let set_meas_kind str =
  meas_kind := Measure.Kind.of_string str
let meas_kinds_strs =
  Measure.Kind.(CCList.map to_string all)

let options = [
  "--filter", Arg.Set filter, "\tEnable feature filtering" ;
  "--measure", Arg.Symbol (meas_kinds_strs, set_meas_kind), "\tSet type size kind" ;
]

let file = ref None
let ty = ref None

let anon_fun arg =
  if CCOpt.is_none ! file then
    file := Some arg
  else if CCOpt.is_none ! ty then
    ty := Some arg
  else
    raise @@ Arg.Bad "too many arguments"

let main meas_kind filter file str =
  let idx =
    try Index.load file
    with Sys_error _ ->
      Common.error () ~msg:(Fmt.str "cannot open file '%s'." file)
  in
  let env = Type.Env.make Query in
  let ty = Common.type_of_string env str in
  let aux ?stats0 iter_idx =
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
  let stats0 = aux @@ Index.iter idx in
  if filter then begin
    Fmt.pr "@," ;
    ignore @@ aux ~stats0 @@ Index.iter_with idx ty
  end ;
  Fmt.pr "@]@."

let main () =
  if CCOpt.(is_none ! file || is_none ! ty) then
    raise @@ Arg.Bad "too few arguments" ;
  main ! meas_kind ! filter (Option.get ! file) (Option.get ! ty)
