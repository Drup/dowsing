open Containers
open Bechamel
open Toolkit

let make_test idx_file with_feats ty =
  let name =
    Format.sprintf "%s_%a" (if with_feats then "wf" else "nf") Type.pp ty
  in
  let allocate () =
    let env = Type.Env.make () in
    let db =
      try Db.load idx_file
      with Sys_error _ ->
        failwith @@ Fmt.str "cannot open index file `%a'" Fpath.pp idx_file
    in
    let filter = if with_feats then `Default else `None in
    Db.find ~filter db env ty
  in
  let free _ = () in
  Test.make_with_resource ~name Test.multiple ~allocate ~free
    (Staged.stage @@ fun sols -> sols ignore)

let instances =
  Instance.
    [
      monotonic_clock;
      Tracing.Instance.ac_sols;
      Tracing.Instance.arrow_sols;
      Tracing.Instance.timeout;
    ]

let cfg =
  (* Benchmark.cfg ~limit:2000 ~stabilize:true ~quota:(Time.second 0.5) () *)
  Benchmark.cfg ()

let ols = Analyze.ols ~bootstrap:0 ~r_square:true ~predictors:[| Measure.run |]

type result = {
  time : float;
  time_r_square : float;
  ac : int;
  arrow : int;
  timeout : int;
}

type data = { ty : Type.t; feats : result; no_feats : result }

module B = PrintBox

let make_comparaison_matrix name_1 result_1 name_2 result_2 =
  let grid = Array.init 3 (fun _ -> Array.make 3 (B.text "")) in
  grid.(1).(0) <- B.text name_1;
  grid.(2).(0) <- B.text name_2;
  grid.(0).(1) <- B.text name_1;
  grid.(0).(2) <- B.text name_2;
  let ratio12 = result_1.time /. result_2.time in
  let ratio21 = result_2.time /. result_1.time in
  grid.(1).(2) <- B.center_hv @@ B.sprintf "x%.2f" ratio21;
  grid.(2).(1) <- B.center_hv @@ B.sprintf "x%.2f" ratio12;
  B.grid grid |> B.frame

let pp_result fmt result =
  Format.fprintf fmt "@[<v>Time: %.2f ms@;AC sol: %i@;Arrow: %i@;Timeout: %i@]"
    (result.time /. 1_000_000.)
    result.ac result.arrow result.timeout

let pp_data fmt data =
  Format.fprintf fmt
    "@[<v>@{<bold>@[%a@]:@}@;\
     <1 2>@[<v>@{<bold>With features:@}@;\
     <1 2>%a@;\
     @;\
     @{<bold>Without features:@}@;\
     <1 2>%a@;\
     @;\
     %a@]@]"
    Type.pp data.ty pp_result data.feats pp_result data.no_feats
    (PrintBox_text.pp_with ~style:true)
    (make_comparaison_matrix "Feat" data.feats "No feat" data.no_feats)

let get_singleton = function [ e ] -> e | _ -> failwith "Not a singleton"

let get_const result instance =
  int_of_float
    (Measurement_raw.get ~label:(Measure.label instance) result.Benchmark.lr.(0))

let run test =
  let test = get_singleton (Test.elements test) in
  let result = Benchmark.run cfg instances test in
  let time = Analyze.one ols Instance.monotonic_clock result in
  let time_r_square =
    CCOption.get_exn_or "time_r2" (Analyze.OLS.r_square time)
  in
  let time =
    CCOption.get_exn_or "time" (Analyze.OLS.estimates time) |> get_singleton
  in
  let ac = get_const result Tracing.Instance.ac_sols in
  let arrow = get_const result Tracing.Instance.arrow_sols in
  let timeout = get_const result Tracing.Instance.timeout in
  { time; time_r_square; ac; arrow; timeout }

let get_data idx_file ty =
  {
    ty;
    feats = run (make_test idx_file true ty);
    no_feats = run (make_test idx_file false ty);
  }

let stat idx_file save_file tys =
  CCFormat.set_color_default true;
  let res =
    List.map
      (fun ty ->
        let data = get_data idx_file ty in
        Format.printf "@[%a@]@." pp_data data;
        data)
      tys
  in
  match save_file with
  | None -> ()
  | Some file -> CCIO.with_out file (fun cout -> Marshal.to_channel cout res [])

let _grid_diff base result =
  let grid = Array.init 5 (fun _ -> Array.make 3 (B.text "")) in
  grid.(0).(1) <- B.center_hv @@ B.text "Base";
  grid.(0).(2) <- B.center_hv @@ B.text "New";
  List.iteri
    (fun i w -> grid.(i + 1).(0) <- B.text @@ Measure.label w)
    instances;
  grid.(1).(1) <- B.center_hv @@ B.sprintf "%.2f" base.time;
  grid.(1).(2) <- B.center_hv @@ B.sprintf "%.2f" result.time;
  grid.(2).(1) <- B.center_hv @@ B.sprintf "%i" base.ac;
  grid.(2).(2) <- B.center_hv @@ B.sprintf "%i" result.ac;
  grid.(3).(1) <- B.center_hv @@ B.sprintf "%i" base.arrow;
  grid.(3).(2) <- B.center_hv @@ B.sprintf "%i" result.arrow;
  grid.(4).(1) <- B.center_hv @@ B.sprintf "%i" base.timeout;
  grid.(4).(2) <- B.center_hv @@ B.sprintf "%i" result.timeout;
  B.grid grid |> B.frame

let ratio x y =
  let open Float.Infix in
  let r = x /. y in
  if r < 0.1 then
    B.sprintf_with_style
      { B.Style.default with bold = true; bg_color = Some Red }
      "%.2f" r
  else if r < 0.5 then
    B.sprintf_with_style
      { B.Style.default with bold = true; fg_color = Some Red }
      "%.2f" r
  else if r < 1. then
    B.sprintf_with_style
      { B.Style.default with bold = true; fg_color = Some Yellow }
      "%.2f" r
  else if r < 1.5 then
    B.sprintf_with_style
      { B.Style.default with bold = true; fg_color = Some Green }
      "%.2f" r
  else
    B.sprintf_with_style
      { B.Style.default with bold = true; bg_color = Some Green }
      "%.2f" r

let compare base idx_file =
  CCFormat.set_color_default true;
  let base = CCIO.with_in base (fun cin -> Marshal.from_channel cin) in
  let grid =
    Array.init (List.length base + 1) (fun _ -> Array.make 4 (B.text ""))
  in
  grid.(0).(1) <- B.center_hv @@ B.text "Base";
  grid.(0).(2) <- B.center_hv @@ B.text "New";
  grid.(0).(3) <- B.center_hv @@ B.text "Ratio";
  List.iteri
    (fun i res ->
      let ty = res.ty in
      let data = get_data idx_file ty in
      grid.(i + 1).(0) <- B.asprintf "%a" Type.pp ty;
      grid.(i + 1).(1) <-
        B.center_hv @@ B.sprintf "%.2f ms" (res.feats.time /. 1_000_000.);
      grid.(i + 1).(2) <-
        B.center_hv @@ B.sprintf "%.2f ms" (data.feats.time /. 1_000_000.);
      grid.(i + 1).(3) <- B.center_hv @@ ratio res.feats.time data.feats.time)
    base;
  let grid = B.grid grid in
  Format.printf "@[%a@]@." (PrintBox_text.pp_with ~style:true) grid
(* List.iter *)
(*   (fun res -> *)
(*     let ty = res.ty in *)
(*     let data = get_data idx_file ty in *)
(*     Format.printf "@[<v>@{<bold>%a@}@;<1 2>%a@]@." Type.pp ty *)
(*       (PrintBox_text.pp_with ~style:true) *)
(*       (grid_diff res.feats data.feats)) *)
(*   base *)

open Cmdliner

let idx_file =
  let docv = "FILE" in
  let doc = "Set index file." in
  Arg.(
    value & opt Convs.file Utils.Paths.idx_file & info [ "index" ] ~docv ~doc)

let save_file =
  let docv = "SAVE" in
  let doc = "Save result to SAVE." in
  Arg.(value & opt (some string) None & info [ "s"; "save" ] ~docv ~doc)

let tys =
  let docv = "TYPES" in
  Arg.(value & pos_all (Common.Convs.typ (Type.Env.make ())) [] & info [] ~docv)

let stat =
  let doc = "Generate statistics" in
  let info = Cmd.info "stat" ~doc in
  Cmd.v info Term.(const stat $ idx_file $ save_file $ tys)

let base_file =
  let docv = "BASE" in
  let doc = "Load the base to compare to." in
  Arg.(value & pos 0 file "" & info [] ~docv ~doc)

let compare =
  let doc =
    "Compare the curent state with a previous state. The databased used must \
     be the same for the test to be relevant."
  in
  let info = Cmd.info "compare" ~doc in
  Cmd.v info Term.(const compare $ base_file $ idx_file)

let cmds = [ stat; compare ]

let main_cmd, main_info =
  let doc = "Benchmark utils for Dowsing" in
  ( Term.(ret (const (`Error (true, "no command")))),
    Cmd.info "bench" ~sdocs:Manpage.s_common_options ~doc )

let () =
  Logs.(set_reporter @@ format_reporter ());
  exit @@ Cmd.eval @@ Cmd.group ~default:main_cmd main_info cmds
