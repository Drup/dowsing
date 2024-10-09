open Containers
open Bechamel
open Toolkit

let test0 = Test.make ~name:"gettimeofday" (Staged.stage Unix.gettimeofday)
let test1 = Test.make ~name:"time" (Staged.stage Unix.time)
let test = Test.make_grouped ~name:"get time" ~fmt:"%s %s" [ test0; test1 ]

(* From our test, we can start to benchmark it!

   A benchmark is a /run/ of your test multiple times. From results given by
   [Benchmark.all], an analyse is needed to infer measures of one call of your
   test.

   [Bechamel] asks 3 things:
   - what you want to record (see [instances])
   - how you want to analyse (see [ols])
   - how you want to benchmark your test (see [cfg])

   The core of [Bechamel] (see [Bechamel.Toolkit]) has some possible measures
   such as the [monotonic-clock] to see time performances.

   The analyse can be OLS (Ordinary Least Square) or RANSAC. In this example, we
   use only one.

   Finally, to launch the benchmark, we need some others details such as:
   - should we stabilise the GC?
   - how many /run/ you want
   - the maximum of time allowed by the benchmark
   - etc.

   [raw_results] is what the benchmark produced. [results] is what the analyse
   can infer. The first one is used to show graphs or to let the user (with
   [Measurement_raw]) to infer something else than what [ols] did. The second is
   mostly what you want: a synthesis of /samples/. *)

let benchmark test =
  let ols =
    Analyze.ols ~bootstrap:0 ~r_square:true ~predictors:Measure.[| run |]
  in
  let instances = Instance.[ monotonic_clock ] in
  let cfg = Benchmark.cfg ~quota:(Time.second 3.) () in
  let raw_results = Benchmark.all cfg instances test in
  let results =
    List.map (fun instance -> Analyze.all ols instance raw_results) instances
  in
  let results = Analyze.merge ols instances results in
  (results, raw_results)

let img (window, results) =
  Bechamel_notty.Multiple.image_of_ols_results ~rect:window
    ~predictor:Measure.run results

let nothing _ = Ok ()

type print_type = Json | Tui

let bench print_type =
  let results = benchmark test in
  match print_type with
  | Json ->
      let open Bechamel_js in
      emit ~dst:(Channel stdout) nothing ~compare:String.compare
        ~x_label:Measure.run
        ~y_label:(Measure.label Instance.monotonic_clock)
        results
      |> Rresult.R.failwith_error_msg
  | Tui ->
      let open Notty_unix in
      List.iter
        (fun v -> Bechamel_notty.Unit.add v (Measure.unit v))
        Instance.[ monotonic_clock ];
      let window =
        match winsize Unix.stdout with
        | Some (w, h) -> { Bechamel_notty.w; h }
        | None -> { Bechamel_notty.w = 80; h = 1 }
      in
      img (window, fst results) |> eol |> output_image

open Cmdliner

let print_type =
  let json =
    let doc = "Output the results as a json" in
    (Json, Arg.info [ "j"; "json" ] ~doc)
  in
  let tui =
    let doc = "Pretty print the results" in
    (Tui, Arg.info [ "t"; "tui" ] ~doc)
  in
  Arg.(value & vflag Tui [ json; tui ])

let cmd =
  let doc = "Benchmark Dowsing" in
  let man =
    [ `S Manpage.s_description; `P "$(tname) runs the benchmarks of Dowsing" ]
  in
  let info = Cmd.info "bench" ~version:"%%VERSION%%" ~doc ~man in
  Cmd.v info Term.(const bench $ print_type)

let main () =
  exit
    (let e = Cmd.eval cmd in
     flush_all ();
     e)

let () = main ()
