open CommonOpts

let cmp (time1, _) (time2, _) = CCFloat.compare time1 time2

let full size iter_idx =
  let timer = Timer.make () in
  let acc_time = ref 0. in
  let types = Iter.to_list iter_idx |> List.sort_uniq Type.compare in
  let n_types = List.length types in
  let rec all_pairs i acc l =
    match l with
    | [] -> acc
    | t1 :: t ->
        Format.printf "@[<h>%i/%i: %a@]@." i n_types Type.pp t1;
        acc_time := 0.;
        let acc =
          List.fold_left
            (fun acc t2 ->
              let env = Type.Env.make () in
              Timer.start timer;
              (try ignore @@ Acic.unify env t1 t2
               with e ->
                 Format.printf "\"%a\" \"%a\"@." Type.pp t1 Type.pp t2;
                 raise e);
              Timer.stop timer;
              let time = Timer.get timer in
              acc_time := !acc_time +. time;
              (* if time > 10. then Format.printf "@[<h>Big time: %a@]@." Type.pp t2; *)
              CCList.tl (CCList.sorted_insert ~cmp (time, (t1, t2)) acc))
            acc l
        in
        Gc.full_major ();
        Gc.print_stat stdout;
        all_pairs (i + 1) acc t
  in
  all_pairs 1 (List.init size (fun _ -> (0., (Type.dummy, Type.dummy)))) types

let one size iter_idx t1 =
  let timer = Timer.make () in
  let types = Iter.to_list iter_idx |> List.sort_uniq Type.compare in
  let n_types = List.length types in
  Format.printf "@[<h>Test against: %a@]@." Type.pp t1;
  CCList.foldi
    (fun acc i t2 ->
      Format.printf "@[<h>%i/%i: %a@]@." i n_types Type.pp t2;
      let env = Type.Env.make () in
      Timer.start timer;
      (try ignore @@ Acic.unify env t1 t2
       with e ->
         Format.printf "\"%a\" \"%a\"@." Type.pp t1 Type.pp t2;
         raise e);
      Timer.stop timer;
      let time = Timer.get timer in
      CCList.tl (CCList.sorted_insert ~cmp (time, (t1, t2)) acc))
    (List.init size (fun _ -> (0., (Type.dummy, Type.dummy))))
    types

let print_stat () =
  let stats =
    [|
      Tracing.get_nb_ac (); Tracing.get_nb_arrow (); Tracing.get_nb_timeout ();
    |]
  in
  Fmt.pr "@.Stats@.";
  Format.printf "AC\tArrow\tTimeout@.";
  Format.printf "@[%a@]@."
    (CCArray.pp ~pp_sep:(CCFormat.return "\t") CCInt.pp)
    stats;
  Fmt.pr "@."

let main size idx_file ty =
  let iter_idx =
    let db =
      try Db.load idx_file
      with Sys_error _ ->
        error @@ Fmt.str "cannot open index file `%a'" Fpath.pp idx_file
    in
    Db.iter db |> Iter.map snd
  in
  let hof =
    match ty with None -> full size iter_idx | Some ty -> one size iter_idx ty
  in
  Format.printf "@[<v>%a@]@."
    (CCList.pp ~pp_start:(CCFormat.return "@[<h>")
       ~pp_sep:(CCFormat.return "@]@ @[<h>")
       ~pp_stop:(CCFormat.return "@]")
       (CCPair.pp CCFloat.pp (CCPair.pp Type.pp Type.pp)))
    hof;
  print_stat ()

let main size idx_file ty =
  Format.set_margin max_int;
  try Ok (main size idx_file ty) with Error msg -> Error (`Msg msg)

open Cmdliner
open Cmd

let size =
  let docv = "size" in
  let doc = "Number of function in the hall of fame." in
  Arg.(value & opt int 10 & info [ "size" ] ~docv ~doc)

let idx_file =
  let docv = "file" in
  let doc = "Set index file." in
  Arg.(value & opt Convs.file Paths.idx_file & info [ "index" ] ~docv ~doc)

let ty =
  let docv = "TYPE" in
  Arg.(value & pos 0 (some @@ Convs.typ env) None & info [] ~docv)

let cmd =
  let doc = "compute hall of fame" in
  Cmd.v
    (info "hof" ~sdocs:Manpage.s_common_options ~doc)
    Term.(term_result (const main $ size $ idx_file $ ty))
