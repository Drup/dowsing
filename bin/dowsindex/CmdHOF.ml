open CommonOpts

type data = {
  id : int * int;
  t1 : Type.t;
  t2 : Type.t;
  unif_time : float;
  skipable : bool;
}

let pp_data fmt data =
  Format.fprintf fmt "%i/%i: %b %f %a %a" (fst data.id) (snd data.id)
    data.skipable data.unif_time Type.pp data.t1 Type.pp data.t2

let pp_data_csv fmt data =
  Format.fprintf fmt "%i/%i,%b,%f,%a,%a" (fst data.id) (snd data.id)
    data.skipable data.unif_time Type.pp data.t1 Type.pp data.t2

let cmp data1 data2 = CCFloat.compare data1.unif_time data2.unif_time
let features = Db.Feature.all

let check_features t1 t2 =
  not
  @@ List.for_all
       (fun (module F : Db.Feature.S) ->
         let f1 = F.compute t1 and f2 = F.compute t2 in
         F.compatible ~query:f1 ~data:f2)
       features

let full types k =
  let n_types = List.length types in
  let timer = Timer.make () in
  let rec all_pairs i l =
    match l with
    | [] -> ()
    | t1 :: t ->
        Format.printf "@[<h>%i/%i: %a@]@." i n_types Type.pp t1;
        List.iteri
          (fun j t2 ->
            let env = Type.Env.make () in
            Timer.start timer;
            (try ignore @@ Acic.unify env t1 t2
             with e ->
               Format.printf "\"%a\" \"%a\"@." Type.pp t1 Type.pp t2;
               raise e);
            Timer.stop timer;
            let unif_time = Timer.get timer in
            k
              {
                id = (i, j);
                t1;
                t2;
                unif_time;
                skipable = check_features t1 t2;
              })
          l;
        Gc.compact ();
        (* Gc.print_stat stdout; *)
        all_pairs (i + 1) t
  in
  all_pairs 1 types

let one types t1 k =
  let n_types = List.length types in
  let timer = Timer.make () in
  Format.printf "@[<h>Test against: %a@]@." Type.pp t1;
  List.iteri
    (fun j t2 ->
      Format.printf "@[<h>%i/%i: %a@]@." j n_types Type.pp t2;
      let env = Type.Env.make () in
      Timer.start timer;
      (try ignore @@ Acic.unify env t1 t2
       with e ->
         Format.printf "\"%a\" \"%a\"@." Type.pp t1 Type.pp t2;
         raise e);
      Timer.stop timer;
      let unif_time = Timer.get timer in
      k { id = (-1, j); t1; t2; unif_time; skipable = check_features t1 t2 })
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

let main csv size idx_file ty =
  let types =
    let db =
      try Db.load idx_file
      with Sys_error _ ->
        error @@ Fmt.str "cannot open index file `%a'" Fpath.pp idx_file
    in
    Db.iter db |> Iter.map snd |> Iter.to_list |> List.sort_uniq Type.compare
  in
  let all_unifs =
    match ty with None -> full types | Some ty -> one types ty
  in
  let output, close =
    match csv with
    | None -> ((fun _ -> ()), fun () -> ())
    | Some csv ->
        let oc = open_out csv in
        let fmt = Format.formatter_of_out_channel oc in
        Format.pp_set_margin fmt max_int;
        ( (fun data -> Format.fprintf fmt "@[<h>%a@]@." pp_data_csv data),
          fun () -> close_out oc )
  in
  let hof =
    Iter.fold
      (fun acc data ->
        output data;
        CCList.tl (CCList.sorted_insert ~cmp data acc))
      (List.init size (fun _ ->
           {
             id = (-2, -2);
             unif_time = 0.;
             t1 = Type.dummy;
             t2 = Type.dummy;
             skipable = true;
           }))
      all_unifs
  in
  close ();
  Format.printf "@[<v>%a@]@."
    (CCList.pp ~pp_start:(CCFormat.return "@[<h>")
       ~pp_sep:(CCFormat.return "@]@ @[<h>")
       ~pp_stop:(CCFormat.return "@]") pp_data)
    hof;
  print_stat ()

let main csv size idx_file ty =
  Format.set_margin max_int;
  try Ok (main csv size idx_file ty) with Error msg -> Error (`Msg msg)

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

let csv =
  let doc = "Export data to a csv file." in
  Arg.(value & opt (some string) None & info [ "csv" ] ~doc)

let cmd =
  let doc = "compute hall of fame" in
  Cmd.v
    (info "hof" ~sdocs:Manpage.s_common_options ~doc)
    Term.(term_result (const main $ csv $ size $ idx_file $ ty))
