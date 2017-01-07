
let database_of_libindex dirs =
  let open Database in
  let ht = Typexpr.HC.create 17 in
  let index =
    LibIndex.load @@ LibIndex.Misc.unique_subdirs
      (Sequence.to_list dirs)
  in
  let all = LibIndex.all index in
  let f x = match x.LibIndex.kind, x.ty with
    | Value, Some (Osig_value {oval_type}) ->
      let lid = Longident.parse @@ String.concat "." (x.path @ [x.name]) in
      let nf = try
          Imports.of_outcometree ~ht oval_type
        with Not_found as e->
          Format.printf "@[<2>Error while converting value %a:@ %a@]@."
            Typexpr.P.pp lid
            !Oprint.out_type oval_type ;
          raise e
      in
      let (LibIndex.Cmt s | Cmti s | Cmi s) = x.file in
      let info = {Database.
        source = s ;
        lid ;
      }
      in
      Some (nf, info)
    | _ -> None
  in
  NFMap.of_seq (Sequence.filter_map f @@ Sequence.of_list all)

let rectime s t =
  let t' = Unix.gettimeofday () in
  Format.printf "%s: %f@\n@." s (t' -. t) ;
  t'

let save ~file dirs =
  let t = Unix.gettimeofday () in
  Format.printf "@[<v2>Saving directories:@ %a@]@."
    CCFormat.(seq ~sep:"" string) dirs ;
  let map = database_of_libindex dirs in
  let t = rectime "Loading into the map" t in
  Database.save file map ;
  let _t = rectime "Save to file" t in
  ()

let pp_item ty ppf x =
  Format.fprintf ppf "@[<2>%a:@ %a@]"
    Typexpr.P.pp x.Database.lid
    Typexpr.pp ty

let search ~file key =
  let t = Unix.gettimeofday () in
  let map = Database.load file in
  let t = rectime "Loading map from disk" t in

  Format.printf "@[<2>Searching:@ %a@]@." Typexpr.pp key ;

  let decls = Database.NFMap.find map key in
  let _t = rectime "Search in the map" t in

  Format.printf "@[<v>%a@]@."
    (CCFormat.list ~sep:"" (pp_item key)) decls ;

  ()

let file = "foo.db"

let () = match Sys.argv.(1) with
  | "save" -> save ~file Sequence.(drop 2 @@ of_array Sys.argv)
  | "search" -> search ~file (Imports.read (Lexing.from_string Sys.argv.(2)))
  | _ -> failwith "wrong cli"
