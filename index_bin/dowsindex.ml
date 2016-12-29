
module NFMap = CCMultiMap.Make(Normal_form)(struct
    type t = LibIndex.info
    let compare = compare
  end)

let rectime s t =
  let t' = Unix.gettimeofday () in
  Format.printf "%s: %f@\n@." s (t' -. t) ;
  t'

let main dir _search =
  let t = Unix.gettimeofday () in
  let index =
    LibIndex.load @@ LibIndex.Misc.unique_subdirs [dir]
  in
  let all = LibIndex.all index in
  let f x =
    let open LibIndex in match x.kind, x.ty with
    | Value, Some (Osig_value {oval_type}) ->
      Some (Normal_form.of_outcometree oval_type, x)
    | _ -> None
  in
  let t = rectime "Ocp-lib Init" t in
  let map =
    NFMap.of_seq (Sequence.filter_map f @@ Sequence.of_list all)
  in
  let t = rectime "Loading into the map" t in

  let pp_item ty ppf x =
    Format.fprintf ppf "@[<2>%s.%s:@ %a@]"
      (String.concat "." x.LibIndex.path) x.LibIndex.name
      Normal_form.pp ty
  in
  (* Format.printf *)
  (*   "@[<v2>Env:@ %a@]@." *)
  (*   (CCFormat.seq ~sep:"" pp_item) (NFMap.to_seq map) ; *)

  let key =
    Normal_form.(Ty.Arrow (NSet.singleton (Var 0), Var 0))
  in

  let decls = NFMap.find map key in


  let _t = rectime "Search in the map" t in

  Format.printf "@[<v>%a@]@."
    (CCFormat.list (pp_item key)) decls ;

  ()

let () = main Sys.argv.(1) ()
