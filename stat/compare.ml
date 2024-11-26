open Containers

let ratio x y =
  let open Float.Infix in
  let r = x /. y in
  if r < 0.5 then
    PrintBox.sprintf_with_style
      { PrintBox.Style.default with bold = true; bg_color = Some Red }
      "%.2f x%.2f" (y /. 1_000_000.) r
  else if r < 0.7 then
    PrintBox.sprintf_with_style
      { PrintBox.Style.default with bold = true; fg_color = Some Red }
      "%.2f x%.2f" (y /. 1_000_000.) r
  else if r < 0.99 then
    PrintBox.sprintf_with_style
      { PrintBox.Style.default with bold = true; fg_color = Some Yellow }
      "%.2f x%.2f" (y /. 1_000_000.) r
  else if r < 1.01 then
    PrintBox.sprintf_with_style PrintBox.Style.default "%.2f x%.2f"
      (y /. 1_000_000.) r
  else if r < 1.5 then
    PrintBox.sprintf_with_style
      { PrintBox.Style.default with bold = true; fg_color = Some Green }
      "%.2f x%.2f" (y /. 1_000_000.) r
  else
    PrintBox.sprintf_with_style
      { PrintBox.Style.default with bold = true; bg_color = Some Green }
      "%.2f x%.2f" (y /. 1_000_000.) r

let get_name file =
  let file = Filename.basename file in
  try Filename.chop_extension file with Invalid_argument _ -> file

let prod_l ls =
  let rec aux = function
    | [] :: _ -> []
    | ls ->
        let hs, rest =
          List.fold_left
            (fun (hs, rest) l -> (List.hd l :: hs, List.tl l :: rest))
            ([], []) ls
        in
        let hs = List.rev hs in
        let rest = List.rev rest in
        hs :: aux rest
  in
  aux ls

let compare results =
  let env = Type.Env.make () in
  let results : (string * Bench.data list) list =
    List.map
      (fun file ->
        let l =
          CCIO.with_in file Marshal.from_channel
          |> List.map (fun (ty, feats, no_feats) ->
                 { Bench.ty = Type.of_string env ty; feats; no_feats })
        in

        (get_name file, l))
      results
  in
  let _, base = List.hd results in
  let grid =
    Array.init
      (List.length base + 1)
      (fun _ -> Array.make (List.length results + 1) (PrintBox.text ""))
  in
  List.iteri
    (fun i (name, _) ->
      grid.(0).(i + 1) <- PrintBox.center_hv @@ PrintBox.text name)
    results;
  let table = List.map snd results |> prod_l |> CCList.combine base in
  List.iteri
    (fun r (res, row) ->
      grid.(r + 1).(0) <- PrintBox.asprintf "%a" Type.pp res.Bench.ty;
      List.iteri
        (fun c { Bench.feats; _ } ->
          grid.(r + 1).(c + 1) <- ratio res.Bench.feats.time feats.time)
        row)
    table;
  let grid = PrintBox.grid grid in
  Format.printf "@[%a@]@." (PrintBox_text.pp_with ~style:true) grid

open Cmdliner

let data =
  let docv = "DATAS" in
  Arg.(value & pos_all file [] & info [] ~docv)

let compare =
  let doc =
    "Compare the curent state with a previous state. The databased used must \
     be the same for the test to be relevant."
  in
  let info = Cmd.info "compare" ~doc in
  Cmd.v info Term.(const compare $ data)
