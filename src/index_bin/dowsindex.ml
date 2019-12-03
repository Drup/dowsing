(** Interface with ocp-index *)
module Idx = struct

  let path path name =
    let rec aux l = match l with
      | [] -> assert false
      | [p] -> Longident.Lident p
      | p :: t -> Longident.Ldot (aux t, p)
    in
    match path with
    | [] -> Longident.Lident name
    | l -> Longident.Ldot (aux (List.rev l), name)

  type data = {
    path : Longident.t ;
    ty : Outcometree.out_type ;
  }

  type t = data Database.t

  let database dirs =
    let ht = Typexpr.HC.create 17 in
    let index =
      LibIndex.load @@ LibIndex.Misc.unique_subdirs
        (Sequence.to_list dirs)
    in
    let all = LibIndex.all index in
    let f i = match i.LibIndex.kind, i.ty with
      | Value, Some (Osig_value {oval_type}) ->
        let base_lid = path i.path i.name in
        let lid = path i.orig_path i.name in
        let ty = try
            Imports.of_outcometree ~ht oval_type
          with Not_found as e->
            Format.printf "@[<2>Error while converting value %a:@ %a@]@."
              Typexpr.P.pp lid
              !Oprint.out_type oval_type ;
            raise e
        in
        let data = { path = base_lid ; ty = oval_type } in
        let insert = {Database.Insert. ty ; lid ; data } in
        Some insert
      | _ -> None
    in
    Database.of_seq (Sequence.filter_map f @@ Sequence.of_list all)

  let pp_data ppf x = Typexpr.P.pp ppf x.path
  let pp =
    let eq path x = Typexpr.P.compare path x.path = 0 in
    let ppdata_main ppf x = !Oprint.out_type ppf x.ty in
    Database.Info.pp eq ppdata_main pp_data

end

let rectime s t =
  let t' = Unix.gettimeofday () in
  Format.printf "%s: %f@\n@." s (t' -. t) ;
  t'

let save ~file dirs =
  let t = Unix.gettimeofday () in
  Format.printf "@[<v2>Saving directories:@ %a@]@."
    CCFormat.(seq ~sep:silent string) dirs ;
  let map = Idx.database dirs in
  let t = rectime "Loading into the map" t in
  Database.save file map ;
  let _t = rectime "Save to file" t in
  ()

let search ~file key =
  let t = Unix.gettimeofday () in
  let map = Database.load file in
  let t = rectime "Loading map from disk" t in
  (* let _map' = Database.ByHead.fuse map in *)
  (* let t = rectime "Fusing database" t in *)


  Format.printf "@[<2>Searching:@ %a@]@." Typexpr.pp key ;

  let decls = Database.find map key in
  let _t = rectime "Search in the map" t in

  Format.printf "%a@." Idx.pp decls ;

  ()

let stat file =
  let t = Unix.gettimeofday () in
  let map = Database.load file in
  let _t = rectime "Loading map from disk" t in
  Format.printf "%a@." Database.ByHead.pp_stat map

let unif s1 s2 =
  let gen = Variables.init 0 in
  let t1 = Imports.read ~gen (Lexing.from_string s1) in
  let t2 = Imports.read ~gen (Lexing.from_string s2) in
  Format.printf "@[<2>t1:@ %a@]@." Typexpr.pp t1 ;
  Format.printf "@[<2>t2:@ %a@]@." Typexpr.pp t2 ;
  let t = Unix.gettimeofday () in
  let env = Unification.Env.make ~gen () in
  let Unification.Done = Unification.insert env [] t1 t2 in
  let _t = rectime "Unification" t in
  Format.printf "%a@.Occur-check: %b@."
    Unification.Env.pp env
    (Unification.occur_check env) ;
  let system = Unification.System.make env.pure_problems in
  Format.printf "@[<2>System: @,%a@]@." Unification.System.pp system ;
  Format.printf "@[<2>Solutions: @,%a@]@."
    (CCFormat.seq ~sep:Fmt.sp Unification.System.DSystem.pp_sol)
    (Unification.System.solutions system)


let file = "foo.db"

let () = Format.set_margin 100

let () = match Sys.argv.(1) with
  | "save" -> save ~file Sequence.(drop 2 @@ of_array Sys.argv)
  | "search" -> search ~file (Imports.read (Lexing.from_string Sys.argv.(2)))
  | "unif" -> unif Sys.argv.(2) Sys.argv.(3)
  | "stat" -> stat file
  | _ -> failwith "wrong cli"
