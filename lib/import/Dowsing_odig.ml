(** Import packages and odoc objects using Odig *)

module PkgSet = CCSet.Make(String)

exception Unbound_package of string

let odig args =
  let ic = Unix.open_process_args_in "odig"
      (Array.concat [[|"odig"|]; args; [|"--color=never"|]])
  in
  let l = CCIO.read_lines_l ic in
  l

let odig_odoc_cache () =
  match odig [|"cache";"path"|] with
  | [s] -> CCResult.get_exn @@ Fpath.of_string s
  | _ -> assert false

let odig_cache_dir = odig_odoc_cache ()
let odig_cache_pkg pkgname =
  let dir = Fpath.(odig_cache_dir / "odoc" / pkgname) in
  dir

let odig_pkgs () =
  PkgSet.of_list @@ odig [|"pkg";"--no-pager";"-s"|]

let get_odocl_files dir =
  let odocl = Fpath.(dir / "$(name).odocl") in
  Bos.OS.Path.matches odocl
  |> CCResult.get_or ~default:[]
    
let opam_list_depends pkgs =
  let req_by = String.concat "," pkgs in
  let cli = [|"opam"; "list"; "--safe"; "--normalise";"--short";"--nobuild";
              "--color=never"; "--recursive"; "--required-by="^req_by|] in
  (* Fmt.epr "Run %s@." (String.concat " " @@ Array.to_list cli); *)
  let ic = Unix.open_process_args_in "opam" cli in
  let l = CCIO.read_lines_l ic in
  (* Fmt.epr "Found %s@." (String.concat ", " l); *)
  l

let find_all () = 
  let all_pkgs = odig_pkgs () in
  PkgSet.elements all_pkgs
  |> List.map (fun pkg -> pkg, get_odocl_files @@ odig_cache_pkg pkg)

let find ~dependencies pkg_names =
  let all_pkgs = odig_pkgs () in
  begin
    List.iter (fun pkg ->
        if not @@ PkgSet.mem pkg all_pkgs then
          raise @@ Unbound_package pkg
      ) pkg_names
  end;
  let pkgs =
    if dependencies then
      List.filter (fun pkg -> PkgSet.mem pkg all_pkgs) @@
      opam_list_depends pkg_names
    else
      pkg_names
  in
  pkgs
  |> List.map (fun pkg -> pkg, get_odocl_files @@ odig_cache_pkg pkg)
