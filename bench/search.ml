let env = Common.Type.Env.make ()
module Index = Db.DefaultIndex

let path =
  match Fpath.of_string Sys.argv.(1) with
  | Ok path -> path
  | Error _ -> invalid_arg "Incorrect path to library"

let types_str =
  [
    "int -> int -> int";
    "int -> ('a -> 'b -> 'c) * int -> 'a * int";
    "('a -> 'a) -> 'a";
    "int * float -> ('a -> int list) -> int";
    "float";
    "'a -> 'a list -> ('a -> 'b) -> 'a list * 'b list";
    "'a list -> _ -> unit";
  ]

let types =
  let of_str s =
    Type.of_string env s |> Type.freeze_variables env
  in
  CCList.map of_str types_str

let () =
  let exception Index_not_found of string in
  let idx =
    try Db.load path
    with Sys_error _ ->
      raise
      @@ Index_not_found (Fmt.str "cannot open index file `%a'" Fpath.pp path)
  in
  let search_exhaustive ty =
    let it = Db.find ~filter:`None idx env ty in
    Iter.max it
  in
  let search_trie ty =
    let it = Db.find ~filter:`OnlyTrie idx env ty in
    Iter.max it
  in
  let search_trie_poset ty =
    let it = Db.find ~filter:`Default idx env ty in
    Iter.max it
  in
  let make_res ty =
    Format.printf "Search time for query type : %a @." Type.pp ty;
    let res =
      Benchmark.throughputN (*latencyN (Int64.of_int 1000)*) 10
        [
          ("Exhaustive search", search_exhaustive, ty);
          ("Search with Trie", search_trie, ty);
          ("Search with Trie + Poset", search_trie_poset, ty);
        ]
    in
    Benchmark.tabulate res
  in
  CCList.iter make_res types
