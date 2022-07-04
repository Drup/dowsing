let env_data = Common.Type.Env.make Data
let env_query = Common.Type.Env.make Query
let idx = Index.make Index.Feature.all

let path =
  match Fpath.of_string Sys.argv.(1) with
  | Ok path -> path
  | Error _ -> invalid_arg "Uncorrect path to library"

let types_str =
  [
    "int -> int -> int";
    "int -> ('a -> 'b -> 'c) * int -> 'a * int";
    "('a -> 'a) -> 'a";
    "int * float -> ('a -> int list) -> int";
    "float";
    "'a -> 'a list -> ('a -> 'b) -> 'a list * 'b list";
  ]

let types = CCList.map (Type.of_string env_query) types_str

let () =
  let module Indexing = Index in
  let module Index = (val idx) in
  let exception Index_not_found of string in
  let idx =
    try Index.load path
    with Sys_error _ ->
      raise
      @@ Index_not_found (Fmt.str "cannot open index file `%a'" Fpath.pp path)
  in
  let search_exhaustive ty =
    let it = Index.find_exhaustive idx env_data ty in
    Iter.max it
  in
  let search_trie ty =
    let it = Index.find_with_trie idx env_data ty in
    Iter.max it
  in
  let search_trie_poset ty =
    let it = Index.find idx env_data ty in
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
