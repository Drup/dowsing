let env = Common.Type.Env.make Data
let idx = Index.make Index.Feature.all
let pkgs = try Package.find [ Sys.argv.(1) ] with _ -> Package.find_all ()

let () =
  let module Indexing = Index in
  let module Index = (val idx) in
  let idx = Index.make env in
  let index_trie () = Index.import_package ~with_poset:false idx pkgs in
  let index_trie_poset () = Index.import_package ~with_feat:false idx pkgs in
  let index_trie_poset_feat () =
    Index.import_package ~with_feat:true idx pkgs
  in
  let res =
    Format.printf "Index construction time";
    Benchmark.latencyN (Int64.of_int 10)
      [
        ("Trie", index_trie, ());
        ("Trie + Poset ", index_trie_poset, ());
        ("Trie + Poset with features", index_trie_poset_feat, ());
      ]
  in
  Benchmark.tabulate res
