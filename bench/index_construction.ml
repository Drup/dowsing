let env = Common.Type.Env.make Data
module Index = Db.DefaultIndex
let pkgs = try Dowsing_findlib.find [ Sys.argv.(1) ] with _ -> Dowsing_findlib.find_all ()

let () =
  let it =
    pkgs
    |> Iter.of_list
    |> Iter.flat_map (fun (pkg, dir) -> Dowsing_libindex.iter pkg dir)
    |> Iter.persistent
  in
  let index_trie () = Db.create ~with_poset:false env it in
  let index_trie_poset () = Db.create ~with_feat:false env it in
  let index_trie_poset_feat () = Db.create ~with_feat:true env it in
  Format.printf "Index construction time";
  let res =
    Benchmark.latencyN (Int64.of_int 10)
      [
        ("Trie", index_trie, ());
        ("Trie + Poset ", index_trie_poset, ());
        ("Trie + Poset with features", index_trie_poset_feat, ());
      ]
  in
  Benchmark.tabulate res
