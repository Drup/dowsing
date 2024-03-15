let env = Common.Type.Env.make ()
module Index = Db.DefaultIndex
let files =
  try List.map Fpath.v @@ CCList.drop 1 @@ Array.to_list @@ Sys.argv
  with _ -> Fmt.failwith "please provide some odocl files"

let () =
  let it =
    Dowsing_odoc.iter ["", files]
    |> Iter.persistent
  in
  let index_trie () = Db.create ~with_poset:false env it in
  let index_trie_poset () = Db.create ~with_poset:true env it in
  Format.printf "Index construction time";
  let res =
    Benchmark.latencyN (Int64.of_int 10)
      [
        ("Trie", index_trie, ());
        ("Trie + Poset ", index_trie_poset, ());
      ]
  in
  Benchmark.tabulate res
