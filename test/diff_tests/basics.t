Some initial basic tests.

  $ dowsindex search index.db "int -> int -> int"
  CCShimsInt_.compare: int * int -> int
  Containers.Hashtbl.hash: 'a -> int
  Containers.Hashtbl.seeded_hash: 'a * int -> int
  ContainersLabels.Hashtbl.hash: 'a -> int
  ContainersLabels.Hashtbl.seeded_hash: 'a * int -> int
  CCFun.const: 'a * 'b -> 'a
  CCShimsFun_.const: 'a * 'b -> 'a
  Containers.Fun.const: 'a * 'b -> 'a
  ContainersLabels.Fun.const: 'a * 'b -> 'a

  $ dowsindex search index.db "int -> int -> int -> int"
  Containers.Hashtbl.hash: 'a -> int
  Containers.Hashtbl.hash_param: 'a * int * int -> int
  Containers.Hashtbl.seeded_hash: 'a * int -> int
  ContainersLabels.Hashtbl.hash: 'a -> int
  ContainersLabels.Hashtbl.hash_param: 'a * int * int -> int
  ContainersLabels.Hashtbl.seeded_hash: 'a * int -> int
  CCFun.const: 'a * 'b -> 'a
  CCShimsFun_.const: 'a * 'b -> 'a
  Containers.Fun.const: 'a * 'b -> 'a
  ContainersLabels.Fun.const: 'a * 'b -> 'a
