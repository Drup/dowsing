Some initial basic tests.

  $ dowsindex search index.db "int -> int -> int"
  int * int -> int:
    Containers.max
    Containers.min
    CCShimsInt_.add
    CCShimsInt_.div
    CCShimsInt_.mul
    CCShimsInt_.rem
    CCShimsInt_.sub
    CCMonomorphic.max
    CCMonomorphic.min
    CCShimsInt_.logor
    CCShimsInt_.logand
    CCShimsInt_.logxor
    Containers.compare
    CCShimsInt_.compare
    ContainersLabels.max
    ContainersLabels.min
    CCMonomorphic.compare
    CCShimsInt_.shift_left
    CCShimsInt_.shift_right
    ContainersLabels.compare
    CCShimsInt_.shift_right_logical
  'a -> int:
    Containers.Hashtbl.hash
    ContainersLabels.Hashtbl.hash
  'a * int -> int:
    Containers.Hashtbl.seeded_hash
    ContainersLabels.Hashtbl.seeded_hash
  'a * 'b -> 'a:
    CCFun.const
    CCShimsFun_.const

  $ dowsindex search index.db "int -> int -> int -> int"
  'a -> int:
    Containers.Hashtbl.hash
    ContainersLabels.Hashtbl.hash
  'a * int -> int:
    Containers.Hashtbl.seeded_hash
    ContainersLabels.Hashtbl.seeded_hash
  'a * int * int -> int:
    Containers.Hashtbl.hash_param
    ContainersLabels.Hashtbl.hash_param
  'a * 'b -> 'a:
    CCFun.const
    CCShimsFun_.const

  $ dowsindex search index.db "int -> int -> 'a"
  'a -> 'a:
    CCFun.id
    CCShimsFun_.id
    CCFun.opaque_identity
  int * int -> bool:
    CCOrd.equiv
    Containers.(<)
    Containers.(=)
    Containers.(>)
    Containers.(<=)
    Containers.(<>)
    Containers.(>=)
    CCMonomorphic.(<)
    CCMonomorphic.(=)
    CCMonomorphic.(>)
    CCShimsInt_.equal
    CCMonomorphic.(<=)
    CCMonomorphic.(<>)
    CCMonomorphic.(>=)
    ContainersLabels.(<)
    ContainersLabels.(=)
    ContainersLabels.(>)
    ContainersLabels.(<=)
    ContainersLabels.(<>)
    ContainersLabels.(>=)
  'a * int -> int:
    Containers.Hashtbl.seeded_hash
    ContainersLabels.Hashtbl.seeded_hash
  int * int -> int:
    Containers.max
    Containers.min
    CCShimsInt_.add
    CCShimsInt_.div
    CCShimsInt_.mul
    CCShimsInt_.rem
    CCShimsInt_.sub
    CCMonomorphic.max
    CCMonomorphic.min
    CCShimsInt_.logor
    CCShimsInt_.logand
    CCShimsInt_.logxor
    Containers.compare
    CCShimsInt_.compare
    ContainersLabels.max
    ContainersLabels.min
    CCMonomorphic.compare
    CCShimsInt_.shift_left
    CCShimsInt_.shift_right
    ContainersLabels.compare
    CCShimsInt_.shift_right_logical
  int * int -> t random_gen:
    CCInt.random_range
  int * int -> int t:
    CCSeq.(--)
    CCList.(--)
    CCSeq.(--^)
    CCSeq.range
    CCArray.(--)
    CCList.(--^)
    CCList.range
    CCArray.(--^)
    CCList.range'
    CCSeq.Infix.(--)
    CCList.Infix.(--)
    CCListLabels.(--)
    CCSeq.Infix.(--^)
    CCArray.Infix.(--)
    CCArrayLabels.(--)
    CCList.Infix.(--^)
    CCListLabels.(--^)
    CCListLabels.range
    CCRandom.int_range
    CCArray.Infix.(--^)
    CCArrayLabels.(--^)
    CCListLabels.range'
    CCListLabels.Infix.(--)
    CCArrayLabels.Infix.(--)
    CCListLabels.Infix.(--^)
    CCArrayLabels.Infix.(--^)
  int * int -> int list option t:
    CCRandom.split_list
  int * int -> ():
    CCFormat.print_break
    CCFormat.print_tbreak
    CCFormat.set_geometry
    CCFormat.safe_set_geometry
  'a * 'b -> 'a:
    CCFun.const
    CCShimsFun_.const
