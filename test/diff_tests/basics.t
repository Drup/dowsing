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
  int * int -> t/3 random_gen:
    CCInt.random_range
  int * int -> int t:
    CCArray.(--)
    CCArray.(--^)
    CCSeq.Infix.(--)
    CCList.Infix.(--)
    CCListLabels.(--)
    CCSeq.Infix.(--^)
    CCArray.Infix.(--)
    CCList.Infix.(--^)
    CCListLabels.(--^)
    CCListLabels.range
    CCArray.Infix.(--^)
    CCListLabels.range'
  int * int -> int t/1:
    CCArrayLabels.(--)
    CCArrayLabels.(--^)
    CCListLabels.Infix.(--)
    CCArrayLabels.Infix.(--)
    CCListLabels.Infix.(--^)
    CCArrayLabels.Infix.(--^)
  int * int -> int t/2:
    CCSeq.(--)
    CCSeq.(--^)
    CCSeq.range
    CCRandom.int_range
  int * int -> int list option t/2:
    CCRandom.split_list
  int * int -> int t/3:
    CCList.(--)
    CCList.(--^)
    CCList.range
    CCList.range'
  int * int -> ():
    CCFormat.print_break
    CCFormat.print_tbreak
    CCFormat.set_geometry
    CCFormat.safe_set_geometry
  'a * 'b -> 'a:
    CCFun.const
    CCShimsFun_.const
