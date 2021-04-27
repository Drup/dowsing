Some initial basic tests.

  $ dowsindex search index.db "int -> int -> int"
  int * int -> int:
    CCShimsInt_.compare
    CCShimsInt_.logxor
    CCShimsInt_.logor
    CCShimsInt_.logand
    CCShimsInt_.rem
    CCShimsInt_.div
    CCShimsInt_.shift_right_logical
    CCShimsInt_.shift_right
    CCShimsInt_.shift_left
    CCShimsInt_.sub
    CCShimsInt_.add
    CCShimsInt_.mul
    CCMonomorphic.max
    CCMonomorphic.min
    CCMonomorphic.compare
    Containers.max
    Containers.min
    Containers.compare
    Containers.Monomorphic.max
    Containers.Monomorphic.min
    Containers.Monomorphic.compare
    ContainersLabels.max
    ContainersLabels.min
    ContainersLabels.compare
    ContainersLabels.Monomorphic.max
    ContainersLabels.Monomorphic.min
    ContainersLabels.Monomorphic.compare
  'a -> int:
    Containers.Hashtbl.hash
    ContainersLabels.Hashtbl.hash
  'a * int -> int:
    Containers.Hashtbl.seeded_hash
    ContainersLabels.Hashtbl.seeded_hash
  'a * 'b -> 'a:
    CCShimsFun_.const
    CCFun.const
    Containers.Fun.const
    ContainersLabels.Fun.const

  $ dowsindex search index.db "int -> int -> int -> int"
  'a -> int:
    Containers.Hashtbl.hash
    ContainersLabels.Hashtbl.hash
  'a * int * int -> int:
    Containers.Hashtbl.hash_param
    ContainersLabels.Hashtbl.hash_param
  'a * int -> int:
    Containers.Hashtbl.seeded_hash
    ContainersLabels.Hashtbl.seeded_hash
  'a * 'b -> 'a:
    CCShimsFun_.const
    CCFun.const
    Containers.Fun.const
    ContainersLabels.Fun.const

  $ dowsindex search index.db "int -> int -> 'a"
  int * int -> (()):
    CCFormat.safe_set_geometry
    CCFormat.set_geometry
    CCFormat.print_tbreak
    CCFormat.print_break
    Containers.Format.safe_set_geometry
    Containers.Format.set_geometry
    Containers.Format.print_tbreak
    Containers.Format.print_break
    ContainersLabels.Format.safe_set_geometry
    ContainersLabels.Format.set_geometry
    ContainersLabels.Format.print_tbreak
    ContainersLabels.Format.print_break
    Containers_codegen.Fmt.safe_set_geometry
    Containers_codegen.Fmt.set_geometry
    Containers_codegen.Fmt.print_tbreak
    Containers_codegen.Fmt.print_break
  int * int -> t/3 random_gen:
    CCInt.random_range
    Containers.Int.random_range
    ContainersLabels.Int.random_range
  int * int -> int t/3:
    CCList.(--^)
    CCList.(--)
    CCList.range'
    CCList.range
    Containers.List.(--^)
    Containers.List.(--)
    Containers.List.range'
    Containers.List.range
  int * int -> int list option t/2:
    CCRandom.split_list
    Containers.Random.split_list
    ContainersLabels.Random.split_list
  int * int -> int t/2:
    CCSeq.(--^)
    CCSeq.(--)
    CCSeq.range
    CCRandom.int_range
    Containers.Seq.(--^)
    Containers.Seq.(--)
    Containers.Seq.range
    Containers.Random.int_range
    ContainersLabels.Seq.(--^)
    ContainersLabels.Seq.(--)
    ContainersLabels.Seq.range
    ContainersLabels.Random.int_range
  'a -> 'a:
    CCShimsFun_.id
    CCFun.opaque_identity
    CCFun.id
    Containers.Fun.opaque_identity
    Containers.Fun.id
    ContainersLabels.Fun.opaque_identity
    ContainersLabels.Fun.id
  int * int -> int:
    CCShimsInt_.compare
    CCShimsInt_.logxor
    CCShimsInt_.logor
    CCShimsInt_.logand
    CCShimsInt_.rem
    CCShimsInt_.div
    CCShimsInt_.shift_right_logical
    CCShimsInt_.shift_right
    CCShimsInt_.shift_left
    CCShimsInt_.sub
    CCShimsInt_.add
    CCShimsInt_.mul
    CCMonomorphic.max
    CCMonomorphic.min
    CCMonomorphic.compare
    Containers.max
    Containers.min
    Containers.compare
    Containers.Monomorphic.max
    Containers.Monomorphic.min
    Containers.Monomorphic.compare
    ContainersLabels.max
    ContainersLabels.min
    ContainersLabels.compare
    ContainersLabels.Monomorphic.max
    ContainersLabels.Monomorphic.min
    ContainersLabels.Monomorphic.compare
  int * int -> bool:
    CCShimsInt_.equal
    CCOrd.equiv
    CCMonomorphic.(>=)
    CCMonomorphic.(>)
    CCMonomorphic.(<=)
    CCMonomorphic.(<>)
    CCMonomorphic.(<)
    CCMonomorphic.(=)
    Containers.(>=)
    Containers.(>)
    Containers.(<=)
    Containers.(<>)
    Containers.(<)
    Containers.(=)
    Containers.Ord.equiv
    Containers.Monomorphic.(>=)
    Containers.Monomorphic.(>)
    Containers.Monomorphic.(<=)
    Containers.Monomorphic.(<>)
    Containers.Monomorphic.(<)
    Containers.Monomorphic.(=)
    ContainersLabels.(>=)
    ContainersLabels.(>)
    ContainersLabels.(<=)
    ContainersLabels.(<>)
    ContainersLabels.(<)
    ContainersLabels.(=)
    ContainersLabels.Ord.equiv
    ContainersLabels.Monomorphic.(>=)
    ContainersLabels.Monomorphic.(>)
    ContainersLabels.Monomorphic.(<=)
    ContainersLabels.Monomorphic.(<>)
    ContainersLabels.Monomorphic.(<)
    ContainersLabels.Monomorphic.(=)
  int * int -> int t/1:
    CCListLabels.Infix.(--^)
    CCListLabels.Infix.(--)
    CCArrayLabels.(--^)
    CCArrayLabels.(--)
    CCArrayLabels.Infix.(--^)
    CCArrayLabels.Infix.(--)
    ContainersLabels.List.Infix.(--^)
    ContainersLabels.List.Infix.(--)
    ContainersLabels.Array.(--^)
    ContainersLabels.Array.(--)
    ContainersLabels.Array.Infix.(--^)
    ContainersLabels.Array.Infix.(--)
  int * int -> int t:
    CCSeq.Infix.(--^)
    CCSeq.Infix.(--)
    CCList.Infix.(--^)
    CCList.Infix.(--)
    CCListLabels.(--^)
    CCListLabels.(--)
    CCListLabels.range'
    CCListLabels.range
    CCArray.(--^)
    CCArray.(--)
    CCArray.Infix.(--^)
    CCArray.Infix.(--)
    Containers.Seq.Infix.(--^)
    Containers.Seq.Infix.(--)
    Containers.List.Infix.(--^)
    Containers.List.Infix.(--)
    Containers.Array.(--^)
    Containers.Array.(--)
    Containers.Array.Infix.(--^)
    Containers.Array.Infix.(--)
    ContainersLabels.Seq.Infix.(--^)
    ContainersLabels.Seq.Infix.(--)
    ContainersLabels.List.(--^)
    ContainersLabels.List.(--)
    ContainersLabels.List.range'
    ContainersLabels.List.range
  'a * int -> int:
    Containers.Hashtbl.seeded_hash
    ContainersLabels.Hashtbl.seeded_hash
  'a * 'b -> 'a:
    CCShimsFun_.const
    CCFun.const
    Containers.Fun.const
    ContainersLabels.Fun.const
