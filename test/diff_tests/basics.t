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
  int * int -> (int, 'a) t:
    CCVector.(--)
    CCVector.(--^)
  int * int -> int list option t:
    CCRandom.split_list
  int * int -> ():
    CCFormat.print_break
    CCFormat.print_tbreak
    CCFormat.set_geometry
    CCFormat.safe_set_geometry
  'a -> 'a:
    CCFun.id
    CCShimsFun_.id
    CCFun.opaque_identity
  'a * int -> 'a array:
    CCArray.make
    CCArray.create
    CCArrayLabels.make
    CCShimsArray_.make
    CCArrayLabels.create
    CCShimsArray_.create
    CCShimsArrayLabels_.make
    CCShimsArrayLabels_.create
  'a -> int:
    Containers.Hashtbl.hash
    ContainersLabels.Hashtbl.hash
  'a * int -> int:
    Containers.Hashtbl.seeded_hash
    ContainersLabels.Hashtbl.seeded_hash
  'a -> 'a t:
    CCOpt.pure
    CCSeq.pure
    CCList.pure
    CCOpt.return
    CCParse.pure
    CCRef.create
    CCSeq.return
    CCList.return
    CCRandom.pure
    CCParse.return
    CCRandom.return
    CCSeq.singleton
    CCListLabels.pure
    CCFun.Monad.return
    CCSeq.MONAD.return
    CCList.MONAD.return
    CCListLabels.return
    CCResult.MONAD.return
    CCListLabels.MONAD.return
  'a * int -> 'a t:
    CCSeq.repeat
    CCList.replicate
    CCListLabels.replicate
  'a -> ('a, 'b) t:
    CCEither.left
    CCResult.pure
    CCResult.return
    CCVector.return
  'a * int -> ('a, 'b) t:
    CCVector.make
  'b -> ('a, 'b) t:
    CCResult.fail
    CCEither.right
  'a * int -> ('a, rw) t:
    CCVector.create_with
  'a -> ('a * 'a):
    CCPair.dup
  'a * 'b -> 'a:
    CCFun.const
    CCShimsFun_.const
  'a * 'b -> ('a, 'b) t:
    CCPair.make
  'a * 'b -> ('a * 'b):
    CCPair.swap
