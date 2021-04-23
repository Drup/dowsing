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

  $ dowsindex search index.db "int -> int -> 'a"
  CCFormat.safe_set_geometry: int * int -> (())
  CCInt.random_range: int * int -> t/2 random_gen
  CCRandom.split_list: int * int -> int list option t/2
  CCSeq.(--^): int * int -> int t/2
  CCShimsInt_.compare: int * int -> int
  CCShimsInt_.equal: int * int -> bool
  CCVector.(--): int * int -> (int, 'mut) t/2
  CCVector.(--^): int * int -> (int, 'mut) t/2
  CCListLabels.Infix.(--^): int * int -> int t/1
  CCSeq.Infix.(--^): int * int -> int t
  Containers.Vector.(--): int * int -> (int, 'mut) t/2
  Containers.Vector.(--^): int * int -> (int, 'mut) t/2
  ContainersLabels.Vector.(--): int * int -> (int, 'mut) t/2
  ContainersLabels.Vector.(--^): int * int -> (int, 'mut) t/2
  CCArray.create: 'a * int -> 'a array
  CCArray.make: 'a * int -> 'a array
  CCArrayLabels.create: 'a * int -> 'a array
  CCArrayLabels.make: 'a * int -> 'a array
  CCEither.left: 'a -> ('a, 'b) t/2
  CCEither.right: 'b -> ('a, 'b) t/2
  CCFun.id: 'a -> 'a
  CCFun.opaque_identity: 'a -> 'a
  CCList.pure: 'a -> 'a t/2
  CCList.replicate: 'a * int -> 'a t/2
  CCList.return: 'a -> 'a t/2
  CCListLabels.pure: 'a -> 'a t
  CCListLabels.replicate: 'a * int -> 'a t
  CCListLabels.return: 'a -> 'a t
  CCOpt.pure: 'a -> 'a t/2
  CCOpt.return: 'a -> 'a t/2
  CCPair.dup: 'a -> ('a * 'a)
  CCParse.pure: 'a -> 'a t/2
  CCParse.return: 'a -> 'a t/2
  CCRandom.pure: 'a -> 'a t/2
  CCRandom.return: 'a -> 'a t/2
  CCRef.create: 'a -> 'a t/3
  CCResult.fail: 'err -> ('a, 'err) t/4
  CCResult.pure: 'a -> ('a, 'err) t/4
  CCResult.return: 'a -> ('a, 'err) t/4
  CCSeq.pure: 'a -> 'a t/2
  CCSeq.repeat: 'a * int -> 'a t/2
  CCSeq.return: 'a -> 'a t/2
  CCSeq.singleton: 'a -> 'a t/2
  CCShimsArrayLabels_.create: 'a * int -> 'a array
  CCShimsArrayLabels_.make: 'a * int -> 'a array
  CCShimsArray_.create: 'a * int -> 'a array
  CCShimsArray_.make: 'a * int -> 'a array
  CCShimsFun_.id: 'a -> 'a
  CCVector.create_with: 'a * int -> ('a, rw) t/2
  CCVector.make: 'a * int -> ('a, 'mut) t/2
  CCVector.return: 'a -> ('a, 'mut) t/2
  CCFun.Monad.return: 'a -> 'a t/1
  CCList.MONAD.return: 'a -> 'a t
  CCListLabels.MONAD.return: 'a -> 'a t
  CCResult.MONAD.return: 'a -> 'a t
  CCSeq.MONAD.return: 'a -> 'a t
  Containers.Array.create: 'a * int -> 'a array
  Containers.Array.make: 'a * int -> 'a array
  Containers.Either.left: 'a -> ('a, 'b) t/2
  Containers.Either.right: 'b -> ('a, 'b) t/2
  Containers.Fun.id: 'a -> 'a
  Containers.Fun.opaque_identity: 'a -> 'a
  Containers.Hashtbl.hash: 'a -> int
  Containers.Hashtbl.seeded_hash: 'a * int -> int
  Containers.List.pure: 'a -> 'a t/2
  Containers.List.replicate: 'a * int -> 'a t/2
  Containers.List.return: 'a -> 'a t/2
  Containers.Option.pure: 'a -> 'a t/2
  Containers.Option.return: 'a -> 'a t/2
  Containers.Pair.dup: 'a -> ('a * 'a)
  Containers.Parse.pure: 'a -> 'a t/2
  Containers.Parse.return: 'a -> 'a t/2
  Containers.Random.pure: 'a -> 'a t/2
  Containers.Random.return: 'a -> 'a t/2
  Containers.Ref.create: 'a -> 'a t/3
  Containers.Result.fail: 'err -> ('a, 'err) t/4
  Containers.Result.pure: 'a -> ('a, 'err) t/4
  Containers.Result.return: 'a -> ('a, 'err) t/4
  Containers.Seq.pure: 'a -> 'a t/2
  Containers.Seq.repeat: 'a * int -> 'a t/2
  Containers.Seq.return: 'a -> 'a t/2
  Containers.Seq.singleton: 'a -> 'a t/2
  Containers.Vector.create_with: 'a * int -> ('a, rw) t/2
  Containers.Vector.make: 'a * int -> ('a, 'mut) t/2
  Containers.Vector.return: 'a -> ('a, 'mut) t/2
  ContainersLabels.Array.create: 'a * int -> 'a array
  ContainersLabels.Array.make: 'a * int -> 'a array
  ContainersLabels.Either.left: 'a -> ('a, 'b) t/2
  ContainersLabels.Either.right: 'b -> ('a, 'b) t/2
  ContainersLabels.Fun.id: 'a -> 'a
  ContainersLabels.Fun.opaque_identity: 'a -> 'a
  ContainersLabels.Hashtbl.hash: 'a -> int
  ContainersLabels.Hashtbl.seeded_hash: 'a * int -> int
  ContainersLabels.List.pure: 'a -> 'a t
  ContainersLabels.List.replicate: 'a * int -> 'a t
  ContainersLabels.List.return: 'a -> 'a t
  ContainersLabels.Option.pure: 'a -> 'a t/2
  ContainersLabels.Option.return: 'a -> 'a t/2
  ContainersLabels.Pair.dup: 'a -> ('a * 'a)
  ContainersLabels.Parse.pure: 'a -> 'a t/2
  ContainersLabels.Parse.return: 'a -> 'a t/2
  ContainersLabels.Random.pure: 'a -> 'a t/2
  ContainersLabels.Random.return: 'a -> 'a t/2
  ContainersLabels.Ref.create: 'a -> 'a t/3
  ContainersLabels.Result.fail: 'err -> ('a, 'err) t/4
  ContainersLabels.Result.pure: 'a -> ('a, 'err) t/4
  ContainersLabels.Result.return: 'a -> ('a, 'err) t/4
  ContainersLabels.Seq.pure: 'a -> 'a t/2
  ContainersLabels.Seq.repeat: 'a * int -> 'a t/2
  ContainersLabels.Seq.return: 'a -> 'a t/2
  ContainersLabels.Seq.singleton: 'a -> 'a t/2
  ContainersLabels.Vector.create_with: 'a * int -> ('a, rw) t/2
  ContainersLabels.Vector.make: 'a * int -> ('a, 'mut) t/2
  ContainersLabels.Vector.return: 'a -> ('a, 'mut) t/2
  Containers.Fun.Monad.return: 'a -> 'a t/1
  Containers.List.MONAD.return: 'a -> 'a t
  Containers.Result.MONAD.return: 'a -> 'a t
  Containers.Seq.MONAD.return: 'a -> 'a t
  ContainersLabels.Fun.Monad.return: 'a -> 'a t/1
  ContainersLabels.List.MONAD.return: 'a -> 'a t
  ContainersLabels.Result.MONAD.return: 'a -> 'a t
  ContainersLabels.Seq.MONAD.return: 'a -> 'a t
  CCFun.const: 'a * 'b -> 'a
  CCPair.make: 'a * 'b -> ('a, 'b) t/2
  CCPair.swap: 'b * 'a -> ('b * 'a)
  CCShimsFun_.const: 'a * 'b -> 'a
  Containers.Fun.const: 'a * 'b -> 'a
  Containers.Pair.make: 'a * 'b -> ('a, 'b) t/2
  Containers.Pair.swap: 'b * 'a -> ('b * 'a)
  ContainersLabels.Fun.const: 'a * 'b -> 'a
  ContainersLabels.Pair.make: 'a * 'b -> ('a, 'b) t/2
  ContainersLabels.Pair.swap: 'b * 'a -> ('b * 'a)
