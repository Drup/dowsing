Some initial basic tests.

  $ dowsindex search --index index.db containers "int -> int -> int"
  Containers.max : int -> int -> int
  Containers.min : int -> int -> int
  CCShimsInt_.add : int -> int -> int
  CCShimsInt_.div : int -> int -> int
  CCShimsInt_.mul : int -> int -> int
  CCShimsInt_.rem : int -> int -> int
  CCShimsInt_.sub : int -> int -> int
  CCMonomorphic.max : int -> int -> int
  CCMonomorphic.min : int -> int -> int
  CCShimsInt_.logor : int -> int -> int
  CCShimsInt_.logand : int -> int -> int
  CCShimsInt_.logxor : int -> int -> int
  Containers.compare : int -> int -> int
  CCShimsInt_.compare : int -> int -> int
  ContainersLabels.max : int -> int -> int
  ContainersLabels.min : int -> int -> int
  CCMonomorphic.compare : int -> int -> int
  CCShimsInt_.shift_left : int -> int -> int
  CCShimsInt_.shift_right : int -> int -> int
  ContainersLabels.compare : int -> int -> int
  CCShimsInt_.shift_right_logical : int -> int -> int
  Containers.Hashtbl.hash : 'a -> int
  ContainersLabels.Hashtbl.hash : 'a -> int
  Containers.Hashtbl.seeded_hash : int -> 'a -> int
  ContainersLabels.Hashtbl.seeded_hash : int -> 'a -> int
  CCFun.const : 'a -> 'b -> 'a
  CCShimsFun_.const : 'a -> 'b -> 'a

  $ dowsindex search --index index.db containers "int -> int -> int -> int"
  Containers.Hashtbl.hash : 'a -> int
  ContainersLabels.Hashtbl.hash : 'a -> int
  Containers.Hashtbl.seeded_hash : int -> 'a -> int
  ContainersLabels.Hashtbl.seeded_hash : int -> 'a -> int
  Containers.Hashtbl.hash_param : int -> int -> 'a -> int
  ContainersLabels.Hashtbl.hash_param : int -> int -> 'a -> int
  CCFun.const : 'a -> 'b -> 'a
  CCShimsFun_.const : 'a -> 'b -> 'a

  $ dowsindex search --index index.db containers ". int -> int -> 'a"
  CCOrd.equiv : int -> int -> bool
  Containers.(<) : int -> int -> bool
  Containers.(=) : int -> int -> bool
  Containers.(>) : int -> int -> bool
  Containers.(<=) : int -> int -> bool
  Containers.(<>) : int -> int -> bool
  Containers.(>=) : int -> int -> bool
  CCMonomorphic.(<) : int -> int -> bool
  CCMonomorphic.(=) : int -> int -> bool
  CCMonomorphic.(>) : int -> int -> bool
  CCShimsInt_.equal : int -> int -> bool
  CCMonomorphic.(<=) : int -> int -> bool
  CCMonomorphic.(<>) : int -> int -> bool
  CCMonomorphic.(>=) : int -> int -> bool
  ContainersLabels.(<) : int -> int -> bool
  ContainersLabels.(=) : int -> int -> bool
  ContainersLabels.(>) : int -> int -> bool
  ContainersLabels.(<=) : int -> int -> bool
  ContainersLabels.(<>) : int -> int -> bool
  ContainersLabels.(>=) : int -> int -> bool
  Containers.max : int -> int -> int
  Containers.min : int -> int -> int
  CCShimsInt_.add : int -> int -> int
  CCShimsInt_.div : int -> int -> int
  CCShimsInt_.mul : int -> int -> int
  CCShimsInt_.rem : int -> int -> int
  CCShimsInt_.sub : int -> int -> int
  CCMonomorphic.max : int -> int -> int
  CCMonomorphic.min : int -> int -> int
  CCShimsInt_.logor : int -> int -> int
  CCShimsInt_.logand : int -> int -> int
  CCShimsInt_.logxor : int -> int -> int
  Containers.compare : int -> int -> int
  CCShimsInt_.compare : int -> int -> int
  ContainersLabels.max : int -> int -> int
  ContainersLabels.min : int -> int -> int
  CCMonomorphic.compare : int -> int -> int
  CCShimsInt_.shift_left : int -> int -> int
  CCShimsInt_.shift_right : int -> int -> int
  ContainersLabels.compare : int -> int -> int
  CCShimsInt_.shift_right_logical : int -> int -> int
  CCInt.random_range : int -> int -> t random_gen
  CCSeq.(--) : int -> int -> int t
  CCList.(--) : int -> int -> int t
  CCSeq.(--^) : int -> int -> int t
  CCSeq.range : int -> int -> int t
  CCArray.(--) : int -> int -> int t
  CCList.(--^) : int -> int -> int t
  CCList.range : int -> int -> int t
  CCArray.(--^) : int -> int -> int t
  CCList.range' : int -> int -> int t
  CCSeq.Infix.(--) : int -> int -> int t
  CCList.Infix.(--) : int -> int -> int t
  CCListLabels.(--) : int -> int -> int t
  CCSeq.Infix.(--^) : int -> int -> int t
  CCArray.Infix.(--) : int -> int -> int t
  CCArrayLabels.(--) : int -> int -> int t
  CCList.Infix.(--^) : int -> int -> int t
  CCListLabels.(--^) : int -> int -> int t
  CCListLabels.range : int -> int -> int t
  CCRandom.int_range : int -> int -> int t
  CCArray.Infix.(--^) : int -> int -> int t
  CCArrayLabels.(--^) : int -> int -> int t
  CCListLabels.range' : int -> int -> int t
  CCListLabels.Infix.(--) : int -> int -> int t
  CCArrayLabels.Infix.(--) : int -> int -> int t
  CCListLabels.Infix.(--^) : int -> int -> int t
  CCArrayLabels.Infix.(--^) : int -> int -> int t
  CCVector.(--) : int -> int -> (int, 'mut) t
  CCVector.(--^) : int -> int -> (int, 'mut) t
  CCRandom.split_list : int -> len:int -> int list option t
  CCFormat.print_break : int -> int -> unit
  CCFormat.print_tbreak : int -> int -> unit
  CCFormat.set_geometry : max_indent:int -> margin:int -> unit
  CCFormat.safe_set_geometry : max_indent:int -> margin:int -> unit
  CCFun.id : 'a -> 'a
  CCShimsFun_.id : 'a -> 'a
  CCFun.opaque_identity : 'a -> 'a
  CCArray.make : int -> 'a -> 'a array
  CCArray.create : int -> 'a -> 'a array
  CCArrayLabels.make : int -> 'a -> 'a array
  CCShimsArray_.make : int -> 'a -> 'a array
  CCArrayLabels.create : int -> 'a -> 'a array
  CCShimsArray_.create : int -> 'a -> 'a array
  CCShimsArrayLabels_.make : int -> 'a -> 'a array
  CCShimsArrayLabels_.create : int -> 'a -> 'a array
  Containers.Hashtbl.hash : 'a -> int
  ContainersLabels.Hashtbl.hash : 'a -> int
  Containers.Hashtbl.seeded_hash : int -> 'a -> int
  ContainersLabels.Hashtbl.seeded_hash : int -> 'a -> int
  CCOpt.pure : 'a -> 'a t
  CCSeq.pure : 'a -> 'a t
  CCList.pure : 'a -> 'a t
  CCOpt.return : 'a -> 'a t
  CCParse.pure : 'a -> 'a t
  CCRef.create : 'a -> 'a t
  CCSeq.return : 'a -> 'a t
  CCList.return : 'a -> 'a t
  CCRandom.pure : 'a -> 'a t
  CCParse.return : 'a -> 'a t
  CCRandom.return : 'a -> 'a t
  CCSeq.singleton : 'a -> 'a t
  CCListLabels.pure : 'a -> 'a t
  CCFun.Monad.return : 'a -> 'a t
  CCSeq.MONAD.return : 'a -> 'a t
  CCList.MONAD.return : 'a -> 'a t
  CCListLabels.return : 'a -> 'a t
  CCResult.MONAD.return : 'a -> 'a t
  CCListLabels.MONAD.return : 'a -> 'a t
  CCSeq.repeat : ?n:int -> 'a -> 'a t
  CCList.replicate : int -> 'a -> 'a t
  CCListLabels.replicate : int -> 'a -> 'a t
  CCEither.left : 'a -> ('a, 'b) t
  CCResult.pure : 'a -> ('a, 'err) t
  CCResult.return : 'a -> ('a, 'err) t
  CCVector.return : 'a -> ('a, 'mut) t
  CCVector.make : int -> 'a -> ('a, 'mut) t
  CCResult.fail : 'err -> ('a, 'err) t
  CCEither.right : 'b -> ('a, 'b) t
  CCVector.create_with : ?capacity:int -> 'a -> ('a, rw) t
  CCPair.dup : 'a -> 'a * 'a
  CCFun.const : 'a -> 'b -> 'a
  CCShimsFun_.const : 'a -> 'b -> 'a
  CCPair.make : 'a -> 'b -> ('a, 'b) t
  CCPair.swap : 'a * 'b -> 'b * 'a
