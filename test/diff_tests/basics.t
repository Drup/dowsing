Some initial basic tests.

  $ dowsindex search --index index.db containers "int -> int -> int"
  Containers.max : int -> int -> int
  Containers.min : int -> int -> int
  CCMonomorphic.max : int -> int -> int
  CCMonomorphic.min : int -> int -> int
  Containers.compare : int -> int -> int
  ContainersLabels.max : int -> int -> int
  ContainersLabels.min : int -> int -> int
  CCMonomorphic.compare : int -> int -> int
  ContainersLabels.compare : int -> int -> int
  Containers.Hashtbl.seeded_hash : int -> 'a -> int
  ContainersLabels.Hashtbl.seeded_hash : int -> 'a -> int
  CCFun.const : 'a -> 'b -> 'a
  Containers.Hashtbl.hash : 'a -> int
  ContainersLabels.Hashtbl.hash : 'a -> int

  $ dowsindex search --index index.db containers "int -> int -> int -> int"
  Containers.Hashtbl.hash_param : int -> int -> 'a -> int
  ContainersLabels.Hashtbl.hash_param : int -> int -> 'a -> int
  Containers.Hashtbl.seeded_hash : int -> 'a -> int
  ContainersLabels.Hashtbl.seeded_hash : int -> 'a -> int
  CCFun.const : 'a -> 'b -> 'a
  Containers.Hashtbl.hash : 'a -> int
  ContainersLabels.Hashtbl.hash : 'a -> int

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
  CCMonomorphic.max : int -> int -> int
  CCMonomorphic.min : int -> int -> int
  Containers.compare : int -> int -> int
  ContainersLabels.max : int -> int -> int
  ContainersLabels.min : int -> int -> int
  CCMonomorphic.compare : int -> int -> int
  ContainersLabels.compare : int -> int -> int
  CCFormat.print_break : int -> int -> unit
  CCFormat.print_tbreak : int -> int -> unit
  CCFormat.set_geometry : max_indent:int -> margin:int -> unit
  CCFormat.safe_set_geometry : max_indent:int -> margin:int -> unit
  Containers.Hashtbl.seeded_hash : int -> 'a -> int
  ContainersLabels.Hashtbl.seeded_hash : int -> 'a -> int
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
  CCSeq.Infix.(--^) : int -> int -> int t
  CCArray.Infix.(--) : int -> int -> int t
  CCArrayLabels.(--) : int -> int -> int t
  CCList.Infix.(--^) : int -> int -> int t
  CCListLabels.range : int -> int -> int t
  CCRandom.int_range : int -> int -> int t
  CCArray.Infix.(--^) : int -> int -> int t
  CCArrayLabels.(--^) : int -> int -> int t
  CCListLabels.range' : int -> int -> int t
  CCArrayLabels.Infix.(--) : int -> int -> int t
  CCArrayLabels.Infix.(--^) : int -> int -> int t
  CCListLabels.(--) : int -> int -> int CCList.t
  CCListLabels.(--^) : int -> int -> int CCList.t
  CCListLabels.Infix.(--) : int -> int -> int CCList.t
  CCListLabels.Infix.(--^) : int -> int -> int CCList.t
  CCArray.make : int -> 'a -> 'a array
  CCArray.create : int -> 'a -> 'a array
  CCArrayLabels.make : int -> 'a -> 'a array
  CCArrayLabels.create : int -> 'a -> 'a array
  CCFun.const : 'a -> 'b -> 'a
  CCSeq.repeat : ?n:int -> 'a -> 'a t
  CCList.replicate : int -> 'a -> 'a t
  CCListLabels.replicate : int -> 'a -> 'a t
  CCVector.(--) : int -> int -> (int, 'mut) t
  CCVector.(--^) : int -> int -> (int, 'mut) t
  Containers.Hashtbl.hash_param : int -> int -> 'a -> int
  ContainersLabels.Hashtbl.hash_param : int -> int -> 'a -> int
  CCString.sub : string -> int -> int -> string
  CCStringLabels.sub : string -> pos:int -> len:int -> string
  CCFormat.pp_print_break : formatter -> int -> int -> unit
  CCFormat.pp_print_tbreak : formatter -> int -> int -> unit
  CCFormat.pp_set_geometry : formatter -> max_indent:int -> margin:int -> unit
  CCFormat.pp_safe_set_geometry :
  formatter -> max_indent:int -> margin:int -> unit
  Containers.Hashtbl.hash : 'a -> int
  ContainersLabels.Hashtbl.hash : 'a -> int
  CCVector.make : int -> 'a -> ('a, 'mut) t
  CCVector.create_with : ?capacity:int -> 'a -> ('a, rw) t
  CCRandom.split_list : int -> len:int -> int list option t
  CCList.range_by : step:int -> int -> int -> int t
  CCListLabels.range_by : step:int -> int -> int -> int t
  CCArray.swap : 'a t -> int -> int -> unit
  CCArrayLabels.swap : 'a t -> int -> int -> unit
  CCPair.make : 'a -> 'b -> ('a, 'b) t
  CCArray.sub : 'a array -> int -> int -> 'a array
  CCArrayLabels.sub : 'a array -> pos:int -> len:int -> 'a array
  CCArray.make_matrix : int -> int -> 'a -> 'a array array
  CCArray.create_matrix : int -> int -> 'a -> 'a array array
  CCArrayLabels.make_matrix : dimx:int -> dimy:int -> 'a -> 'a array array
  CCArrayLabels.create_matrix : dimx:int -> dimy:int -> 'a -> 'a array array
  CCAtomic.compare_and_set : 'a t -> 'a -> 'a -> bool
  Containers.Hashtbl.seeded_hash_param : int -> int -> int -> 'a -> int
  ContainersLabels.Hashtbl.seeded_hash_param : int -> int -> int -> 'a -> int
  CCArray.set : 'a array -> int -> 'a -> unit
  CCArrayLabels.set : 'a array -> int -> 'a -> unit
  CCArray.unsafe_set : 'a array -> int -> 'a -> unit
  CCArrayLabels.unsafe_set : 'a array -> int -> 'a -> unit
  CCString.fill : bytes -> int -> int -> char -> unit
  CCStringLabels.fill : bytes -> pos:int -> len:int -> char -> unit
  CCString.unsafe_fill : bytes -> int -> int -> char -> unit
  CCStringLabels.unsafe_fill : bytes -> pos:int -> len:int -> char -> unit
  CCByte_buffer.append_subbytes : t -> bytes -> int -> int -> unit
  CCByte_buffer.append_substring : t -> string -> int -> int -> unit
  CCPair.swap : 'a * 'b -> 'b * 'a
  CCString.is_sub : sub:string -> int -> string -> int -> sub_len:int -> bool
  CCStringLabels.is_sub :
  sub:string -> sub_pos:int -> string -> pos:int -> sub_len:int -> bool
  CCFun.iterate : int -> ('a -> 'a) -> 'a -> 'a
  CCVector.slice_iter : ('a, 'b) t -> int -> int -> 'a iter
  CCList.set_at_idx : int -> 'a -> 'a t -> 'a t
  CCList.insert_at_idx : int -> 'a -> 'a t -> 'a t
  CCListLabels.set_at_idx : int -> 'a -> 'a t -> 'a t
  CCListLabels.insert_at_idx : int -> 'a -> 'a t -> 'a t
  CCVector.set : ('a, rw) t -> int -> 'a -> unit
  CCVector.insert : ('a, rw) t -> int -> 'a -> unit
  CCVector.ensure_with : init:'a -> ('a, rw) t -> int -> unit
  CCVector.resize_with_init : ('a, rw) t -> init:'a -> int -> unit
  CCHashtbl.decr : ?by:int -> ('a, int) Hashtbl.t -> 'a -> unit
  CCHashtbl.incr : ?by:int -> ('a, int) Hashtbl.t -> 'a -> unit
  CCHashtbl.Poly.decr : ?by:int -> ('a, int) Hashtbl.t -> 'a -> unit
  CCHashtbl.Poly.incr : ?by:int -> ('a, int) Hashtbl.t -> 'a -> unit
  Containers.Hashtbl.decr : ?by:int -> ('a, int) Hashtbl.t -> 'a -> unit
  Containers.Hashtbl.incr : ?by:int -> ('a, int) Hashtbl.t -> 'a -> unit
  ContainersLabels.Hashtbl.decr : ?by:int -> ('a, int) Hashtbl.t -> 'a -> unit
  ContainersLabels.Hashtbl.incr : ?by:int -> ('a, int) Hashtbl.t -> 'a -> unit
  CCArray.fill : 'a array -> int -> int -> 'a -> unit
  CCArrayLabels.fill : 'a array -> pos:int -> len:int -> 'a -> unit
  CCString.unsafe_blit : string -> int -> bytes -> int -> int -> unit
  CCStringLabels.unsafe_blit :
  src:string -> src_pos:int -> dst:bytes -> dst_pos:int -> len:int -> unit
  CCString.blit : t -> int -> Bytes.t -> int -> int -> unit
  CCStringLabels.blit :
  src:t -> src_pos:int -> dst:Bytes.t -> dst_pos:int -> len:int -> unit
  CCFun.id : 'a -> 'a
  CCFun.opaque_identity : 'a -> 'a
  CCOpt.pure : 'a -> 'a t
  CCOpt.some : 'a -> 'a t
  CCSeq.pure : 'a -> 'a t
  CCList.pure : 'a -> 'a t
  CCOpt.return : 'a -> 'a t
  CCParse.pure : 'a -> 'a t
  CCRef.create : 'a -> 'a t
  CCSeq.return : 'a -> 'a t
  CCAtomic.make : 'a -> 'a t
  CCList.return : 'a -> 'a t
  CCOption.pure : 'a -> 'a t
  CCOption.some : 'a -> 'a t
  CCRandom.pure : 'a -> 'a t
  CCParse.return : 'a -> 'a t
  CCOption.return : 'a -> 'a t
  CCRandom.return : 'a -> 'a t
  CCSeq.singleton : 'a -> 'a t
  CCListLabels.pure : 'a -> 'a t
  CCFun.Monad.return : 'a -> 'a t
  CCSeq.MONAD.return : 'a -> 'a t
  CCList.MONAD.return : 'a -> 'a t
  CCListLabels.return : 'a -> 'a t
  CCResult.MONAD.return : 'a -> 'a t
  CCListLabels.MONAD.return : 'a -> 'a t
  CCPair.fst_map : ('a -> 'b) -> 'a * 'c -> 'b
  CCPair.snd_map : ('a -> 'b) -> 'c * 'a -> 'b
  CCHashtbl.get_or : ('a, 'b) Hashtbl.t -> 'a -> default:'b -> 'b
  CCHashtbl.Poly.get_or : ('a, 'b) Hashtbl.t -> 'a -> default:'b -> 'b
  Containers.Hashtbl.get_or : ('a, 'b) Hashtbl.t -> 'a -> default:'b -> 'b
  ContainersLabels.Hashtbl.get_or :
  ('a, 'b) Hashtbl.t -> 'a -> default:'b -> 'b
  CCOrd.(<?>) : int -> 'a t * 'a * 'a -> int
  CCOrd.Infix.(<?>) : int -> 'a t * 'a * 'a -> int
  Containers.Hashtbl.add : ('a, 'b) t -> 'a -> 'b -> unit
  Containers.Hashtbl.replace : ('a, 'b) t -> 'a -> 'b -> unit
  ContainersLabels.Hashtbl.add : ('a, 'b) t -> 'a -> 'b -> unit
  ContainersLabels.Hashtbl.replace : ('a, 'b) t -> 'a -> 'b -> unit
  CCResult.fail : 'err -> ('a, 'err) t
  CCEither.right : 'b -> ('a, 'b) t
  CCEither.left : 'a -> ('a, 'b) t
  CCResult.pure : 'a -> ('a, 'err) t
  CCResult.return : 'a -> ('a, 'err) t
  CCVector.return : 'a -> ('a, 'mut) t
  CCFun.flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
  CCFun.curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
  CCPair.fold : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
  CCPair.merge : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
  CCFun.uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
  CCHashtbl.add_list : ('a, 'b list) Hashtbl.t -> 'a -> 'b -> unit
  CCHashtbl.Poly.add_list : ('a, 'b list) Hashtbl.t -> 'a -> 'b -> unit
  Containers.Hashtbl.add_list : ('a, 'b list) Hashtbl.t -> 'a -> 'b -> unit
  ContainersLabels.Hashtbl.add_list :
  ('a, 'b list) Hashtbl.t -> 'a -> 'b -> unit
  CCPair.iter : ('a -> 'b -> unit) -> 'a * 'b -> unit
  CCArray.blit : 'a array -> int -> 'a array -> int -> int -> unit
  CCArrayLabels.blit :
  src:'a array -> src_pos:int -> dst:'a array -> dst_pos:int -> len:int -> unit
  CCFormat.pp_print_custom_break :
  formatter ->
  fits:string * int * string -> breaks:string * int * string -> unit
  CCPair.map_same : ('a -> 'b) -> 'a * 'a -> 'b * 'b
  CCPair.dup : 'a -> 'a * 'a
  CCArray.memq : 'a -> 'a array -> bool
  CCArrayLabels.memq : 'a -> set:'a array -> bool
  CCList.memq : 'a -> 'a list -> bool
  CCListLabels.memq : 'a -> set:'a list -> bool
  CCUtf8_string.fold : ?idx:int -> ('a -> uchar -> 'a) -> 'a -> t -> 'a
  CCFormat.to_string : 'a printer -> 'a -> string
  CCFormat.Dump.to_string : 'a t -> 'a -> string
  CCOpt.return_if : bool -> 'a -> 'a t
  CCOption.return_if : bool -> 'a -> 'a t
  CCMap.S.singleton : key -> 'a -> 'a t
  CCMap.Make.singleton : key -> 'a -> 'a t
  CCAtomic.set : 'a t -> 'a -> unit
  CCList.Ref.push : 'a t -> 'a -> unit
  CCListLabels.Ref.push : 'a t -> 'a -> unit
  CCPair.(<<<) : ('a -> 'b) -> 'a * 'c -> 'b * 'c
  CCPair.map_fst : ('a -> 'b) -> 'a * 'c -> 'b * 'c
  CCPair.(>>>) : ('a -> 'b) -> 'c * 'a -> 'c * 'b
  CCPair.map_snd : ('a -> 'b) -> 'c * 'a -> 'c * 'b
  CCList.assq : 'a -> ('a * 'b) list -> 'b
  CCListLabels.assq : 'a -> ('a * 'b) list -> 'b
  Containers.Hashtbl.find : ('a, 'b) t -> 'a -> 'b
  ContainersLabels.Hashtbl.find : ('a, 'b) t -> 'a -> 'b
  CCFun.(@@) : ('a -> 'b) -> 'a -> 'b
  CCFun.(|>) : 'a -> ('a -> 'b) -> 'b
  CCFun.Infix.(@@) : ('a -> 'b) -> 'a -> 'b
  CCFun.Infix.(|>) : 'a -> ('a -> 'b) -> 'b
  CCList.mem_assq : 'a -> ('a * 'b) list -> bool
  CCListLabels.mem_assq : 'a -> map:('a * 'b) list -> bool
  Containers.Hashtbl.mem : ('a, 'b) t -> 'a -> bool
  ContainersLabels.Hashtbl.mem : ('a, 'b) t -> 'a -> bool
  CCFun.negate : ('a -> bool) -> 'a -> bool
  CCFormat.const : 'a printer -> 'a -> unit printer
  CCResult.wrap2 : ('a -> 'b -> 'c) -> 'a -> 'b -> ('c, exn) t
  Containers.Hashtbl.remove : ('a, 'b) t -> 'a -> unit
  ContainersLabels.Hashtbl.remove : ('a, 'b) t -> 'a -> unit
  CCVector.push : ('a, rw) t -> 'a -> unit
  CCFun.compose_binop : ('a -> 'b) -> ('b -> 'b -> 'c) -> 'a -> 'a -> 'c
  CCFun.finally2 : h:(unit -> 'd) -> ('a -> 'b -> 'c) -> 'a -> 'b -> 'c
  CCHash.combine : 'a t -> hash -> 'a -> hash
  Containers.Hashtbl.find_all : ('a, 'b) t -> 'a -> 'b list
  ContainersLabels.Hashtbl.find_all : ('a, 'b) t -> 'a -> 'b list
  CCList.assq_opt : 'a -> ('a * 'b) t -> 'b option
  CCListLabels.assq_opt : 'a -> ('a * 'b) t -> 'b option
  Containers.Hashtbl.find_opt : ('a, 'b) t -> 'a -> 'b option
  ContainersLabels.Hashtbl.find_opt : ('a, 'b) t -> 'a -> 'b option
  CCHashtbl.get : ('a, 'b) Hashtbl.t -> 'a -> 'b option
  CCHashtbl.Poly.get : ('a, 'b) Hashtbl.t -> 'a -> 'b option
  Containers.Hashtbl.get : ('a, 'b) Hashtbl.t -> 'a -> 'b option
  ContainersLabels.Hashtbl.get : ('a, 'b) Hashtbl.t -> 'a -> 'b option
  CCOpt.to_result : 'e -> 'a t -> ('a, 'e) result
  CCOption.to_result : 'e -> 'a t -> ('a, 'e) result
  CCHashtbl.S.add : 'a t -> key -> 'a -> unit
  CCHashtbl.Make.add : 'a t -> key -> 'a -> unit
  CCHashtbl.S.replace : 'a t -> key -> 'a -> unit
  CCHashtbl.Make.replace : 'a t -> key -> 'a -> unit
  Containers.Hashtbl.S.add : 'a t -> key -> 'a -> unit
  Containers.Hashtbl.Make.add : 'a t -> key -> 'a -> unit
  Containers.Hashtbl.S.replace : 'a t -> key -> 'a -> unit
  Containers.Hashtbl.SeededS.add : 'a t -> key -> 'a -> unit
  ContainersLabels.Hashtbl.S.add : 'a t -> key -> 'a -> unit
  Containers.Hashtbl.Make.replace : 'a t -> key -> 'a -> unit
  Containers.Hashtbl.MakeSeeded.add : 'a t -> key -> 'a -> unit
  ContainersLabels.Hashtbl.Make.add : 'a t -> key -> 'a -> unit
  Containers.Hashtbl.SeededS.replace : 'a t -> key -> 'a -> unit
  ContainersLabels.Hashtbl.S.replace : 'a t -> key -> 'a -> unit
  ContainersLabels.Hashtbl.SeededS.add : 'a t -> key -> 'a -> unit
  Containers.Hashtbl.MakeSeeded.replace : 'a t -> key -> 'a -> unit
  ContainersLabels.Hashtbl.Make.replace : 'a t -> key -> 'a -> unit
  ContainersLabels.Hashtbl.MakeSeeded.add : 'a t -> key -> 'a -> unit
  ContainersLabels.Hashtbl.SeededS.replace : 'a t -> key -> 'a -> unit
  ContainersLabels.Hashtbl.MakeSeeded.replace : 'a t -> key -> 'a -> unit
  CCFormat.output : t -> 'a printer -> 'a -> unit
  CCArray.sort_generic :
  (module MONO_ARRAY with type elt = 'elt and type t = 'arr) ->
  cmp:('elt -> 'elt -> int) -> 'arr -> unit
  CCArrayLabels.sort_generic :
  (module MONO_ARRAY with type elt = 'elt and type t = 'arr) ->
  cmp:('elt -> 'elt -> int) -> 'arr -> unit
  CCOpt.value : 'a t -> default:'a -> 'a
  CCOpt.get_or : default:'a -> 'a t -> 'a
  CCOption.value : 'a t -> default:'a -> 'a
  CCOption.get_or : default:'a -> 'a t -> 'a
  CCAtomic.exchange : 'a t -> 'a -> 'a
  CCFun.lexicographic :
  ('a -> 'a -> int) -> ('a -> 'a -> int) -> 'a -> 'a -> int
  CCList.cons : 'a -> 'a list -> 'a list
  CCList.intersperse : 'a -> 'a list -> 'a list
  CCListLabels.intersperse : x:'a -> 'a list -> 'a list
  CCPair.to_string :
  ?sep:string -> ('a -> string) -> ('b -> string) -> 'a * 'b -> string
  CCResult.wrap1 : ('a -> 'b) -> 'a -> ('b, exn) t
  CCSeq.cons : 'a -> 'a t -> 'a t
  CCList.cons' : 'a t -> 'a -> 'a t
  CCListLabels.cons : 'a -> 'a t -> 'a t
  CCListLabels.cons' : 'a t -> 'a -> 'a t
  CCHashtbl.S.add_list : 'a list t -> key -> 'a -> unit
  CCHashtbl.Make.add_list : 'a list t -> key -> 'a -> unit
  CCPair.map_same2 : ('a -> 'b -> 'c) -> 'a * 'a -> 'b * 'b -> 'c * 'c
  CCResult.get_or : ('a, 'b) t -> default:'a -> 'a
  CCFun.tap : ('a -> 'b) -> 'a -> 'a
  CCFun.finally1 : h:(unit -> 'c) -> ('a -> 'b) -> 'a -> 'b
  CCList.sublists_of_len :
  ?last:('a list -> 'a list option) ->
  ?offset:int -> int -> 'a list -> 'a list list
  CCListLabels.sublists_of_len :
  ?last:('a list -> 'a list option) ->
  ?offset:int -> len:int -> 'a list -> 'a list list
  CCOpt.wrap2 :
  ?handler:(exn -> bool) -> ('a -> 'b -> 'c) -> 'a -> 'b -> 'c option
  CCOption.wrap2 :
  ?handler:(exn -> bool) -> ('a -> 'b -> 'c) -> 'a -> 'b -> 'c option
  CCOpt.if_ : ('a -> bool) -> 'a -> 'a option
  CCOption.if_ : ('a -> bool) -> 'a -> 'a option
  CCResult.wrap3 : ('a -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'c -> ('d, exn) t
  CCPair.map : ('a -> 'c) -> ('b -> 'd) -> 'a * 'b -> 'c * 'd
  CCPair.( *** ) : ('a -> 'c) -> ('b -> 'd) -> 'a * 'b -> 'c * 'd
  CCPair.dup_map : ('a -> 'b) -> 'a -> 'a * 'b
  CCMap.S.get_or : key -> 'a t -> default:'a -> 'a
  CCMap.Make.get_or : key -> 'a t -> default:'a -> 'a
  CCHashtbl.S.get_or : 'a t -> key -> default:'a -> 'a
  CCHashtbl.Make.get_or : 'a t -> key -> default:'a -> 'a
  CCFun.(%) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
  CCFun.(%>) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
  CCFun.compose : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
  CCFun.Infix.(%) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
  CCFun.Infix.(%>) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
  CCPair.equal :
  ('a -> 'a -> bool) -> ('b -> 'b -> bool) -> 'a * 'b -> 'a * 'b -> bool
  CCPair.compare :
  ('a -> 'a -> int) -> ('b -> 'b -> int) -> 'a * 'b -> 'a * 'b -> int
  CCArray.lookup_exn : cmp:'a ord -> 'a -> 'a t -> int
  CCArrayLabels.lookup_exn : cmp:'a ord -> key:'a -> 'a t -> int
  CCList.remove_assq : 'a -> ('a * 'b) list -> ('a * 'b) list
  CCListLabels.remove_assq : 'a -> ('a * 'b) list -> ('a * 'b) list
  CCList.Assoc.set :
  eq:('a -> 'a -> bool) -> 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t
  CCListLabels.Assoc.set :
  eq:('a -> 'a -> bool) -> 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t
  CCMap.S.add : key -> 'a -> 'a t -> 'a t
  CCMap.Make.add : key -> 'a -> 'a t -> 'a t
  CCOpt.wrap : ?handler:(exn -> bool) -> ('a -> 'b) -> 'a -> 'b option
  CCOption.wrap : ?handler:(exn -> bool) -> ('a -> 'b) -> 'a -> 'b option
  CCArray.lookup : cmp:'a ord -> 'a -> 'a t -> int option
  CCArrayLabels.lookup : cmp:'a ord -> key:'a -> 'a t -> int option
  CCSeq.unfold : ('b -> ('a * 'b) option) -> 'b -> 'a t
  CCParse.chars_fold :
  f:('acc ->
     char ->
     [ `Consume_and_stop of 'acc
     | `Continue of 'acc
     | `Fail of string
     | `Stop of 'acc ]) ->
  'acc -> ('acc * slice) t
  CCParse.chars_fold_transduce :
  f:('acc ->
     char ->
     [ `Consume_and_stop
     | `Continue of 'acc
     | `Fail of string
     | `Stop
     | `Yield of 'acc * char ]) ->
  'acc -> ('acc * string) t
  CCOpt.map_or : default:'b -> ('a -> 'b) -> 'a t -> 'b
  CCOption.map_or : default:'b -> ('a -> 'b) -> 'a t -> 'b
  CCHashtbl.get_or_add : ('a, 'b) Hashtbl.t -> f:('a -> 'b) -> k:'a -> 'b
  CCHashtbl.Poly.get_or_add : ('a, 'b) Hashtbl.t -> f:('a -> 'b) -> k:'a -> 'b
  Containers.Hashtbl.get_or_add :
  ('a, 'b) Hashtbl.t -> f:('a -> 'b) -> k:'a -> 'b
  ContainersLabels.Hashtbl.get_or_add :
  ('a, 'b) Hashtbl.t -> f:('a -> 'b) -> k:'a -> 'b
  CCResult.map_or : ('a -> 'b) -> ('a, 'c) t -> default:'b -> 'b
  CCPair.map2 :
  ('a1 -> 'b1 -> 'c1) ->
  ('a2 -> 'b2 -> 'c2) -> 'a1 * 'a2 -> 'b1 * 'b2 -> 'c1 * 'c2
  CCString.fold_left : ('a -> char -> 'a) -> 'a -> string -> 'a
  CCString.fold_right : (char -> 'a -> 'a) -> string -> 'a -> 'a
  CCStringLabels.fold_left : f:('a -> char -> 'a) -> init:'a -> string -> 'a
  CCStringLabels.fold_right : f:(char -> 'a -> 'a) -> string -> init:'a -> 'a
  CCString.fold : ('a -> char -> 'a) -> 'a -> t -> 'a
  CCStringLabels.fold : f:('a -> char -> 'a) -> init:'a -> t -> 'a
  CCByte_buffer.fold_left : ('a -> char -> 'a) -> 'a -> t -> 'a
  CCSet.S.fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  CCHeap.S.fold : ('a -> elt -> 'a) -> 'a -> t -> 'a
  CCSet.Make.fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  CCHeap.Make.fold : ('a -> elt -> 'a) -> 'a -> t -> 'a
  CCHeap.Make_from_compare.fold : ('a -> elt -> 'a) -> 'a -> t -> 'a
  CCList.sorted_mem : cmp:('a -> 'a -> int) -> 'a -> 'a list -> bool
  CCListLabels.sorted_mem : cmp:('a -> 'a -> int) -> 'a -> 'a list -> bool
  CCList.mem : ?eq:('a -> 'a -> bool) -> 'a -> 'a t -> bool
  CCArray.mem : ?eq:('a -> 'a -> bool) -> 'a -> 'a t -> bool
  CCListLabels.mem : ?eq:('a -> 'a -> bool) -> 'a -> 'a t -> bool
  CCArrayLabels.mem : ?eq:('a -> 'a -> bool) -> 'a -> 'a t -> bool
  CCPair.(&&&) : ('a -> 'b) -> ('a -> 'c) -> 'a -> 'b * 'c
  CCArray.fold_left : ('a -> 'b -> 'a) -> 'a -> 'b array -> 'a
  CCArray.fold_right : ('b -> 'a -> 'a) -> 'b array -> 'a -> 'a
  CCArrayLabels.fold_left : f:('a -> 'b -> 'a) -> init:'a -> 'b array -> 'a
  CCArrayLabels.fold_right : f:('b -> 'a -> 'a) -> 'b array -> init:'a -> 'a
  CCList.fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
  CCListLabels.fold_left : f:('a -> 'b -> 'a) -> init:'a -> 'b list -> 'a
  CCString.foldi : ('a -> int -> char -> 'a) -> 'a -> t -> 'a
  CCStringLabels.foldi : f:('a -> int -> char -> 'a) -> 'a -> t -> 'a
  CCOpt.fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  CCSeq.fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  CCArray.fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  CCOption.fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  CCSeq.fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  CCList.fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  CCArrayLabels.fold : f:('a -> 'b -> 'a) -> init:'a -> 'b t -> 'a
  CCListLabels.fold_right : f:('a -> 'b -> 'b) -> 'a t -> init:'b -> 'b
  CCArray.bsearch :
  cmp:('a -> 'a -> int) ->
  'a ->
  'a t ->
  [ `All_bigger | `All_lower | `At of int | `Empty | `Just_after of int ]
  CCArrayLabels.bsearch :
  cmp:('a -> 'a -> int) ->
  key:'a ->
  'a t ->
  [ `All_bigger | `All_lower | `At of int | `Empty | `Just_after of int ]
  CCList.assoc : eq:('a -> 'a -> bool) -> 'a -> ('a * 'b) t -> 'b
  CCListLabels.assoc : eq:('a -> 'a -> bool) -> 'a -> ('a * 'b) t -> 'b
  CCList.Assoc.get_exn : eq:('a -> 'a -> bool) -> 'a -> ('a, 'b) t -> 'b
  CCListLabels.Assoc.get_exn : eq:('a -> 'a -> bool) -> 'a -> ('a, 'b) t -> 'b
  CCList.mem_assoc : ?eq:('a -> 'a -> bool) -> 'a -> ('a * 'b) t -> bool
  CCListLabels.mem_assoc : ?eq:('a -> 'a -> bool) -> 'a -> ('a * 'b) t -> bool
  CCVector.member : eq:('a -> 'a -> bool) -> 'a -> ('a, 'b) t -> bool
  CCList.Assoc.mem : ?eq:('a -> 'a -> bool) -> 'a -> ('a, 'b) t -> bool
  CCListLabels.Assoc.mem : ?eq:('a -> 'a -> bool) -> 'a -> ('a, 'b) t -> bool
  CCList.scan_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a list -> 'acc list
  CCListLabels.scan_left :
  f:('acc -> 'a -> 'acc) -> init:'acc -> 'a list -> 'acc list
  CCArray.scan_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc t
  CCArrayLabels.scan_left :
  f:('acc -> 'a -> 'acc) -> init:'acc -> 'a t -> 'acc t
  CCHashtbl.update :
  ('a, 'b) Hashtbl.t -> f:('a -> 'b option -> 'b option) -> k:'a -> unit
  CCHashtbl.Poly.update :
  ('a, 'b) Hashtbl.t -> f:('a -> 'b option -> 'b option) -> k:'a -> unit
  Containers.Hashtbl.update :
  ('a, 'b) Hashtbl.t -> f:('a -> 'b option -> 'b option) -> k:'a -> unit
  ContainersLabels.Hashtbl.update :
  ('a, 'b) Hashtbl.t -> f:('a -> 'b option -> 'b option) -> k:'a -> unit
  CCString.fold2 : ('a -> char -> char -> 'a) -> 'a -> string -> string -> 'a
  CCStringLabels.fold2 :
  f:('a -> char -> char -> 'a) -> init:'a -> string -> string -> 'a
  CCList.foldi : ('b -> int -> 'a -> 'b) -> 'b -> 'a t -> 'b
  CCArray.foldi : ('a -> int -> 'b -> 'a) -> 'a -> 'b t -> 'a
  CCListLabels.foldi : f:('b -> int -> 'a -> 'b) -> init:'b -> 'a t -> 'b
  CCArrayLabels.foldi : f:('a -> int -> 'b -> 'a) -> init:'a -> 'b t -> 'a
  CCMap.S.fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  CCMap.Make.fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  CCHashtbl.S.fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  CCHashtbl.Make.fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  Containers.Hashtbl.S.fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  Containers.Hashtbl.Make.fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  Containers.Hashtbl.SeededS.fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  ContainersLabels.Hashtbl.S.fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  Containers.Hashtbl.MakeSeeded.fold :
  (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  ContainersLabels.Hashtbl.Make.fold :
  (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  ContainersLabels.Hashtbl.SeededS.fold :
  (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  ContainersLabels.Hashtbl.MakeSeeded.fold :
  (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  CCList.fold_while :
  ('a -> 'b -> 'a * [ `Continue | `Stop ]) -> 'a -> 'b t -> 'a
  CCArray.fold_while :
  ('a -> 'b -> 'a * [ `Continue | `Stop ]) -> 'a -> 'b t -> 'a
  CCListLabels.fold_while :
  f:('a -> 'b -> 'a * [ `Continue | `Stop ]) -> init:'a -> 'b t -> 'a
  CCArrayLabels.fold_while :
  f:('a -> 'b -> 'a * [ `Continue | `Stop ]) -> init:'a -> 'b t -> 'a
  CCVector.fold : ('b -> 'a -> 'b) -> 'b -> ('a, 'c) t -> 'b
  CCResult.fold_ok : ('a -> 'b -> 'a) -> 'a -> ('b, 'c) t -> 'a
  CCList.assoc_opt : eq:('a -> 'a -> bool) -> 'a -> ('a * 'b) t -> 'b option
  CCListLabels.assoc_opt :
  eq:('a -> 'a -> bool) -> 'a -> ('a * 'b) t -> 'b option
  CCList.Assoc.get : eq:('a -> 'a -> bool) -> 'a -> ('a, 'b) t -> 'b option
  CCListLabels.Assoc.get :
  eq:('a -> 'a -> bool) -> 'a -> ('a, 'b) t -> 'b option
  CCSeq.Traverse.fold_m : ('b -> 'a -> 'b M.t) -> 'b -> 'a t -> 'b M.t
  CCList.Traverse.fold_m : ('b -> 'a -> 'b M.t) -> 'b -> 'a t -> 'b M.t
  CCListLabels.Traverse.fold_m :
  f:('b -> 'a -> 'b M.t) -> init:'b -> 'a t -> 'b M.t
  Containers.Hashtbl.fold : ('a -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
  ContainersLabels.Hashtbl.fold :
  ('a -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
  CCList.remove : eq:('a -> 'a -> bool) -> key:'a -> 'a t -> 'a t
  CCList.add_nodup : eq:('a -> 'a -> bool) -> 'a -> 'a t -> 'a t
  CCList.remove_one : eq:('a -> 'a -> bool) -> 'a -> 'a t -> 'a t
  CCListLabels.remove : eq:('a -> 'a -> bool) -> key:'a -> 'a t -> 'a t
  CCListLabels.add_nodup : eq:('a -> 'a -> bool) -> 'a -> 'a t -> 'a t
  CCListLabels.remove_one : eq:('a -> 'a -> bool) -> 'a -> 'a t -> 'a t
  CCResult.Traverse.fold_m :
  ('b -> 'a -> 'b M.t) -> 'b -> ('a, 'err) t -> 'b M.t
  CCArray.fold_left_map :
  ('a -> 'b -> 'a * 'c) -> 'a -> 'b array -> 'a * 'c array
  CCArrayLabels.fold_left_map :
  f:('a -> 'b -> 'a * 'c) -> init:'a -> 'b array -> 'a * 'c array
  CCList.fold_left2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b list -> 'c list -> 'a
  CCList.fold_right2 : ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c
  CCListLabels.fold_left2 :
  f:('a -> 'b -> 'c -> 'a) -> init:'a -> 'b list -> 'c list -> 'a
  CCListLabels.fold_right2 :
  f:('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> init:'c -> 'c
  CCList.fold_on_map :
  f:('a -> 'b) -> reduce:('acc -> 'b -> 'acc) -> 'acc -> 'a list -> 'acc
  CCListLabels.fold_on_map :
  f:('a -> 'b) -> reduce:('acc -> 'b -> 'acc) -> init:'acc -> 'a list -> 'acc
  CCSeq.fold2 : ('acc -> 'a -> 'b -> 'acc) -> 'acc -> 'a t -> 'b t -> 'acc
  CCArray.fold2 : ('acc -> 'a -> 'b -> 'acc) -> 'acc -> 'a t -> 'b t -> 'acc
  CCArrayLabels.fold2 :
  f:('acc -> 'a -> 'b -> 'acc) -> init:'acc -> 'a t -> 'b t -> 'acc
  CCList.fold_product : ('c -> 'a -> 'b -> 'c) -> 'c -> 'a t -> 'b t -> 'c
  CCListLabels.fold_product :
  f:('c -> 'a -> 'b -> 'c) -> init:'c -> 'a t -> 'b t -> 'c
  CCList.fold_map :
  ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a list -> 'acc * 'b list
  CCList.fold_left_map : ('a -> 'b -> 'a * 'c) -> 'a -> 'b list -> 'a * 'c list
  CCListLabels.fold_map :
  f:('acc -> 'a -> 'acc * 'b) -> init:'acc -> 'a list -> 'acc * 'b list
  CCListLabels.fold_left_map :
  f:('a -> 'b -> 'a * 'c) -> init:'a -> 'b list -> 'a * 'c list
  CCArray.fold_map : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a t -> 'acc * 'b t
  CCArrayLabels.fold_map :
  f:('acc -> 'a -> 'acc * 'b) -> init:'acc -> 'a t -> 'acc * 'b t
  CCList.sorted_insert :
  cmp:('a -> 'a -> int) -> ?uniq:bool -> 'a -> 'a list -> 'a list
  CCList.sorted_remove :
  cmp:('a -> 'a -> int) -> ?all:bool -> 'a -> 'a list -> 'a list
  CCListLabels.sorted_insert :
  cmp:('a -> 'a -> int) -> ?uniq:bool -> 'a -> 'a list -> 'a list
  CCListLabels.sorted_remove :
  cmp:('a -> 'a -> int) -> ?all:bool -> 'a -> 'a list -> 'a list
  CCResult.fold_iter :
  ('b -> 'a -> ('b, 'err) t) -> 'b -> 'a iter -> ('b, 'err) t
  CCResult.fold_l : ('b -> 'a -> ('b, 'err) t) -> 'b -> 'a list -> ('b, 'err) t
  CCList.foldi2 : ('c -> int -> 'a -> 'b -> 'c) -> 'c -> 'a t -> 'b t -> 'c
  CCListLabels.foldi2 :
  f:('c -> int -> 'a -> 'b -> 'c) -> init:'c -> 'a t -> 'b t -> 'c
  CCList.fold_map_i :
  ('acc -> int -> 'a -> 'acc * 'b) -> 'acc -> 'a list -> 'acc * 'b list
  CCListLabels.fold_map_i :
  f:('acc -> int -> 'a -> 'acc * 'b) -> init:'acc -> 'a list -> 'acc * 'b list
  CCList.fold_flat_map :
  ('acc -> 'a -> 'acc * 'b list) -> 'acc -> 'a list -> 'acc * 'b list
  CCListLabels.fold_flat_map :
  f:('acc -> 'a -> 'acc * 'b list) -> init:'acc -> 'a list -> 'acc * 'b list
  CCList.fold_filter_map :
  ('acc -> 'a -> 'acc * 'b option) -> 'acc -> 'a list -> 'acc * 'b list
  CCListLabels.fold_filter_map :
  f:('acc -> 'a -> 'acc * 'b option) -> init:'acc -> 'a list -> 'acc * 'b list
  CCList.remove_assoc :
  eq:('a -> 'a -> bool) -> 'a -> ('a * 'b) t -> ('a * 'b) t
  CCListLabels.remove_assoc :
  eq:('a -> 'a -> bool) -> 'a -> ('a * 'b) t -> ('a * 'b) t
  CCList.Assoc.remove : eq:('a -> 'a -> bool) -> 'a -> ('a, 'b) t -> ('a, 'b) t
  CCListLabels.Assoc.remove :
  eq:('a -> 'a -> bool) -> 'a -> ('a, 'b) t -> ('a, 'b) t
  CCList.fold_flat_map_i :
  ('acc -> int -> 'a -> 'acc * 'b list) -> 'acc -> 'a list -> 'acc * 'b list
  CCListLabels.fold_flat_map_i :
  f:('acc -> int -> 'a -> 'acc * 'b list) ->
  init:'acc -> 'a list -> 'acc * 'b list
  CCList.fold_filter_map_i :
  ('acc -> int -> 'a -> 'acc * 'b option) -> 'acc -> 'a list -> 'acc * 'b list
  CCListLabels.fold_filter_map_i :
  f:('acc -> int -> 'a -> 'acc * 'b option) ->
  init:'acc -> 'a list -> 'acc * 'b list
  CCList.fold_map2 :
  ('acc -> 'a -> 'b -> 'acc * 'c) ->
  'acc -> 'a list -> 'b list -> 'acc * 'c list
  CCListLabels.fold_map2 :
  f:('acc -> 'a -> 'b -> 'acc * 'c) ->
  init:'acc -> 'a list -> 'b list -> 'acc * 'c list
  CCList.Assoc.update :
  eq:('a -> 'a -> bool) ->
  f:('b option -> 'b option) -> 'a -> ('a, 'b) t -> ('a, 'b) t
  CCListLabels.Assoc.update :
  eq:('a -> 'a -> bool) ->
  f:('b option -> 'b option) -> 'a -> ('a, 'b) t -> ('a, 'b) t
