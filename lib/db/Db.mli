
module Entry = Entry
module Content = Content

module Feature = Feature
module TypeIndex = TypeIndex

module DefaultIndex : TypeIndex.S 

type t = {
  idx : DefaultIndex.t ;
  content : Content.t ;
}

val create : Type.Env.t -> Entry.t Iter.t -> t

val find : 
  ?pkgs:Utils.String.HMap.key list ->
  t ->
  Type.Env.t ->
  Type.t -> (Entry.t * (Type.t * Common.Subst.t)) Iter.t
val find_exhaustive : 
  ?pkgs:Utils.String.HMap.key list ->
  t ->
  Type.Env.t ->
  Type.t -> (Entry.t * (Type.t * Common.Subst.t)) Iter.t

val iter : 
  ?pkgs:Utils.String.HMap.key list -> t -> (Entry.t * Type.t) Iter.t
val iter_compatible : 
  ?pkgs:Utils.String.HMap.key list ->
  t -> Type.t -> (Entry.t * Type.t) Iter.t
val load : Fpath.t -> t
val save : t -> Fpath.t -> unit

module Internals : sig
  module Trie = Trie
  module Poset = Poset
  module MatchFeat = MatchFeat
end
