
module Entry = Entry
module Content = Content

module Feature = Feature
module TypeIndex = TypeIndex

module DefaultIndex : TypeIndex.S 

type t = {
  idx : DefaultIndex.t ;
  content : Content.t ;
}

val create : 
  with_poset:bool ->
  Type.Env.t -> t

val add : Type.Env.t -> t -> Entry.t -> unit

val find : 
  ?pkgs:Utils.String.HMap.key list ->
  ?filter:[ `Default | `None | `OnlyTrie] ->
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
