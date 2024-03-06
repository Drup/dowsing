module Feature = Feature
module Trie = Trie
module Info = Info
module Poset = Poset
module MatchFeat = MatchFeat
module Doc = Doc

module type S = IndexIntf.S

val make : (module Feature.S) List.t -> (module S)
