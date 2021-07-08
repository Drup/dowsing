module Feature = Feature
module Trie = Trie
module Info = Info
module Cell = Cell

module type S = IndexIntf.S
val make : (module Feature.S) List.t -> (module S)
