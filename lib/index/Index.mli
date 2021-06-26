module Feature = Feature
module Trie = Trie
module Info = Info
module Cell = Cell

module Make (Trie : Trie.NODE) : IndexIntf.S
val make_ : (module Feature.S) List.t -> (module IndexIntf.S)

include IndexIntf.S
