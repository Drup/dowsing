module type S = TypeIndexIntf.S

module Make (T : Trie.NODE) : S
