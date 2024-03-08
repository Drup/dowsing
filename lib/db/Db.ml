
module Entry = Entry
module Content = Content

module Feature = Feature
module TypeIndex = TypeIndex

let features = Feature.all
module DefaultIndex : TypeIndex.S = TypeIndex.Make ((val Trie.make features))

type t = {
  idx : DefaultIndex.t ;
  content : Content.t ;
}

let create env entries =
  let content = Content.create () in
  let idx = DefaultIndex.create env in
  Iter.iter (fun entry -> 
      let _ = Content.add content entry in
      ()
    ) entries;
  let type_column =
    Content.iteri content
    |> Iter.map (fun (id, entry) -> id, entry.Entry.ty)
  in
  DefaultIndex.import idx type_column;
  { idx; content}

let find ?pkgs t env ty =
  DefaultIndex.find t.idx env ty
  |> Content.resolve_all ?pkgs t.content

let find_exhaustive ?pkgs t env ty =
  DefaultIndex.find_exhaustive t.idx env ty
  |> Content.resolve_all ?pkgs t.content

let iter ?pkgs t =
  DefaultIndex.iter t.idx
  |> Content.resolve_all ?pkgs t.content

let iter_compatible ?pkgs t ty =
  DefaultIndex.iter_compatible t.idx ty
  |> Content.resolve_all ?pkgs t.content

type serial = DefaultIndex.serial * Content.t
let to_serial t : serial = DefaultIndex.to_serial t.idx, t.content
let of_serial (serialidx, content : serial) =
  { content; idx = DefaultIndex.of_serial serialidx }

let load file : t =
  of_serial @@
  CCIO.with_in (Fpath.to_string file) Marshal.from_channel

let save (t : t) file =
  CCIO.with_out (Fpath.to_string file) @@ fun out ->
  Marshal.to_channel out (to_serial t) []

module Internals = struct
  module Trie = Trie
  module Poset = Poset
  module MatchFeat = MatchFeat
end
