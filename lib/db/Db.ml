
module Entry = Entry
module Content = Content

module Feature = Feature
module TypeIndex = TypeIndex

module DefaultIndex = TypeIndex.Make (Content.ID)

module Logs = (val Logs.(src_log @@ Src.create __MODULE__))

let _info = Logs.info
let _debug = Logs.debug

type t = {
  idx : DefaultIndex.t ;
  content : Content.t ;
}

let create ~with_poset env entries =
  let content = Content.create () in
  let idx = DefaultIndex.create ~with_poset env in
  Iter.iter (fun entry -> 
      let _ = Content.add content entry in
      ()
    ) entries;
  _info (fun m ->
      m "@[%i @ types to insert in the index @]"
        (Content.size content));
  Content.iteri content
  |> Iter.map
    (fun (id, entry) -> id, Type.of_outcometree env entry.Entry.ty)
  |>  Iter.iter
    (fun (id, ty) -> DefaultIndex.add idx id ty)
  ;
  { idx; content}

let find ?pkgs ?filter t env ty =
  DefaultIndex.find ?filter t.idx env ty
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
