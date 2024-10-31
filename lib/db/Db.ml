
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

let create ~with_poset env =
  let content = Content.create () in
  let idx = DefaultIndex.create ~with_poset env in
  { idx; content}

let add env { idx ; content } entry =
  _debug (fun m -> m "Inserting %a" Entry.pp entry);
  let id = Content.add content entry in
  match entry.Entry.desc with
  | Val ty ->
    DefaultIndex.add idx id (Type.of_outcometree env ty)
  | Type { params; manifest } ->
    match manifest with
    | None ->
      ()
    | Some ty ->
      let vars, ty = Type.of_outcometree' env ty in
      let params =
        List.map (fun s ->
            match s with
            | Some s when String.HMap.mem vars s ->
              String.HMap.find vars s
            | _ -> Variable.(Gen.gen Flags.empty env.var_gen)
          ) params
      in
      DefaultIndex.add_type_decl idx id entry.lid params ty

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
