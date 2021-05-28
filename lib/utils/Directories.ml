(*
    We should be using the library "directories".
    Unfortunately, compiling with dune gives "inconsistent assumptions over interface Common".
*)

let ( / ) = Fpath.( / )

let home_dir =
  Bos.OS.Dir.user ()
  |> CCResult.get_lazy @@ fun (`Msg msg) -> failwith msg

module type S = sig
  val data_dir : Fpath.t
end

module Linux : S = struct
  let data_dir = home_dir / ".local" / "share"
end

module Macos : S = struct
  let data_dir = home_dir / "Library" / "Application Support"
end

include (val
  match Config.system with
  | "linux" -> (module Linux : S)
  | "macosx" -> (module Macos : S)
  | _ -> assert false
)
