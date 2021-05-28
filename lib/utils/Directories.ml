(*
    We should be using the library "directories".
    Unfortunately, compiling with dune gives "inconsistent assumptions over interface Common".
*)

let ( / ) = Filename.concat

module type S = sig
  val home_dir : String.t
  val data_dir : String.t
end

module Linux : S = struct
  let home_dir = Sys.getenv "HOME"
  let data_dir = home_dir / ".local" / "share"
end

module Macos : S = struct
  let home_dir = Sys.getenv "HOME"
  let data_dir = home_dir / "Library" / "Application Support"
end

include (val
  match Config.system with
  | "linux" -> (module Linux : S)
  | "macosx" -> (module Macos : S)
  | _ -> assert false
)
