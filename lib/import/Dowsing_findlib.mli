(** Utilities functions related to query findlib *)

exception Error of String.t

val find : String.t List.t -> (String.t * Fpath.t) List.t
(** [find pkgs] returns the list of packages [pgks] with their dependencies *)

val find_all : unit -> (String.t * Fpath.t) List.t
(** [find_all ()] returns the list of all available packages *)
