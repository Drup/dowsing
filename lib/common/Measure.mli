module Kind : sig

  type t =
    | VarCount
    | UniqueVarCount
    | NodeCount
    | HeadKind
    | TailSpineVarCount
    | TailSpineNonVarCount
    | SpineVarCount
    | TailLength

  val all : t List.t

  val of_string : String.t -> t
  val to_string : t -> String.t

end

type t = Int.t

val make : Kind.t -> Type.t -> t

module Map : CCMap.S with type key = t
module HMap : CCHashtbl.S with type key = t

val pp : Kind.t -> t Fmt.t [@@ocaml.toplevel_printer]
