module Stack : sig
  type elt = Type.t * Type.t
  type t

  val empty : t
  val is_empty : t -> bool
  val pop : t -> (elt * t) option
  val push : t -> Type.t -> Type.t -> t
  val push_array2 : t -> Type.t array -> Type.t array -> t
  val of_list : elt list -> t
  val pp : t Fmt.t [@@warning "-32"] [@@ocaml.toplevel_printer]
end

type return = Done | FailUnif of Type.t * Type.t | FailedOccurCheck of Env.t

module Infix : sig
  val ( let* ) : return -> ( unit -> return ) -> return
end

val ( let* ) : return -> ( unit -> return ) -> return

val occur_check : Env.t -> return

val process_stack : Env.t -> Stack.t -> return

val insert : Env.t -> Type.t -> Type.t -> return

val insert_var : Env.t -> Variable.t -> Type.t -> return

val attach : bool -> Env.t -> Variable.t -> Type.t -> return

val debug : ((('a, Format.formatter, unit, string) format4 -> 'a) -> string) -> unit
