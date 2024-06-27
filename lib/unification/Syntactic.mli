type return = Done | FailUnif of Type.t * Type.t | FailedOccurCheck of Env.t

val ( let* ) : return -> ( unit -> return ) -> return

val insert : Env.t -> Type.t -> Type.t -> return

val insert_var : Env.t -> Variable.t -> Type.t -> return

val debug : ((('a, Format.formatter, unit, string) format4 -> 'a) -> string) -> unit
