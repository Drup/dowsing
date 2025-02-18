module Stack : sig
  (** Stack of unification equations. *)

  type elt = Type.t * Type.t
  (** Unification equations are pairs of types. *)

  type t
  (** The type of stack. *)

  val empty : t
  (** An empty stack. *)

  val is_empty : t -> bool
  (** [is_empty s] returns true if [s] is empty. *)

  val pop : t -> (elt * t) option
  (** [pop s] returns [None] if [s] is empty and [Some (eq, tl)] where [eq] is
      the last pushed equation on the stack and [tl] is the rest of the stack.
  *)

  val push : t -> Type.t -> Type.t -> t
  (** [push s t1 t2] returns a stack where the equation [(t1, t2)] is added at the
      top of [s].
  *)

  val push_array2 : t -> Type.t array -> Type.t array -> t
  (** [push_array2 s a1 a2] zip [a1] and [a2] to make equatins and push them on
      top of [s].
      @raise Invalid_argument if [a1] and [a2] do not have the same length.
  *)

  val of_list : elt list -> t
  (** Convert a list of equation into a stack. *)

  val pp : t Fmt.t [@@warning "-32"] [@@ocaml.toplevel_printer]
  (** Pretty printer *)
end

type return = Done | FailUnif of Type.t * Type.t | FailedOccurCheck of Env.t
(** The type representing the result of the unification procedure.
    [Done] means that the unification succed
    [FailUnif (t1, t2)] means that the unification failed because of the equality [(t1, t2)]
    [FailedOccurCheck] means that the unification failed because there is a non
        solvable cycle in the environnement.
*)

module Infix : sig
  val ( let* ) : return -> ( unit -> return ) -> return
end

include module type of Infix

val occur_check : Env.t -> return
(** [occur_check env] checks if the substitution defined by [env] contains cycle.
    Because of collapses, if [env] contains cycles, [occur_check] tries to collapse them
    to make [env] cycle free.
*)

val process_stack : Env.t -> Stack.t -> return
(** [process_stack env s] given an unification environment [env] and a stack [s]
    of unification equations, processes each equations.
*)

val insert : Env.t -> Type.t -> Type.t -> return
(** [insert env t1 t2] process the unification equation [t1 ≡ t2]. *)

val insert_var : Env.t -> Variable.t -> Type.t -> return
(** [insert_var env v t] process the unification equation [v ≡ t]. *)

val attach : Env.t -> Variable.t -> Type.t -> return
(** [attach env v t] modify the substitution represented by [env] by associating
    [t] to [v].
*)

val debug : ((('a, Format.formatter, unit, string) format4 -> 'a) -> string) -> unit
