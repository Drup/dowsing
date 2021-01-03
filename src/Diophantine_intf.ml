(** {2 Abstract representation of Integers} *)

module type S = sig
  module Z = CCInt
  exception Inconsistent_lengths

  module Solution : sig
    type t = Z.t array
    (** Immutable! do not modify *)

    val pp : t CCFormat.printer
  end

  type solution = Solution.t

  module Homogeneous_system : sig
    type t

    val make : Z.t array array -> t
    (** [make eqns] builds the system [eqns = 0], where each [eqn : Z.t array]
        in [eqns] is an array [ [| a1, …, an |] ] representing
        the equation [a1 x1 + a2 x2 + … + an xn = 0].
        @raise Inconsistent_lengths if all equations don't have the same length
    *)

    val len : t -> int
    (** Number of equations *)

    val n_vars : t -> int
    (** Number of variables (i.e. length of each equation) *)

    val eqns : t -> Z.t array array
    (** Get underlying equations.
        {b NOTE}: do not modify! *)

    val solve : ?cut:(solution->bool) -> t -> solution Iter.t
    (** Return an iterator on minimum solutions.
        Any solution to the initial problem is a linear combination of these
        minimal solutions.
        @param cut called on every intermediate tuple traversed by
          the algorithm (in an increasing order). If it returns [true],
          the tuple (and all solutions below it) is dropped.
    *)

    val solve_l : ?cut:(solution->bool) -> t -> solution list
    (** Eager version of {!solve}, returns the (reverse) list of solutions *)

    val pp : t CCFormat.printer
    val pp_sol : solution CCFormat.printer

    (**/**)
    val log_enabled : bool ref
    (**/**)
  end

  val solve : ?cut:(solution->bool) -> Z.t array array -> solution Iter.t
  (** Return an iterator on minimum solutions.
      Any solution to the initial problem is a linear combination of these
      minimal solutions.
      @param cut called on every intermediate tuple traversed by
        the algorithm (in an increasing order). If it returns [true],
        the tuple (and all solutions below it) is dropped.
      @raise Inconsistent_lengths if all equations don't have the same length
  *)

  val solve_l : ?cut:(solution->bool) -> Z.t array array -> solution list
  (** Eager version of {!solve}, returns the (reverse) list of solutions
      @raise Inconsistent_lengths if all equations don't have the same length
  *)

  (* TODO: solve heterogeneous systems, using homogeneous system + first variable
     with coefficient [-rhs_i], being blocked between [0,1] using [cut] *)
end
