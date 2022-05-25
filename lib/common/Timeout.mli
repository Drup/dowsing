(** Lightweight timeout mechanisms. *)

(** [check ()] checks if a timeout has been triggered, and raises {!Timeout} if
    necessary. *)
val check : unit -> unit

(** [with_timeout delay fn] runs [fn ()] and cancels it if it runs longer than
    [delay] in seconds.

    It should not be nested.
*) 
val with_timeout : float -> (unit -> 'a) -> ('a, unit) result
