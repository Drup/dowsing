
(** Solving Diophantine equations, and systems of Diophantine equations.

    We follow "Linear Diophantine Equations", S. Contejean.
*)

module type S = Diophantine_intf.S

module Make() : S
