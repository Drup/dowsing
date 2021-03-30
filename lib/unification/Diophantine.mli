
(** Solving Diophantine equations, and systems of Diophantine equations.

    We follow "Linear Diophantine Equations", S. Contejean.
*)

module type S = DiophantineIntf.S

module Make() : S
