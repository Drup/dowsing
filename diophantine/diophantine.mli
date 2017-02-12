(**************************************************************************)
(*                                                                        *)
(*     The CiME3 tool box for term rewriting                              *)
(*     Copyright (C) 2007                                                 *)
(*                                                                        *)
(*                                                                        *)
(*     Evelyne Contejean                                                  *)
(*     Pierre Courtieu                                                    *)
(*     Julien Forest                                                      *)
(*     Olivier Pons                                                       *)
(*     Xavier Urbain                                                      *)
(*                                                                        *)
(*     CNRS-LRI-Universite Paris Sud XI                                   *)
(*     Cedric-CNAM-ENSIIE                                                 *)
(*                                                                        *)
(*                                                                        *)
(*   This file is distributed under the terms of the CeCILL-C licence     *)
(*                                                                        *)
(**************************************************************************)


(**
   Resolution of systems of linear Diophantine equations

   Extracted from cime v3.0
   @author The CIME Team
   @see <https://www.lri.fr/~contejea/publis/biblio.html#contejean94ic> An efficient algorithm for solving systems of diophantine equations by Evelyne Contejean and Hervé Devie.
*)

(**
  [solve v_type system] returns the minimal solutions [m]s of the linear
   Diophantine system [system] such that:
   - [∀i], [v_type.(0) < i] implies [m.(i) <= 1].
   - if there is a pair [(i,k)] such that
   [v_type.(k) <= i < v_type.(k+1)] and [m.(i) > 0], then
   [∀ k' <> k], [∀i'],  [v_type.(k') <= i' < v_type.(k'+1)]
   implies [m.(i') = 0].

   [i] is constrained if [v_type.(0) < i]
*)
val solve :
  ?debug:bool ->
  int array -> int array array -> int array list
