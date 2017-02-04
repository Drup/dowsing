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
   @see https://www.lri.fr/~contejea/publis/biblio.html#contejean94ic
*)

(**
  [solve v_type system] returns the minimal solutions [m]s of the linear
   Diophantine system [system] such that
   ∀i, [i > v_type.(0)] implies [m.(i) <= 1].
   if there is a pair [(i,k)] such that
   [v_type.(k) <= i < v_type.(k+1)] and [m.(i) > 0], then
   ∀ [k' <> k], ∀ [i'],  [v_type.(k') <= i' < v_type.(k'+1)]
   implies [m.(i') = 0].
*)
val solve :
  ?debug:bool ->
  int array -> int array array -> int array array
