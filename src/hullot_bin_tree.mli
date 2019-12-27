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

(**************************************************************************

   Binary trees a la Hullot.

   $Id: hullot_bin_tree.mli 4 2004-10-19 14:04:20Z contejea $

 ***************************************************************************)

module type S =
sig
  type t (* encodes a characteristic function for a finite set *)

  (* [bin_tree n great_enough small_enough] returns the list of all subsets
     of the canonical finite set of cardinality [n], such that each subset
     has sucessfully passed the tests [small_enough] and [great_enough].
     As indicated by their names, [small_enough] and [great_enough] are
     assumed to satisfy the following properties:
     \begin{itemize}
     \item if [small_enough s] holds, then for all subset [s'] of [s],
     [small_enough s'] holds,
     \item if [great_enough s] holds, then for all superset [s'] of [s],
     [great_enough s'] holds.
     \end{itemize} *)
  val bin_tree : int -> (t -> bool) -> (t -> bool) -> t list
end

module Small : S with type t = int

module Large : S with type t = int array
