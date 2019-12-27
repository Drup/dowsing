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

   $Id: hullot_bin_tree.ml 4 2004-10-19 14:04:20Z contejea $

 ***************************************************************************)


open Bit_field

module type S =
sig
  type t
  val bin_tree : int -> (t -> bool) -> (t -> bool) -> t list
end

module Make_binary_tree (BF : Bit_field.S) =
struct

  type t = BF.t

  type test = SmallEnough | GreatEnough

  let left_son_ge b_length h node =
    BF.bit_and node (BF.bit_not (BF.bit_nth h b_length))

  let right_son_ge b_length h node =
    BF.bit_and node (BF.bit_not (BF.bit_nth_first (pred h) b_length))

  let left_son_se b_length h node =
    BF.bit_or node (BF.bit_nth_first (pred h) b_length)

  let right_son_se b_length h node =
    BF.bit_or node (BF.bit_nth h b_length)

  let rec bin_tree_rec length great_enough small_enough accu = function
    | (0,node,GreatEnough) ->
	    if great_enough node then node :: accu else accu

    | (0,node,SmallEnough) ->
	    if small_enough node then node :: accu else accu

    | (h,node,GreatEnough) ->
	    if great_enough node
	    then
	      let left =
	        bin_tree_rec length great_enough small_enough accu
	          (pred h,(left_son_ge length (pred h) node),GreatEnough) in
	      let right_and_left =
	        bin_tree_rec length great_enough small_enough left
	          (pred h,(right_son_ge length (pred h) node),SmallEnough) in
        right_and_left
	    else accu
    | (h,node,SmallEnough) ->
	    if small_enough node
	    then
	      let left =
	        bin_tree_rec length great_enough small_enough accu
	          (pred h, (left_son_se length (pred h) node),GreatEnough) in
	      let right_and_left =
	        bin_tree_rec length great_enough small_enough left
	          (pred h, (right_son_se length (pred h) node),SmallEnough) in
        right_and_left
	    else accu

  let bin_tree n great_enough small_enough =
    let root = BF.all_one n in
    let b_length = BF.bit_length root in
    bin_tree_rec b_length great_enough small_enough [] (n, root, GreatEnough)

end

module Small = Make_binary_tree (Small_bit_field)
module Large = Make_binary_tree (Large_bit_field)
