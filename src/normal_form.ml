
module P = struct
  type t = [%import: Longident.t]
  [@@deriving ord]

  let rec of_outcometree = let open Outcometree in function
    | Oide_apply (id1,id2) -> Lapply (of_outcometree id1, of_outcometree id2)
    | Oide_dot (id, s) -> Ldot (of_outcometree id, s)
    | Oide_ident s -> Lident s
end

module rec Ty : sig
  type t =
    | Var of string
    | Constr of P.t * t list
    | Arrow of NSet.t * t
    | Tuple of NSet.t
    | Unknown of Outcometree.out_type
end = Ty
and Op : Set.OrderedType with type t = Ty.t = struct
  type t = Ty.t

  let to_int = let open Ty in function
    | Var _ -> 0
    | Constr _ -> 1
    | Arrow _ -> 2
    | Tuple _ -> 3
    | Unknown _ -> 4

  let rec compare lhs rhs = let open Ty in match (lhs, rhs) with
    | Var lhs0, Var rhs0 -> String.compare lhs0 rhs0
    | Constr (c1,arg1), Constr (c2,arg2) ->
      (match P.compare c1 c2 with
       | 0 -> CCList.compare compare arg1 arg2
       | x -> x)
    | Arrow (arg1,ret1), Arrow (arg2,ret2) ->
      (match compare ret1 ret2 with
       | 0 -> NSet.compare arg1 arg2
       | x -> x)
    (* | Tuple lhs0, Tuple rhs0 -> NSet.compare lhs0 rhs0 *)
    | Unknown lhs0, Unknown rhs0 ->
      Pervasives.compare lhs0 rhs0
    | _ ->
      Pervasives.compare (to_int lhs) (to_int rhs)

end
and NSet
  : Set.S with type elt = Ty.t
  = Set.Make(Op)

include Op

module Cstr = struct
  open Ty

  let arrow a r = match a, r with
    | Tuple tup, Arrow (args, ret) ->
      Arrow (NSet.union args tup, ret)
    | arg, Arrow (args, ret) ->
      Arrow (NSet.add arg args, ret)
    | Tuple tup, ret -> Arrow (tup, ret)
    | arg, ret -> Arrow (NSet.singleton arg, ret)

  let tuple f tupl =
    let aux set x = match f x with
      | Tuple tup -> NSet.union set tup
      | res -> NSet.add res set
    in
    Tuple (List.fold_left aux NSet.empty tupl)

end

let rec of_outcometree x =
  let open Outcometree in match x with
  | Otyp_arrow (_label, arg, ret) ->
    Cstr.arrow (of_outcometree arg) (of_outcometree ret)
  | Otyp_constr (id, args) ->
    Ty.Constr (P.of_outcometree id, List.map of_outcometree args)
  | Otyp_tuple typ -> Cstr.tuple of_outcometree typ
  | Otyp_var (_, s) -> Ty.Var s

  (* Not handled *)
  | Otyp_object (_,_)
  | Otyp_class (_,_,_)
  | Otyp_variant (_,_,_,_)
  | Otyp_module (_,_,_)
  | Otyp_alias (_,_)
  | Otyp_attribute (_,_)

  (* Not simple types *)
  | Otyp_stuff _
  | Otyp_poly (_,_)
  | Otyp_abstract
  | Otyp_open
  | Otyp_manifest (_,_)
  | Otyp_record _
  | Otyp_sum _
    -> Unknown x

(*
 * Copyright (c) 2016 Gabriel Radanne <drupyog@zoho.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)
