
let array_compare ord a1 a2 =
  let rec aux i =
    if i = Array.length a1
      then if Array.length a1 = Array.length a2 then 0
      else -1
    else if i = Array.length a2
      then 1
      else
        let c = ord a1.(i) a2.(i) in
        if c = 0
          then aux (i+1) else c
  in
  aux 0


module P = struct
  type t = [%import: Longident.t]
  [@@deriving ord]

  let pp ppf x =
    Format.pp_print_string ppf
      (String.concat "." @@ Longident.flatten x)

  let rec of_outcometree = let open Outcometree in function
    | Oide_apply (id1,id2) -> Lapply (of_outcometree id1, of_outcometree id2)
    | Oide_dot (id, s) -> Ldot (of_outcometree id, s)
    | Oide_ident s -> Lident s

  let unit = Lident "unit"
end

module rec Ty : sig
  type t =
    | Var of int
    | Constr of P.t * t array
    | Arrow of NSet.t * t
    | Tuple of NSet.t
    | Unknown of Outcometree.out_type
    | Unit
end = Ty
and Op : Set.OrderedType with type t = Ty.t = struct
  type t = Ty.t

  let to_int = let open Ty in function
    | Var _ -> 0
    | Constr _ -> 1
    | Arrow _ -> 2
    | Tuple _ -> 3
    | Unknown _ -> 4
    | Unit -> 5

  let rec compare lhs rhs = let open Ty in match (lhs, rhs) with
    | Var lhs0, Var rhs0 -> CCInt.compare lhs0 rhs0
    | Constr (c1,arg1), Constr (c2,arg2) ->
      (match P.compare c1 c2 with
       | 0 -> array_compare compare arg1 arg2
       | x -> x)
    | Arrow (arg1,ret1), Arrow (arg2,ret2) ->
      (match compare ret1 ret2 with
       | 0 -> NSet.compare arg1 arg2
       | x -> x)
    | Tuple lhs0, Tuple rhs0 -> NSet.compare lhs0 rhs0
    | Unknown lhs0, Unknown rhs0 ->
      Pervasives.compare lhs0 rhs0
    | Unit, Unit -> 0
    | _ ->
      CCInt.compare (to_int lhs) (to_int rhs)

end
and NSet
  : Custom_set.S with type elt = Ty.t
  = Custom_set.Array(Op)

include Op

let equal x y = compare x y = 0

module Cstr = struct
  open Ty

  let arrow a r = match a, r with
    | Unit, ret -> ret
    | Tuple tup, Arrow (args, ret) ->
      Arrow (NSet.union args tup, ret)
    | arg, Arrow (args, ret) ->
      Arrow (NSet.add arg args, ret)
    | Tuple tup, ret -> Arrow (tup, ret)
    | arg, ret -> Arrow (NSet.singleton arg, ret)


  let tuple f tupl =
    let is_not_unit = function Unit -> false | _ -> true in
    Tuple
      (NSet.of_seq @@
       Sequence.filter is_not_unit @@ Sequence.map f @@
       Sequence.of_list tupl)

end

module Sym = struct

  module Tbl = CCHashtbl.Make(struct
      include String
      let hash = Hashtbl.hash
    end)

  type 'a t = { mutable size : int ; tbl : 'a Tbl.t }
  let create () = { size = 0 ; tbl = Tbl.create 17 }

  let gen ({size ; tbl} as stbl) s =
    match Tbl.get tbl s with
    | Some i -> i
    | None ->
      Tbl.add tbl s size ;
      stbl.size <- size + 1 ;
      size
end

let rec of_outcometree_rec tbl x =
  let open Outcometree in match x with
  | Otyp_arrow (_label, arg, ret) ->
    Cstr.arrow (of_outcometree_rec tbl arg) (of_outcometree_rec tbl ret)

  | Otyp_constr (id, [])
    when P.of_outcometree id = P.unit ->
    Unit

  | Otyp_constr (id, args) ->
    let a =
      Sequence.to_array @@
      Sequence.map (of_outcometree_rec tbl) @@
      Sequence.of_list args
    in
    Ty.Constr (P.of_outcometree id, a)
  | Otyp_tuple typ -> Cstr.tuple (of_outcometree_rec tbl) typ
  | Otyp_var (_, s) ->
    Ty.Var (Sym.gen tbl s)

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

let normalize nb_vars t =
  let init_el = -1 in
  let l = ref 0 in
  let a = Array.make nb_vars init_el in
  let resym i =
    let e = a.(i) in
    if e = init_el then begin
      let new_e = !l in
      a.(i) <- new_e ;
      incr l ;
      new_e
    end else
      e
  in
  let rec aux x = let open Ty in match x with
    | Var i -> Var (resym i)
    | Constr (p,args) -> Constr (p, Array.map aux args)
    | Arrow (args,ret) -> Arrow (NSet.map aux args, ret)
    | Tuple tup -> Tuple (NSet.map aux tup)
    | Unit
    | Unknown _ -> x
  in
  aux t

let of_outcometree x =
  let tbl = Sym.create () in
  let t = of_outcometree_rec tbl x in
  normalize tbl.size t

let rec pp ppf = let open Ty in function
  | Var i -> Format.fprintf ppf {|\%i|} i
  | Constr (p,[||]) -> P.pp ppf p
  | Constr (p,args) ->
    Format.fprintf ppf "@[<2>(%a)@]@ %a"
      (CCFormat.array ~sep:", " pp) args
      P.pp p
  | Arrow (args,ret) ->
    Format.fprintf ppf "@[<2>(%a)@ ->@ %a@]"
      pp_set args
      pp ret
  | Tuple tup ->
    Format.fprintf ppf "@[<2>(%a)@]" pp_set tup
  | Unknown _ -> Format.pp_print_string ppf "_"
  | Unit -> Format.pp_print_string ppf "()"

and pp_set ppf s = CCFormat.seq ~sep:", " pp ppf (NSet.to_seq s)


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
