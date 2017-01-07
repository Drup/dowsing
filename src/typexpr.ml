
module S = Sequence

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

module Hash
  : sig
    type t = (* private *) int
    val make : 'a -> t
  end
= struct
  type t = int
  let make = Hashtbl.hash
end

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

type ('v, 's) skel =
  | Var of 'v
  | Constr of P.t * ('v, 's) skel array
  | Arrow of 's * ('v, 's) skel
  | Tuple of 's
  | Unknown of Hash.t
  | Unit

let to_int = function
  | Var _ -> 0
  | Constr _ -> 1
  | Arrow _ -> 2
  | Tuple _ -> 3
  | Unknown _ -> 4
  | Unit -> 5

let rec compare_skel cmp_var cmp_set lhs rhs =
  if lhs == rhs then 0
  else match (lhs, rhs) with
    | Var lhs0, Var rhs0 -> cmp_var lhs0 rhs0
    | Constr (c1,arg1), Constr (c2,arg2) ->
      (match P.compare c1 c2 with
       | 0 ->
         array_compare (compare_skel cmp_var cmp_set) arg1 arg2
       | x -> x)
    | Arrow (arg1,ret1), Arrow (arg2,ret2) ->
      (match compare_skel cmp_var cmp_set ret1 ret2 with
       | 0 -> cmp_set arg1 arg2
       | x -> x)
    | Tuple lhs0, Tuple rhs0 -> cmp_set lhs0 rhs0
    | Unknown lhs0, Unknown rhs0 ->
      Pervasives.compare lhs0 rhs0
    | Unit, Unit -> 0
    | _ ->
      CCInt.compare (to_int lhs) (to_int rhs)

(** Normal forms *)

module rec Nf
  : Set.OrderedType with type t = (int, NSet.t) skel = struct
  type t = (int, NSet.t) skel
  let compare = compare_skel CCInt.compare NSet.compare
end
and NSet
  : Custom_set.S with type elt = Nf.t
  = Custom_set.Array(Nf)

include Nf

let equal x y = compare x y = 0

(** Non normalized expressions.
    Usually the result of conversion or parsing.
*)
module Raw = struct

  module S = Sequence
  open S.Infix

  type t = (string, set) skel
  and set = S of t S.t

  (* let flatten_tup = *)
  (*   let rec aux = function *)
  (*     | Unit -> S.empty *)
  (*     | Tuple (S tup) -> S.flat_map aux tup *)
  (*     | x -> S.singleton x *)
  (*   in *)
  (*   S.flat_map aux *)

  (* let rec flatten_arrow args r = match r with *)
  (*   | Arrow (S args', ret) -> *)
  (*     flatten_arrow (args <+> flatten_tup args') ret *)
  (*   | ret -> args, ret *)

  let arrow a r = match a, r with
    | Unit, ret -> ret
    | Tuple (S tup), Arrow (S args, ret) ->
      Arrow (S (tup <+> args), ret)
    | arg, Arrow (S args, ret) ->
      Arrow (S (S.cons arg args), ret)
    | Tuple tup, ret -> Arrow (tup, ret)
    | arg, ret -> Arrow (S (S.singleton arg), ret)

  let tuple elts =
    let aux = function
      | Unit -> S.empty
      | Tuple (S tup) -> tup
      | x -> S.singleton x
    in
    Tuple (S (Sequence.flat_map aux elts))

  let constr lid args = match args with
    | [||] when lid = P.unit -> Unit
    | _ -> Constr (lid, args)

  let unknown x = Unknown (Hash.make x)

  let var vars s =
    vars := s :: !vars ;
    Var s

  type varset = string list ref
  let varset () = ref []
end

(** Normalization

    Going from the Raw.t to Nf.t
*)

module STbl = CCHashtbl.Make(struct
    include String
    let hash (x:string) = Hashtbl.hash x
  end)

module HC = Hashcons.Make(struct
    type t = Nf.t
    let equal = equal
    let hash = Hashtbl.hash
  end)

(** Take as argument a list of variable present in the expression. *)
let normalize ?ht vars x =
  let hashsubst = match ht with
    | None -> fun x -> x
    | Some ht -> fun x -> (HC.hashcons ht x).node
  in
  let rec aux ht vartbl raw =
    hashsubst @@ aux_int ht vartbl raw
  and aux_int ht vartbl raw = match raw with
    | Var s -> STbl.find vartbl s
    | Constr (p,args) ->
      Constr (p, Array.map (aux ht vartbl) args)
    | Arrow (Raw.S args,ret) ->
      Arrow (auxset ht vartbl args, aux ht vartbl ret)
    | Tuple (Raw.S tup) ->
      Tuple (auxset ht vartbl tup)
    | Unit -> Unit
    | Unknown h -> Unknown h
  and auxset ht vartbl rawset =
    NSet.of_seq @@ S.map (aux ht vartbl) rawset
  in
  let tbl = STbl.create 17 in
  let add tbl i x =
    if not (STbl.mem tbl x) then STbl.add tbl x (Var i)
  in
  List.iteri (add tbl) !vars ;
  aux ht tbl x


(** Pretty printing *)

let rec pp ppf = function
  | Var i -> Format.fprintf ppf {|\%i|} i
  | Constr (p,[||]) -> P.pp ppf p
  | Constr (p,args) ->
    Format.fprintf ppf "@[<2>(%a)@]@ %a"
      (CCFormat.array ~sep:", " pp) args
      P.pp p
  | Arrow (args,ret) ->
    Format.fprintf ppf "@[<2>%a@ ->@ %a@]"
      (NSet.pp pp) args
      pp ret
  | Tuple tup ->
    Format.fprintf ppf "@[<2>%a@]" (NSet.pp pp) tup
  | Unknown _ -> Format.pp_print_string ppf "_"
  | Unit -> Format.pp_print_string ppf "unit"


(** Import from Outcometree *)

let rec of_outcometree_rec vars x : Raw.t =
  let open Outcometree in match x with
  | Otyp_arrow (_label, arg, ret) ->
    let arg' = of_outcometree_rec vars arg in
    let ret' = of_outcometree_rec vars ret in
    Raw.arrow arg' ret'
  | Otyp_constr (id, args) ->
    let a =
      S.to_array @@
      S.map (of_outcometree_rec vars) @@
      S.of_list args
    in
    Raw.constr (P.of_outcometree id) a
  | Otyp_tuple tup ->
    let tup' =
      Sequence.map (of_outcometree_rec vars) @@ Sequence.of_list tup
    in
    Raw.tuple tup'
  | Otyp_var (_, s) ->
    Raw.var vars s

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
    -> Raw.unknown x

let of_outcometree ?ht x =
  let vars = Raw.varset () in
  let t = of_outcometree_rec vars x in
  normalize ?ht vars t


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
