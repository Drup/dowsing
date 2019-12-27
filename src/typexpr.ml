
let poly_compare = compare
module S = Iter

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

(** Path *)
module P = struct
  module M = struct
    type t = Longident.t
    let compare = compare
    let equal = (=)
  end
  include M

  let pp ppf x =
    Format.pp_print_string ppf
      (String.concat "." @@ Longident.flatten x)

  let of_list l =
    let rec aux = function
      | [] -> invalid_arg "Typexpr.P.of_list"
      | [s] -> Longident.Lident s
      | h :: t -> Longident.Ldot (aux t, h)
    in
    aux @@ List.rev l
  let rec to_seq t k = match t with
    | Longident.Lident s -> k s
    | Longident.Ldot (t, s) -> to_seq t k ; k s
    | Longident.Lapply (t1, t2) -> to_seq t1 k ; to_seq t2 k

  let unit = Longident.Lident "unit"
  module Map = CCTrie.Make(struct
      type char_ = string
      let compare = String.compare
      type t = Longident.t
      let of_list = of_list
      let to_seq = to_seq
    end)
  module HMap = CCHashtbl.Make(struct include M let hash = CCHash.poly end)
end

type ('v, 's) skel =
  | Var of 'v
  | Constr of P.t * ('v, 's) skel array
  | Tuple of 's
  | Unknown of int
  | Unit
  | Arrow of 's * ('v, 's) skel

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
      poly_compare lhs0 rhs0
    | Unit, Unit -> 0
    | _ ->
      CCInt.compare (to_int lhs) (to_int rhs)

(** Normal forms *)

module rec Nf
  : Set.OrderedType with type t = (Variables.t, NSet.t) skel = struct
  type t = (Variables.t, NSet.t) skel
  let compare = compare_skel Variables.compare NSet.compare
end
and NSet
  : sig include Custom_set.S with type elt = Nf.t
    val as_array : t -> elt array
  end
  = Custom_set.Array(Nf)

include Nf

let equal x y = Nf.compare x y = 0

(** Non normalized expressions.
    Usually the result of conversion or parsing.
*)
module Raw = struct

  type t = (string option, set) skel
  and set = S of t list

  let arrow a r = match a, r with
    | Unit, ret -> ret
    | Tuple (S tup), Arrow (S args, ret) ->
      Arrow (S (tup @ args), ret)
    | arg, Arrow (S args, ret) ->
      Arrow (S (arg :: args), ret)
    | Tuple tup, ret -> Arrow (tup, ret)
    | arg, ret -> Arrow (S [arg], ret)

  let tuple elts =
    let aux = function
      | Unit -> []
      | Tuple (S tup) -> tup
      | x -> [x]
    in
    Tuple (S (CCList.flat_map aux elts))

  let constr lid args = match args with
    | [||] when lid = P.unit -> Unit
    | _ -> Constr (lid, args)

  let unknown x = Unknown (Hashtbl.hash x)

  let var s = Var s
end

(** Normalization

    Going from the Raw.t to Nf.t
*)

module STbl = CCHashtbl.Make(CCString)

module HC = struct
  type elt = {
    node: Nf.t;
    mutable id: int;
  }

  module W = Weak.Make(struct
    type t = elt
    let equal a b = Nf.compare a.node b.node = 0
    let hash a = Hashtbl.hash a.node
  end)

  type t = {
    tbl: W.t;
    mutable n: int;
  }

  let create size : t = { tbl=W.create size; n=0 }

  let hashcons (self:t) x =
    let x = {node=x; id= -1} in
    let y = W.merge self.tbl x in
    if x==y then (
      x.id <- self.n;
      self.n <- self.n + 1;
    );
    y
end

let normalize ?(gen=Variables.init 0) ?ht x =
  let tbl = STbl.create 17 in
  let sym tbl x =
    match STbl.get tbl x with
    | None ->
      let v = Var (Variables.gen gen) in
      STbl.add tbl x v ; v
    | Some v -> v
  in

  let hashsubst = match ht with
    | None -> fun x -> x
    | Some ht -> fun x -> (HC.hashcons ht x).node
  in
  let rec aux vartbl raw =
    hashsubst @@ aux_int vartbl raw
  and aux_int vartbl raw = match raw with
    | Var (Some s) -> sym vartbl s
    | Var None -> Var (Variables.gen gen)
    | Constr (p,args) ->
      Constr (p, Array.map (aux vartbl) args)
    | Arrow (Raw.S args,ret) ->
      Arrow (auxset vartbl args, aux vartbl ret)
    | Tuple (Raw.S tup) ->
      Tuple (auxset vartbl tup)
    | Unit -> Unit
    | Unknown h -> Unknown h
  and auxset vartbl rawset =
    NSet.of_seq @@ S.map (aux vartbl) @@ S.of_list rawset
  in
  aux tbl x

let rec vars (x: Nf.t) k = match x with
  | Var v -> k v
  | Constr (_,a) -> Array.iter (fun x -> vars x k) a
  | Tuple t -> Iter.flat_map vars (NSet.to_seq t) k
  | Unknown _ -> ()
  | Unit -> ()
  | Arrow (a,r) ->
    Iter.flat_map vars (NSet.to_seq a) k ;
    vars r k

(** Extract the head *)
module Head = struct

  type t =
    | Var
    | Constr of P.t
    | Tuple
    | Other
    | Unit

  let rec get : Nf.t -> t = function
    | Arrow (_, ret) -> get ret
    | Var _i -> Var
    | Constr (p, _) -> Constr p
    | Unknown _ -> Other
    | Tuple _ -> Tuple
    | Unit -> Unit

end

(** Pretty printing *)

let rec pp ppf = function
  | Var i -> Variables.pp ppf i
  | Constr (p,[||]) -> P.pp ppf p
  | Constr (p,args) ->
    Format.fprintf ppf "%a@ %a"
      pp_array args
      P.pp p
  | Arrow (args,ret) ->
    Format.fprintf ppf "@[<2>%a@ ->@ %a@]"
      (NSet.pp pp) args
      pp ret
  | Tuple tup ->
    Format.fprintf ppf "@[<2>%a@]" (NSet.pp pp) tup
  | Unknown _ -> CCFormat.string ppf "_"
  | Unit -> CCFormat.string ppf "unit"

and pp_array ppf = function
  | [||] -> CCFormat.string ppf "()"
  | [|x|] -> pp ppf x
  | a -> Format.fprintf ppf "@[<2>(%a)@]"
      CCFormat.(array ~sep:(return ", ") pp) a

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
