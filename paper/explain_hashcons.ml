type t =
| Leaf of string
| Node of t * t

let tbl : (t, t) Hashtbl.t = Hashtbl.create 17

let rec hashcons t =
  match Hashtbl.find_opt tbl t with
  | Some t' -> t'
  | None ->
    let t' = match t with
      | Leaf _ -> t
      | Node (t1, t2) -> Node (hashcons t1, hashcons t2)
    in
    Hashtbl.add tbl t' t';
    t'

(*
       B
      /  \ 
*X = A    ...
   /   \
  T1   T2 

       C
      /  \ 
     *X   ...


*)
