module Trace = Utils.Tracing
module Logs = (val Logs.(src_log @@ Src.create __MODULE__))

let _info = Logs.info

let debug f =
  Logs.debug (fun m ->
    let s = f CCFormat.sprintf in
    m "%s" s);
  Trace.message ~data:(fun () ->
    let s = f CCFormat.sprintf in
    [ ("message", `String s) ]) "Debug"

(** {2 A stack of unification pairs} *)
module Stack : sig
  type elt = Type.t * Type.t
  type t

  val empty : t
  val is_empty : t -> bool
  val pop : t -> (elt * t) option
  val push : t -> Type.t -> Type.t -> t
  val push_array2 : t -> Type.t array -> Type.t array -> t
  val of_list : elt list -> t
  val pp : t Fmt.t [@@warning "-32"] [@@ocaml.toplevel_printer]
end = struct
  type elt = Type.t * Type.t
  type t = elt list

  let empty : t = []
  let is_empty = List.is_empty
  let[@inline] pop = function [] -> None | h :: t -> Some (h, t)

  let push l t1 t2 = (t1, t2) :: l
  let push_array2 stack a1 a2 = CCArray.fold2 push stack a1 a2

  let of_list l = l

  let pp_elt ppf (t1, t2) =
    Fmt.pf ppf "%a = %a" Type.pp t1 Type.pp t2

  let pp = Fmt.(vbox (list ~sep:cut pp_elt))
end

type return = Done | FailUnif of Type.t * Type.t | FailedOccurCheck of Env.t

module Infix = struct
  let ( let* ) x1 f = match x1 with Done -> f () | _ -> x1
end

include Infix

(** Checking for cycles *)
let rec occur_check env : return =
  Logs.debug (fun m -> m "Occur check in: %a" Env.pp env);
  match Occur_check.occur_check env with
  | None -> Done
  | Some l ->
      Logs.debug (fun m -> m "Cycle: %a" (CCList.pp Variable.pp) l);
      let rec collapse stack = function
        | u::v::tl -> (
          match Env.representative env u with
          | Env.V _ -> failwith "Impossible"
          | Env.E (_, t) ->
              match t with
              | Type.Var _ | FrozenVar _ | Other _ -> failwith "Impossible"
              | Constr _ | Arrow _ -> None
              | Tuple l ->
                  let first = ref true in
                  let stack =
                    Type.NSet.fold
                      (fun t stack ->
                        match t with
                        | Type.Var v' when !first && Variable.equal v v' ->
                          first := false;
                          stack
                        | t ->
                          Stack.push stack t
                            (Type.tuple (Env.tyenv env) Type.NSet.empty)(*unit*))
                      l stack
                  in
                  collapse stack (v::tl)
                  )
        | _ -> Some stack
      in
      match collapse Stack.empty l with
      | None -> FailedOccurCheck env
      | Some stack ->
        let v = Variable.get_most_general (fun f -> Env.gen f env) l in
        List.iter
          (fun u ->
            if not @@ Variable.equal u v then
            Env.add env u (Type.var (Env.tyenv env) v))
          l;
        Env.remove env v;
        let* () = process_stack env stack in
        occur_check env

(** Main process *)

and process_stack env (stack : Stack.t) : return =
  Trace.with_span ~__FUNCTION__ ~__LINE__ ~__FILE__ __FUNCTION__ (fun _sp ->
  debug (fun m -> m "Process_stack");
  Timeout.check ();
  match Stack.pop stack with
  | Some ((t1, t2), stack) -> insert_rec env stack t1 t2
  | None -> Done)

and insert_rec env stack (t1 : Type.t) (t2 : Type.t) : return =
  Trace.with_span ~__FUNCTION__ ~__LINE__ ~__FILE__ __FUNCTION__ (fun _sp ->
  debug (fun m -> m "Insert_rec %a = %a" Type.pp t1 Type.pp t2);
  match (t1, t2) with
  | _ when t1 == t2 -> process_stack env stack
  (* Decomposition rule
     (s₁,...,sₙ) p ≡ (t₁,...,tₙ) p  --> ∀i, sᵢ ≡ tᵢ
     when p is a type constructor.
  *)
  | Type.Constr (p1, args1), Type.Constr (p2, args2) when LongIdent.equal p1 p2
    ->
      debug (fun m -> m "Constr|Constr");
      assert (Array.length args1 = Array.length args2);
      let stack = Stack.push_array2 stack args1 args2 in
      process_stack env stack
  (* Two arrows, (a₁,...,aₙ) -> r ≡ (a'₁,...,a'ₙ) -> r' *)
  | Type.Arrow (arg1, ret1), Type.Arrow (arg2, ret2) ->
      debug (fun m -> m "Arrow|Arrow");
      (* TODO: if ret1 and ret2 for sure cannot be unified with an arrow, we can decompose
         the problem already *)
      Env.push_arrow env (ArrowTerm.make arg1 ret1)
                         (ArrowTerm.make arg2 ret2);
      process_stack env stack
  (* Two tuples, (s₁,...,sₙ) ≡ (t₁,...,tₙ) *)
  | Tuple s, Tuple t ->
      debug (fun m -> m "Tuple|Tuple");
      (* TODO: we can add check that the tuples can actually be unified otherwise stop *)
      Env.push_tuple env (Type.NSet.as_array s) (Type.NSet.as_array t);
      process_stack env stack
  (* A variable and a type, v ≡ t *)
  | Var v, t | t, Var v ->
      debug (fun m -> m "Var");
      insert_var env stack v t
  (* A tuple and a type, (s₁,...,sₙ) ≡ t
     The tuple need to collapse, we transforme t into a tuple of one elements.
  *)
  | Tuple ts, t | t, Tuple ts when not (Type.NSet.is_empty ts) ->
      debug (fun m -> m "Colapse Tuple");
      Env.push_tuple env (Type.NSet.as_array ts) [| t |];
      process_stack env stack
  (* Clash rule
     Terms are incompatible
  *)
  | ( (Tuple _ | Constr _ | Arrow _ | Other _ | FrozenVar _),
      (Tuple _ | Constr _ | Arrow _ | Other _ | FrozenVar _) ) ->
      debug (fun m -> m "Fail");
      FailUnif (t1, t2))

and insert_var env stack x s =
  Trace.with_span ~__FUNCTION__ ~__LINE__ ~__FILE__ __FUNCTION__ (fun _sp ->
  debug (fun m -> m "Insert_var: %a = %a" Variable.pp x Type.pp s);
  match s with
  | Type.Tuple _ | Type.Constr _ | Type.Arrow _ | Type.Other _
  | Type.FrozenVar _ ->
      quasi_solved env stack x s
  | Type.Var y -> non_proper env stack x y)

(* Quasi solved equation
   'x = (s₁,...sₙ)
   'x = (s₁,...sₙ) p
*)
and quasi_solved env stack x s =
  Trace.with_span ~__FUNCTION__ ~__LINE__ ~__FILE__ __FUNCTION__ (fun _sp ->
  (* Rule representative *)
  match Env.representative env x with
  | V x ->
      let* () = attach env x s in
      process_stack env stack
  (* Rule Merge and AC-Merge *)
  | E (_, t) ->
        insert_rec env stack t s
  | exception Env.FlagsClash (v, t) ->
      FailUnif (Type.var (Env.tyenv env) v, t))

(* Non proper equations
   'x ≡ 'y
   To include a non proper equations, we need to be sure that the dependency created between variable is a DAG. Therefore, we use the Variable.get_most_general, to order the dependency.
*)
and non_proper env stack (x : Variable.t) (y : Variable.t) =
  Trace.with_span ~__FUNCTION__ ~__LINE__ ~__FILE__ __FUNCTION__ (fun _sp ->
  match
    (Env.representative env x,
     Env.representative env y) with
  | (V x' | E (x', _)), (V y' | E (y', _)) when Variable.equal x' y' -> process_stack env stack
  | V x', V y' ->
      let z' = Variable.get_most_general (fun f -> Env.gen f env) [x'; y'] in
      let tz = Type.var (Env.tyenv env) z' in
      let* () = if Variable.equal z' x' then Done else attach env x' tz in
      let* () = if Variable.equal z' y' then Done else attach env y' tz in
      process_stack env stack
  | V x', E (y', t) | E (y', t), V x' ->
      let z' = Variable.get_most_general (fun f -> Env.gen f env) [x'; y'] in
      let tz = Type.var (Env.tyenv env) z' in
      let* () = if Variable.equal z' x' then Done else attach env x' tz in
      let* () = if Variable.equal z' y' then Done else attach env y' tz in
      let* () = attach env z' t in
      process_stack env stack
  | E (x', s), E (y', t) ->
      let z' = Variable.get_most_general (fun f -> Env.gen f env) [x'; y'] in
      let tz = Type.var (Env.tyenv env) z' in
      let* () = if Variable.equal z' x' then Done else attach env x' tz in
      let* () = if Variable.equal z' y' then Done else attach env y' tz in
      let* () = attach env z' s in
      insert_rec env stack s t
  | exception Env.FlagsClash (v, t) ->
      FailUnif (Type.var (Env.tyenv env) v, t))

and attach env v t : return =
  Trace.with_span ~__FUNCTION__ ~__LINE__ ~__FILE__ __FUNCTION__ (fun _sp ->
    if Type.variable_clash v t then FailUnif (Type.var (Env.tyenv env) v, t)
    else (
      Env.add env v t;
      occur_check env))

let insert env t u : return =
  Trace.with_span ~__FUNCTION__ ~__FILE__ ~__LINE__ __FUNCTION__
    ~data:(fun () ->
      [
        ("t", `String (CCFormat.sprintf "%a" Type.pp t));
        ("u", `String (CCFormat.sprintf "%a" Type.pp u));
      ])
    (fun _sp ->
      Logs.debug (fun m ->
          m "@[<v2>Insert@ %a = %a@ in@ %a@]" Type.pp t Type.pp u Env.pp env);
      insert_rec env Stack.empty t u)

let insert_var env x ty : return = insert_var env Stack.empty x ty

