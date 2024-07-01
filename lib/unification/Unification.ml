(** ACIC Unification

    Follow the algorithm in
    Competing for the AC-unification Race by Boudet (1993)
*)

module Trace = Utils.Tracing
module Logs = (val Logs.(src_log @@ Src.create __MODULE__))

open Syntactic

(* Elementary AC theory *)
let rec solve_tuple_problems env0 =
  Trace.wrap_iter ~__FUNCTION__ ~__FILE__ ~__LINE__ __FUNCTION__ @@
  (* Call the AC solver on all tuple problems at once *)
  let rec pop_all_tuples acc env =
    match Env.pop_tuple env with
    | None -> acc
    | Some pb -> pop_all_tuples (pb :: acc) env
  in
  Trace.wrap_iter ~__FUNCTION__ ~__FILE__ ~__LINE__ "AC.solve"
    (AC.solve env0 @@ pop_all_tuples [] env0)
  |> Iter.flat_map (solve_loop)

(* TODO: here we could expend the return type and check if it can
   be unified with a arrow under the current substitution. To avoid branching *)
(* Elementary Arrow theory *)
and solve_arrow_problem env0 { ArrowTerm.left; right } =
  (*  TODO: we could reuse fresh variable accross the solutions *)
  (* AL -> BL ≡? AR -> BR *)
  Trace.wrap_iter ~__FUNCTION__ ~__FILE__ ~__LINE__ __FUNCTION__
    ~data:(fun () ->
      [
        ("Left", `String (CCFormat.sprintf "%a" ArrowTerm.pp left));
        ("Right", `String (CCFormat.sprintf "%a" ArrowTerm.pp right));
      ])
    @@
  let potentials =
    [
      ( "AL ≡? AR  ∧  BL ≡? BR",
        fun env () ->
          (* AL ≡? AR  ∧  BL ≡? BR *)
          Trace.with_span ~__FUNCTION__ ~__FILE__ ~__LINE__
            "AL ≡? AR  ∧  BL ≡? BR" (fun _sp ->
              Env.push_tuple env left.args right.args;
              insert env left.ret right.ret) );
      ( "AL * αL ≡? AR  ∧  BL ≡? αL -> βL  ∧  βL ≡? BR",
        fun env () ->
          (* AL * αL ≡? AR  ∧
             BL ≡? αL -> βL  ∧
             βL ≡? BR
          *)
          (* We use this reformulation:
             AL * αL ≡? AR  ∧
             BL ≡? αL -> BR
          *)
          (* TODO: Do not create the variable βL, just put  BL ≡? αL -> BR in as equation *)
          Trace.with_span ~__FUNCTION__ ~__FILE__ ~__LINE__
            "AL * αL ≡? AR  ∧  BL ≡? αL -> βL  ∧  βL ≡? BR" (fun _sp ->
              let var_arg_left = Env.gen env in
              Env.push_tuple env
                (ACTerm.add left.args (Type.var (Env.tyenv env) var_arg_left))
                right.args;
              insert env left.ret
                (Type.arrow (Env.tyenv env)
                   (Type.var (Env.tyenv env) var_arg_left)
                   right.ret) ) );
      ( "AL ≡? AR * αR  ∧  αR -> βR ≡? BR   ∧  βR ≡? BL",
        fun env () ->
          (* AL ≡? AR * αR  ∧
             αR -> βR ≡? BR   ∧
             βR ≡? BL
          *)
          (* We use this reformulation:
             AL ≡? AR * αR  ∧
             αR -> BL ≡? BR   ∧
          *)
          Trace.with_span ~__FUNCTION__ ~__FILE__ ~__LINE__
            "AL ≡? AR * αR  ∧  αR -> βR ≡? BR   ∧  βL ≡? BL" (fun _sp ->
              let var_arg_right = Env.gen env in
              Env.push_tuple env left.args
                (ACTerm.add right.args (Type.var (Env.tyenv env) var_arg_right));
              insert env right.ret
                (Type.arrow (Env.tyenv env)
                   (Type.var (Env.tyenv env) var_arg_right)
                   left.ret) ) );
      ( "AL * αL ≡? AR * αR  ∧  BL ≡? αL -> βL  ∧  αR -> βR ≡? BR   ∧  βL ≡? βR",
        fun env () ->
          (* AL * αL ≡? AR * αR  ∧
             BL ≡? αL -> βL  ∧
             αR -> βR ≡? BR   ∧
             βL ≡? βR
          *)
          (* We use this reformulation:
             AL * αL ≡? AR * αR  ∧
             BL ≡? αL -> β  ∧
             αR -> β ≡? BR   ∧
          *)
          Trace.with_span ~__FUNCTION__ ~__FILE__ ~__LINE__
            "AL * αL ≡? AR * αR  ∧  BL ≡? αL -> βL  ∧  αR -> βR ≡? BR   ∧  βL \
             ≡? βR" (fun _sp ->
              let var_arg_left = Env.gen env in
              let var_arg_right = Env.gen env in
              let var_ret = Env.gen env in
              (* TOCHECK *)
              Env.push_tuple env
                (ACTerm.add left.args (Type.var (Env.tyenv env) var_arg_left))
                (ACTerm.add right.args (Type.var (Env.tyenv env) var_arg_right));
              let* () =
                insert env left.ret
                  (Type.arrow (Env.tyenv env)
                     (Type.var (Env.tyenv env) var_arg_left)
                     (Type.var (Env.tyenv env) var_ret))
              in
              insert env
                (Type.arrow (Env.tyenv env)
                   (Type.var (Env.tyenv env) var_arg_right)
                   (Type.var (Env.tyenv env) var_ret))
                right.ret ) );
    ]
  in
  potentials |> Iter.of_list
  |> Iter.flat_map (fun (desc, f) ->
         Trace.wrap_iter ~__FUNCTION__ ~__FILE__ ~__LINE__
           ("Solve_arrow sub problem: " ^ desc )
           ~data:(fun () -> [ ("case", `String desc) ])
           (try_with_solution env0 f ()))
  |> Trace.wrap_arrow_sol

and try_with_solution : type a. _ -> (Env.t -> a -> return) -> a -> _ =
 fun env f sol k ->
  debug (fun m -> m "Trying a solution");
  Trace.with_span ~__FUNCTION__ ~__LINE__ ~__FILE__ __FUNCTION__ (fun _sp ->
  let env =
    Trace.with_span ~__FUNCTION__ ~__FILE__ ~__LINE__ "Env.copy" (fun _sp ->
        Env.copy env)
  in
  match f env sol with
  | Done -> solve_loop env k
  | FailUnif (t1, t2) ->
      debug (fun m ->
          m "@[<v>Conflict between:@;<1 2>@[%a@]@ and@;<1 2>@[%a@]@]@.@."
            Type.pp t1 Type.pp t2)
  | FailedOccurCheck env ->
      debug (fun m -> m "@[<v>Failed occur check in env@;%a" Env.pp env))

and solve_loop env k =
  Trace.with_span ~__FUNCTION__ ~__LINE__ ~__FILE__ __FUNCTION__ (fun _sp ->
  match Env.is_solved env with
  | Some map ->
      debug (fun m -> m "@[<v2>Solved env:@,%a@]@." Env.pp env);
        k map
  | None -> (
      debug (fun m -> m "@[<v2>New env:@,%a@]@." Env.pp env);
      match Env.pop_arrow env with
      | Some pb ->
          Trace.with_span ~__FUNCTION__ ~__FILE__ ~__LINE__ "Solve_arrow"
            (fun _sp -> solve_arrow_problem env pb k)
      | None ->
          Trace.with_span ~__FUNCTION__ ~__FILE__ ~__LINE__ "Solve_tuple"
            (fun _sp -> solve_tuple_problems env k)))

let unifiers (tyenv : Type.Env.t) t1 t2 : Subst.t Iter.t =
  Trace.wrap_iter ~__FUNCTION__ ~__FILE__ ~__LINE__ __FUNCTION__ @@
  let tyenv = Type.Env.restart tyenv in
  let t1 = Type.refresh_variables tyenv t1 in
  let t2 = Type.refresh_variables tyenv t2 in
  Logs.info (fun m -> m "@[<2>type1:@ %a@]" Type.pp t1);
  Logs.info (fun m -> m "@[<2>type2:@ %a@]" Type.pp t2);
  let orig_vars =
    Variable.Set.(
      union (of_iter @@ Type.iter_vars t1) (of_iter @@ Type.iter_vars t2))
  in
  let env0 = Env.make ~tyenv ~orig_vars in
  debug (fun m -> m {|@[<v>Unify:@ "%a"@ "%a"@]|} Type.pp t1 Type.pp t2);
  match insert env0 t1 t2 with
  | Done ->
      debug (fun m -> m "env0: @,%a" Env.pp env0);
      solve_loop env0
  | FailUnif _ | FailedOccurCheck _ -> Iter.empty

(* 1s timeout *)
let timeout = 30.

let iter_with_timeout (it : _ Iter.t) k =
  match Timeout.with_timeout timeout (fun () -> it k) with
  | Ok () -> ()
  | Error () -> ()

let unifiers env t1 t2 = iter_with_timeout @@ unifiers env t1 t2
let unify (env : Type.Env.t) t1 t2 = Iter.min ~lt:Subst.lt @@ unifiers env t1 t2

let unifiable (env : Type.Env.t) t1 t2 =
  not @@ Iter.is_empty @@ unifiers env t1 t2
