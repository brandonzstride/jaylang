
(* Concolic main *)

open Lang.Ast
open Common

type 'a res = ('a Value.v, Status.Eval.t) result

let deferred_interp expr input_feeder ~max_step =
  let open Effects in

  let rec eval ?(is_stern : bool = false) (expr : Embedded.t) : Value.t m =
    let open Value in
    let k = eval ~is_stern in
    let* () = incr_step ~max_step in
    match expr with
    | EUnit -> return VUnit
    | EInt i -> return @@ VInt (i, Smt.Formula.const_int i)
    | EBool b -> return @@ VBool (b, Smt.Formula.const_bool b)
    | EVar id -> fetch id
    (* inputs *)
    | EPick_i -> get_input Interp_common.Key.Timekey.int_ input_feeder
    | EPick_b -> get_input Interp_common.Key.Timekey.bool_ input_feeder
    (* operations *)
    | EBinop { left ; binop ; right } -> begin
        let* vleft = stern_eval left in
        let* vright = stern_eval right in
        let k f e1 e2 op =
          return @@ f (Smt.Formula.binop op e1 e2)
        in
        let open Smt.Binop in
        let v_int n = fun e -> VInt (n, e) in
        let v_bool b = fun e -> VBool (b, e) in
        match binop, vleft, vright with
        | BPlus        , VInt (n1, e1)  , VInt (n2, e2)              -> k (v_int (n1 + n2)) e1 e2 Plus
        | BMinus       , VInt (n1, e1)  , VInt (n2, e2)              -> k (v_int (n1 - n2)) e1 e2 Minus
        | BTimes       , VInt (n1, e1)  , VInt (n2, e2)              -> k (v_int (n1 * n2)) e1 e2 Times
        | BDivide      , VInt (n1, e1)  , VInt (n2, e2) when n2 <> 0 -> k (v_int (n1 / n2)) e1 e2 Divide
        | BModulus     , VInt (n1, e1)  , VInt (n2, e2) when n2 <> 0 -> k (v_int (n1 mod n2)) e1 e2 Modulus
        | BEqual       , VInt (n1, e1)  , VInt (n2, e2)              -> k (v_bool (n1 = n2)) e1 e2 Equal
        | BEqual       , VBool (b1, e1) , VBool (b2, e2)             -> k (v_bool (b1 = b2)) e1 e2 Equal
        | BNeq         , VInt (n1, e1)  , VInt (n2, e2)              -> k (v_bool (n1 <> n2)) e1 e2 Not_equal
        | BLessThan    , VInt (n1, e1)  , VInt (n2, e2)              -> k (v_bool (n1 < n2)) e1 e2 Less_than
        | BLeq         , VInt (n1, e1)  , VInt (n2, e2)              -> k (v_bool (n1 <= n2)) e1 e2 Less_than_eq
        | BGreaterThan , VInt (n1, e1)  , VInt (n2, e2)              -> k (v_bool (n1 > n2)) e1 e2 Greater_than
        | BGeq         , VInt (n1, e1)  , VInt (n2, e2)              -> k (v_bool (n1 >= n2)) e1 e2 Greater_than_eq
        | BOr          , VBool (b1, e1) , VBool (b2, e2)             -> k (v_bool (b1 || b2)) e1 e2 Or
        | BAnd         , VBool (b1, e1) , VBool (b2, e2)             -> return @@ VBool (b1 && b2, Smt.Formula.and_ [ e1 ; e2 ])
        | _ -> type_mismatch @@ Error_msg.bad_binop vleft binop vright
      end
    | ENot expr -> begin
        let* v = stern_eval expr in
        match v with
        | VBool (b, e_b) -> return @@ VBool (not b, Smt.Formula.not_ e_b)
        | v -> type_mismatch @@ Error_msg.bad_not v
      end
    | EProject { record ; label } -> begin
        let* v = stern_eval record in
        match v with
        | VRecord body | VModule body -> begin
            match RecordLabel.Map.find_opt label body with
            | Some v -> return (Value.cast_up v)
            | None -> type_mismatch @@ Error_msg.project_missing_label label v
          end
        | v -> type_mismatch @@ Error_msg.project_non_record label v
      end
    (* control flow / branches *)
    | EMatch { subject ; patterns  } -> begin
        let* v = stern_eval subject in
        match
          List.find_map (fun (pat, body) ->
              match Value.matches v pat with
              | `Matches -> Some (body, fun x -> x)
              | `Matches_with (v', id) -> Some (body, Env.add id v')
              | `No_match -> None
            ) patterns
        with
        | Some (e, f) -> local f (k e)
        | None -> type_mismatch @@ Error_msg.pattern_not_found patterns v
      end
    | EIf { cond ; true_body ; false_body } -> begin
        let* v = stern_eval cond in
        match v with
        | VBool (b, e_b) ->
          (* let* () = incr_time in *) (* time is not actually needed in practice on branches *)
          let body = if b then true_body else false_body in
          let* () = push_branch (Direction.Bool_direction (b, e_b)) in
          k body
        | v -> type_mismatch @@ Error_msg.cond_non_bool v
      end
    | ECase { subject ; cases ; default } -> begin
        let int_cases = List.map fst cases in
        let* v = stern_eval subject in
        match v with
        | VInt (i, e_i) -> begin
            (* let* () = incr_time in *) (* time is not actually needed in practice on branches *)
            let body_opt = List.find_map (fun (i', body) -> if i = i' then Some body else None) cases in
            match body_opt with
            | Some body ->
              let not_in = List.filter ((<>) i) int_cases in
              let* () = push_branch (Direction.Int_direction { dir = Case_int i ; formula = e_i ; not_in }) in
              k body
            | None ->
              let* () = push_branch (Direction.Int_direction { dir = Case_default ; formula = e_i ; not_in = int_cases }) in
              k default
          end
        | v -> type_mismatch @@ Error_msg.case_non_int v
      end
    (* closures and applications *)
    | EFunction { param ; body } ->
      let* env = read in
      return (VFunClosure { param ; closure = { body ; env }})
    | ELet { var ; defn ; body } ->
      let* v = eval defn in
      local (Env.add var v) (eval body)
    | EAppl { func ; arg } -> begin
        let* v = stern_eval func in
        match v with
        | VFunClosure { param ; closure } ->
          let* v = eval arg in
          local (fun _ -> Env.add param v closure.env) (k closure.body)
        | v -> type_mismatch @@ Error_msg.bad_appl v
      end
    (* modules, records, and variants  *)
    | ERecord label_map ->
      let* value_record_body =
        RecordLabel.Map.fold (fun key e acc_m ->
            let* acc = acc_m in
            let* v = eval e in
            return @@ RecordLabel.Map.add key v acc
          ) label_map (return RecordLabel.Map.empty)
      in
      return @@ VRecord value_record_body
    | EVariant { label ; payload } ->
      let* v = eval payload in
      return (VVariant { label ; payload = v })
    | EModule stmt_ls ->
      let* module_body =
        let rec fold_stmts acc_m = function
          | [] -> acc_m
          | Lang.Ast.Expr.SUntyped { var ; defn } :: tl ->
            let* acc = acc_m in
            let* v = eval defn in
            local (Env.add var v) (
              fold_stmts (return @@ RecordLabel.Map.add (Lang.Ast.RecordLabel.RecordLabel var) v acc) tl
            )
        in
        fold_stmts (return Lang.Ast.RecordLabel.Map.empty) stmt_ls
      in
      return @@ VModule module_body
    | EUntouchable e ->
      let* v = eval e in
      return (VUntouchable v)
    (* deferral *)
    | EDefer body -> if is_stern then k body else defer body
    (* termination *)
    | EVanish () -> vanish
    | EAbort msg -> abort msg

  (*
    This stern eval may error monadically so that we get propagation of
    errors in relaxed eval.

    We have this because in practice, we don't want to clean up many deferred
    proofs on stern evals. This one is very direct, and it does (most of the time)
    the minimal amount of work to get to whnf.
  *)
  and stern_eval (expr : Embedded.t) : Value.whnf m =
    match expr with
    | EDefer body -> stern_eval body (* When sternly evaluating a deferred thing, we can just directly eval the thing *)
    | _ ->
      let* v = eval ~is_stern:true expr in
      let* () = incr_step ~max_step in
      let* () = incr_n_stern_steps in
      Value.split v
        ~symb:(fun ((VSymbol t) as sym) ->
            let* s = get in
            match Time_map.find_opt t s.State.symbol_map with
            | Some v -> return v
            | None -> map_deferred_proof sym stern_eval
          )
        ~whnf:(fun v ->
            let* () = optionally_map_some_deferred_proof stern_eval in
            return v
          )

  (*
    This does not monadically error.
    Any error is packed into result, so there is no implicit error propagation.
    This is helpful because we don't want errors propagating and messing with
    the cleanup in our stern semantics, where we must evaluate everything and
    keep the smallest error.
  *)
  and clean_up_deferred (final : Value.ok res) : Value.ok res s =
    let* s = get in
    match Time_map.choose_opt s.State.pending_proofs with
    | None -> return final (* done! can finish with how we're told to finish *)
    | Some (t, _) -> (* some cleanup to do, so do it, and then keep looping after that *)
      (* Do some cleanup by running this timestamp *)
      handle_error (map_deferred_proof (VSymbol t) stern_eval)
        (fun _ -> clean_up_deferred final) (* ignore value of deferred proof because we already have the final value *)
        (fun e -> clean_up_deferred (Error e)) (* deferred proof errored, so it must be the smaller error, so keep it and continue *)
  in

  let begin_stern_loop (expr : Embedded.t) : Value.ok res s =
    let* r =
      handle_error (stern_eval expr)
        (fun v -> return (Ok v))
        (fun e -> return (Error e))
    in
    clean_up_deferred r
  in

  run (begin_stern_loop expr)

let deferred_eval expr input_feeder ~max_step =
  match deferred_interp expr input_feeder ~max_step with
  | Ok _, state, _ -> Status.Finished, state.path
  | Error e, state, _ -> e, state.path
