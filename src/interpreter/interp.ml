(**
   Module [Interp].

   This module interprets any language defined in this system.
   It uses GADTs and type constraints to define the interpreter
   for all languages in one function.
*)

open Lang.Ast
open Lang.Ast.Expr
open Lang.Ast_tools.Exceptions

module V = Lang.Value.Make (Lang.Value.Map_store) (Lang.Value.Lazy_cell) (Utils.Identity)
open V

module Feeder = Interp_common.Input_feeder

module Input_log = struct
  include Utils.Builder.List_builder (struct
    type t = Interp_common.Input.t * Interp_common.Timestamp.t
  end)

  let to_sequence : t -> Interp_common.Input.t list = fun t ->
    List.rev_map fst t

  let to_time_feeder : t -> Interp_common.Timestamp.t Feeder.t =
    fun t ->
    let module Map = Interp_common.Timestamp.Map in
    let m_ints, m_bools =
      List.fold_left (fun (mi, mb) (input, t) ->
          match input with
          | Interp_common.Input.I i -> (Map.add t i mi, mb)
          | Interp_common.Input.B b -> (mi, Map.add t b mb)
        ) (Map.empty, Map.empty) t
    in
    let get : type a. a Interp_common.Key.Timekey.t -> a = fun key ->
      let a_opt : a option =
        match key with
        | I k -> Map.find_opt k m_ints
        | B k -> Map.find_opt k m_bools
      in
      Option.value a_opt ~default:(Interp_common.Input_feeder.zero.get key)
    in
    { get }
end

module type ENV = sig
  type value
  type t
  val empty : t
  val fetch : Lang.Ast.Ident.t -> t -> value option
end

module CPS_Error_M (Env : ENV) = struct
  module State = struct
    type t =
      { time : Interp_common.Timestamp.t
      ; log : Input_log.t
      ; n_inputs : int }

    let empty : t =
      { time = Interp_common.Timestamp.initial
      ; log = Input_log.empty
      ; n_inputs = 0 }
  end

  let max_step : Interp_common.Step.t = Step (1 lsl 20) (* about a million steps *)

  module Err = struct
    (* Not putting state in the error because it's returned anyways *)
    type t = unit Interp_common.Errors.Runtime.t
  end

  include Interp_common.Monad
  include Interp_common.Monad.Specialize (State) (Env) (Err)

  let incr_step : unit m =
    incr_step ~max_step ~fail_on_max:(fun state -> `XReach_max_step (), state)

  let incr_time : unit m =
    modify (fun (s : State.t) ->
      { s with time = Interp_common.Timestamp.increment s.time }
    )

  let push_time : unit m =
    modify (fun (s : State.t) ->
      { s with time = Interp_common.Timestamp.push s.time }
    )

  let abort (type a) (msg : string) : a m =
    escape @@ `XAbort { Interp_common.Errors.msg ; body = () }

  (* unit is needed to surmount the value restriction *)
  let vanish (type a) (() : unit) : a m =
    let* (s : State.t) = get in
    Format.printf "Vanishing at time %s\n" (Interp_common.Timestamp.to_string s.time);
    escape @@ `XVanish ()

  let type_mismatch (type a) (() : unit) : a m =
    escape @@ `XType_mismatch { Interp_common.Errors.msg =
      "No type mismatch message today, sorry" ; body = () }

  let unbound_variable (type a) (id : Ident.t) : a m =
    escape @@ `XUnbound_variable (id, ())

  let list_map (f : 'a -> 'b m) (ls : 'a list) : 'b list m =
    List.fold_right (fun a acc_m ->
      let* acc = acc_m in
      let* b = f a in
      return (b :: acc)
    ) ls (return [])

  let using_env (f : Env.t -> 'a) : 'a m =
    let* env = read in
    return (f env)

  let log_input (input : Interp_common.Input.t) : unit m =
    let* { State.time ; _ } = get in
    modify (fun (s : State.t) -> { s with n_inputs = s.n_inputs + 1 ;
      log = Input_log.cons (input, time) s.log
    })

  let n_inputs : int m =
    let* s = get in
    return s.State.n_inputs

  let with_time_snapback (x : 'a m) : 'a m =
    let* s = get in
    let* a = x in (* runs x with original time *)
    let* () = modify (fun s' -> { s' with State.time = s.State.time }) in
    return a

  let get_input (type a) (fkey : int -> a Interp_common.Key.Indexkey.t)
      (pack : a -> Interp_common.Input.t) (feeder : int Feeder.t) : a m =
    let* n = n_inputs in
    let a = feeder.get (fkey n) in
    let* () = log_input (pack a) in
    let* () = incr_time in
    return a
end

let eval_exp (type a) (e : a Expr.t) (feeder : int Feeder.t) : a V.t * Input_log.t =
  let module E = struct
    type value = a V.t
    type t = a Env.t
    let empty : t = Env.empty
    let fetch = Env.fetch
  end in
  let open CPS_Error_M (E) in
  let rec eval (e : a Expr.t) : a V.t m =
    let* () = incr_step in
    match e with
    (* direct values *)
    | EUnit -> return VUnit
    | EInt i -> return (VInt i)
    | EBool b -> return (VBool b)
    | EVar id ->
        let* env = read in
        begin match Env.fetch id env with
        | None -> unbound_variable id
        | Some v -> return v
        end
    | ETypeInt -> return VTypeInt
    | ETypeBool -> return VTypeBool
    | ETypeTop -> return VTypeTop
    | ETypeBottom -> return VTypeBottom
    | ETypeUnit -> return VTypeUnit
    | EType -> return VType
    | EAbort msg -> abort msg
    | EVanish () -> vanish ()
    | EFunction { param ; body } ->
      using_env @@ fun env ->
      VFunClosure { param ; closure = { body ; env = lazy env } }
    | EMultiArgFunction { params ; body } ->
      using_env @@ fun env ->
      VMultiArgFunClosure { params ; closure = { body ; env = lazy env } }
    (* inputs *)
    | EInput | EPick_i ->
      let* i = get_input Interp_common.Key.Indexkey.int_ (fun i -> Interp_common.Input.I i) feeder in
      return (VInt i)
    | EPick_b ->
      let* b = get_input Interp_common.Key.Indexkey.bool_ (fun b -> Interp_common.Input.B b) feeder in
      return (VBool b)
    | EAbstractType ->
      let* i = get_input Interp_common.Key.Indexkey.int_ (fun i -> Interp_common.Input.I i) feeder in
      return (VAbstractType i)
    (* deferred expressions *)
    | EDefer e -> (* eagerly evaluate, but still track time correctly *)
      let* v = with_time_snapback (
          let* () = push_time in
          eval e
        ) in
      let* () = incr_time in
      return v
    (* simple propogation *)
    | EVariant { label ; payload } ->
      let* payload = eval payload in
      return (VVariant { label ; payload = payload })
    | EList e_list ->
      let* ls = list_map eval e_list in
      return (VList ls)
    | ETypeList -> return VTypeListFun
    | ETypeSingle -> return VTypeSingleFun
    | ETypeFun { domain ; codomain ; dep } -> begin
        let* domain = eval domain in
        match dep with
        | `Binding binding ->
          using_env @@ fun env ->
          VTypeDepFun { binding ; domain ; codomain = { body = codomain ; env = lazy env } }
        | `No ->
          let* codomain = eval codomain in
          return (VTypeFun { domain ; codomain })
      end
    | ETypeRefinement { tau ; predicate } ->
      let* tau = eval tau in
      let* predicate = eval predicate in
      return (VTypeRefinement { tau ; predicate })
    | ETypeIntersect e_ls ->
      let* ls = list_map (fun (label, tau, tau') ->
          let* vtau = eval tau in
          let* vtau' = eval tau' in
          return (label, vtau, vtau')
        ) e_ls
      in
      return (VTypeIntersect ls)
    | ETypeVariant e_ls ->
      let* ls = list_map (fun (label, tau) ->
          let* vtau = eval tau in
          return (label, vtau)
        ) e_ls
      in
      return (VTypeVariant ls)
    | ERecord record_body ->
      let* new_record = eval_record_body record_body in
      return (VRecord new_record)
    | EModule stmts -> eval_stmt_list stmts
    | ETypeRecord record_type_body ->
      let* new_record = eval_record_body record_type_body in
      return (VTypeRecord new_record)
    | ETypeModule e_ls ->
      using_env @@ fun env ->
      VTypeModule (List.map (fun (label, tau) -> label, { body = tau ; env = lazy env }) e_ls)
    | EGen e ->
      let* _ : a V.t = eval e in
      return VAbort
    | EUntouchable e ->
      let* v = eval e in
      return (VUntouchable v)
    (* bindings *)
    | EAppl { func ; arg } -> begin
        let* vfunc = eval func in
        let* arg = eval arg in
        match vfunc with
        | VFunClosure { param ; closure = { body ; env = lazy env } } ->
          local (fun _ -> Env.add param arg env) (eval body)
        | VMultiArgFunClosure { params ; closure = { body ; env = lazy env }} -> begin
            match params with
            | [] -> type_mismatch ()
            | [ param ] ->
              local (fun _ -> Env.add param arg env) (eval body)
            | param :: params ->
              local (fun _ -> Env.add param arg env) (eval (EMultiArgFunction { params ; body }))
          end
        | VTypeSingleFun -> return (VTypeSingle arg)
        | VTypeListFun -> return (VTypeList arg)
        | _ -> type_mismatch ()
      end
    | ELet { var ; defn ; body } -> eval_let var ~defn ~body
    | ELetTyped { typed_var = { var ; _ } ; defn ; body ; _ } -> eval_let var ~defn ~body
    | ETypeMu { var ; params ; body } ->
      let* env = read in
      let rec rec_env = lazy (
        Env.add var (VTypeMu { var ; params ; closure = { body ; env = rec_env } }) env
      )
      in
      local (fun _ -> Lazy.force rec_env) (eval (Lang.Ast_tools.Utils.abstract_over_ids params (EVar var)))
    (* operations *)
    | EListCons (e_hd, e_tl) -> begin
        let* hd = eval e_hd in
        let* tl = eval e_tl in
        match tl with
        | VList ls -> return (VList (hd :: ls))
        | _ -> type_mismatch ()
      end
    | EBinop { left ; binop ; right } -> begin
        let* a = eval left in
        let* b = eval right in
        match binop, a, b with
        | BPlus, VInt n1, VInt n2                 -> return (VInt (n1 + n2))
        | BMinus, VInt n1, VInt n2                -> return (VInt (n1 - n2))
        | BTimes, VInt n1, VInt n2                -> return (VInt (n1 * n2))
        | BDivide, VInt n1, VInt n2 when n2 <> 0  -> return (VInt (n1 / n2))
        | BModulus, VInt n1, VInt n2 when n2 <> 0 -> return (VInt (n1 mod n2))
        | BEqual, VInt n1, VInt n2                -> return (VBool (n1 = n2))
        | BEqual, VBool b1, VBool b2              -> return (VBool (b1 = b2))
        | BNeq, VInt n1, VInt n2                  -> return (VBool (n1 <> n2))
        | BNeq, VBool b1, VBool b2                -> return (VBool (b1 <> b2))
        | BLessThan, VInt n1, VInt n2             -> return (VBool (n1 < n2))
        | BLeq, VInt n1, VInt n2                  -> return (VBool (n1 <= n2))
        | BGreaterThan, VInt n1, VInt n2          -> return (VBool (n1 > n2))
        | BGeq, VInt n1, VInt n2                  -> return (VBool (n1 >= n2))
        | BAnd, VBool b1, VBool b2                -> return (VBool (b1 && b2))
        | BOr, VBool b1, VBool b2                 -> return (VBool (b1 || b2))
        | _ -> type_mismatch ()
      end
    | ENot e_not_body ->
      let* e_b = eval e_not_body in
      begin match e_b with
      | VBool b -> return (VBool (not b))
      | _ -> type_mismatch ()
      end
    | EIf { cond ; true_body ; false_body } ->
      let* e_b = eval cond in
      begin match e_b with
      | VBool b ->
        let* () = incr_time in
        if b
        then eval true_body
        else eval false_body
      | _ -> type_mismatch ()
      end
    | EProject { record ; label } ->
      let* r = eval record in
      begin match r with
      | VRecord body | VModule body ->
        begin match RecordLabel.Map.find_opt label body with
        | Some v -> return v
        | _ -> type_mismatch ()
        end
      | _ -> type_mismatch ()
      end
    (* failures *)
    | EAssert e_assert_body ->
      let* e_b = eval e_assert_body in
      begin match e_b with
      | VBool true -> return VUnit
      | VBool false -> abort "Failed assertion"
      | _ -> type_mismatch ()
      end
    | EAssume e_assert_body ->
      let* e_b = eval e_assert_body in
      begin match e_b with
      | VBool true -> return VUnit
      | VBool false -> vanish ()
      | _ -> type_mismatch ()
      end
    (* casing *)
    | EMatch { subject ; patterns } ->
      let* v = eval subject in
      let match_opt =
        List.find_map (fun (pat, body) ->
          match V.matches v pat with
          | Some bindings -> Some (body, fun env ->
              List.fold_left (fun acc (v_bind, id_bind) -> Env.add id_bind v_bind acc) env bindings
            )
          | None -> None
        ) patterns
      in
      begin match match_opt with
      | Some (e, f) -> local f (eval e)
      | None -> type_mismatch ()
      end
    | ECase { subject ; cases ; default } ->
      let* v = eval subject in
      begin match v with
      | VInt i ->
        let* () = incr_time in
        let case_opt =
          List.find_map (fun (case_i, body) ->
            if i = case_i then Some body else None
          ) cases
        in
        begin match case_opt with
        | Some body -> eval body
        | None -> eval default
        end
      | _ -> type_mismatch ()
      end
    (* let funs *)
    | ELetFunRec { funcs ; body } -> begin
        let* env = read in
        let rec rec_env = lazy (
          List.fold_left (fun acc fsig ->
            let comps = Lang.Ast_tools.Funsig.to_components fsig in
            match Lang.Ast_tools.Utils.abstract_over_ids comps.params comps.defn with
            | EFunction { param ; body } ->
              Env.add comps.func_id (VFunClosure { param ; closure = { body ; env = rec_env } }) acc
            | _ -> raise @@ InvariantFailure "Logically impossible abstraction from funsig without parameters"
          ) env funcs
        )
        in
        local (fun _ -> Lazy.force rec_env) (eval body)
      end
    | ELetFun { func ; body = body' } -> begin
        let comps = Lang.Ast_tools.Funsig.to_components func in
        Lang.Ast_tools.Utils.abstract_over_ids comps.params comps.defn
        |> function
        | EFunction { param ; body } ->
          local (fun env ->
              Env.add comps.func_id (VFunClosure { param ; closure = { body ; env = lazy env } }) env
            ) (eval body')
        | _ -> raise @@ InvariantFailure "Logically impossible abstraction from funsig without parameters"
      end

  and eval_let (var : Ident.t) ~(defn : a Expr.t) ~(body : a Expr.t) : a V.t m =
    let* v = eval defn in
    local (Env.add var v) (eval body)

  and eval_record_body (record_body : a Expr.t RecordLabel.Map.t) : a V.t RecordLabel.Map.t m =
    RecordLabel.Map.fold (fun key e acc_m ->
        let* acc = acc_m in
        let* v = eval e in
        return (RecordLabel.Map.add key v acc)
      ) record_body (return RecordLabel.Map.empty)

  (* evaluates statement list to a module. This is a total pain for rec funs *)
  and eval_stmt_list (stmts : a Expr.statement list) : a V.t m =
    let* module_body =
      let rec fold_stmts acc_m : a Expr.statement list -> a V.t RecordLabel.Map.t m = function
        | [] -> acc_m
        | SUntyped { var ; defn } :: tl ->
          let* acc = acc_m in
          let* v = eval defn in
          local (Env.add var v) (
            fold_stmts (return (RecordLabel.Map.add (RecordLabel.RecordLabel var) v acc)) tl
          )
        | STyped { typed_var = { var ; _ } ; defn ; _ } :: tl ->
          let* acc = acc_m in
          let* v = eval defn in
          local (Env.add var v) (
            fold_stmts (return (RecordLabel.Map.add (RecordLabel.RecordLabel var) v acc)) tl
          )
        | SFun fsig :: tl -> begin
            let* acc = acc_m in
            let comps = Lang.Ast_tools.Funsig.to_components fsig in
            match Lang.Ast_tools.Utils.abstract_over_ids comps.params comps.defn with
            | EFunction { param ; body } ->
              let* env = read in
              let v = VFunClosure { param ; closure = { body ; env = lazy env } } in
              local (Env.add comps.func_id v) (
                fold_stmts (return (RecordLabel.Map.add (RecordLabel.RecordLabel comps.func_id) v acc)) tl
              )
            | _ -> raise @@ InvariantFailure "Logically impossible abstraction from funsig without parameters"
          end
        | SFunRec fsigs :: tl ->
          let* acc = acc_m in
          let func_comps = List.map Lang.Ast_tools.Funsig.to_components fsigs in
          let* env = read in
          let rec rec_env = lazy (
              List.fold_left (fun acc comps ->
                let params = comps.Lang.Ast_tools.Function_components.params in
                match Lang.Ast_tools.Utils.abstract_over_ids params comps.defn with
                | EFunction { param ; body } ->
                  let v = VFunClosure { param ; closure = { body ; env = rec_env } } in
                  Env.add comps.func_id v acc
                | _ -> raise @@ InvariantFailure "Logically impossible abstraction from funsig without parameters"
              ) env func_comps
            )
          in
          let m =
            List.fold_left (fun acc comps ->
                RecordLabel.Map.add
                  (RecordLabel comps.Lang.Ast_tools.Function_components.func_id)
                  (Obj.magic @@ (* FIXME *)
                    Env.fetch comps.func_id (Lazy.force rec_env)
                    |> Option.get
                  ) acc
              ) acc func_comps
          in
          local (fun _ -> Lazy.force rec_env) (fold_stmts (return m) tl )
      in
      fold_stmts (return RecordLabel.Map.empty) stmts
    in
    return (VModule module_body)
  in

  let res, { State.log ; _ }, _ = run (eval e) State.empty Env.empty in
  let e =
    match res with
    | Ok r -> Format.printf "OK:\n  %s\n" (V.to_string r); r
    | Error `XType_mismatch { Interp_common.Errors.msg = _ ; body = () } ->
      Format.printf "TYPE MISMATCH\n"; VTypeMismatch
    | Error `XAbort  { Interp_common.Errors.msg ; body = () } ->
      Format.printf "FOUND ABORT %s\n" msg; VAbort
    | Error `XVanish () -> Format.printf "VANISH\n"; VVanish
    | Error `XUnbound_variable (Lang.Ast.Ident.Ident s, ()) ->
      Format.printf "UNBOUND VARIABLE %s\n" s; VUnboundVariable (Ident s)
    | Error `XReach_max_step () -> Format.printf "REACHED MAX STEP\n"; VVanish
  in
  e, log

let eval_pgm
    (type a)
    ?(feeder : int Feeder.t = Interp_common.Input_feeder.zero)
    (pgm : a Program.t)
  : a V.t
  =
  fst (eval_exp (EModule pgm) feeder)

let eval_pgm_to_time_feeder
    (type a)
    ?(feeder : int Feeder.t = Interp_common.Input_feeder.zero)
    (pgm : a Program.t)
  : a V.t * Interp_common.Timestamp.t Feeder.t
  =
  let v, log = eval_exp (EModule pgm) feeder in
  v, Input_log.to_time_feeder log
