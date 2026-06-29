
let max_step = Interp_common.Step.Step (1 lsl 20)

(* TODO: should probably differentiate between vanish and other errors *)
let deval
  ?(feeder : Interp_common.Timestamp.t Interp_common.Input_feeder.t = Interp_common.Input_feeder.zero)
  (pgm : Lang.Ast.Embedded.pgm)
  : (Value.Without_symbols.t, Err.t) result
  =
  let expr = Lang.Ast_tools.Utils.pgm_to_module pgm in
  match Concolic.Deferred.Main.deferred_interp expr feeder ~max_step with
  | Ok v, state, _ -> Ok (Value.of_concolic v state.symbol_map)
  | Error status, _, _ -> Error (
    match status with
    | Concolic.Common.Status.Type_mismatch (_, msg) ->
      `XType_mismatch { Interp_common.Errors.msg ; body = () }
    | Found_abort (_, msg) -> `XAbort { Interp_common.Errors.msg ; body = () }
    | Unbound_variable (_, id) -> `XUnbound_variable (id, ())
    | Reached_max_step -> `XReach_max_step ()
    | Finished -> `XVanish () (* This is unpolished, but logically this must mean interpretation vanished because there is no value *)
  )

let deval_with_input_sequence
  (inputs : Interp_common.Input.t list)
  (pgm : Lang.Ast.Embedded.pgm)
  : (Value.Without_symbols.t, Err.t) result
  =
  match inputs with
  | [] -> deval pgm
  | _ ->
    (* Capture all output from interpreter (which translates the input sequence) *)
    let oc_null = Out_channel.open_bin "/dev/null" in
    Format.set_formatter_out_channel oc_null;
    let _, feeder =
      Interpreter.Interp.eval_pgm_to_time_feeder
        ~feeder:(Interp_common.Input_feeder.of_sequence inputs)
        pgm
    in
    Format.set_formatter_out_channel Out_channel.stdout;
    Out_channel.close oc_null;
    deval ~feeder pgm
