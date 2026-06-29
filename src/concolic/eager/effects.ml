
open Interp_common
open Common

type k = Step.t

module Feeder = Input_feeder.Make (Step)

module State = struct
  type t =
    { path : k Path.t
    ; rev_inputs : Input.t list }

  let empty : t =
    { path = Path.empty
    ; rev_inputs = [] }

  let inputs ({ rev_inputs ; _ } : t) : Input.t list =
    List.rev rev_inputs
end

include Interp_common.Monad
include Interp_common.Monad.Specialize (State) (Value.Env) (Status.Eval)

let fetch (id : Lang.Ast.Ident.t) : Value.t m =
  let* env = read in
  match Value.Env.fetch id env with
  | Some v -> return v
  | None ->
    let* s = get in
    escape (Status.Unbound_variable (State.inputs s, id))

let incr_step ~max_step =
  incr_step ~max_step ~fail_on_max:(fun state -> Status.Reached_max_step, state)

let abort (msg : string) : 'a m =
  let* s = get in
  escape (Status.Found_abort (State.inputs s, msg))

let type_mismatch (msg : string) : 'a m =
  let* s = get in
  escape (Status.Type_mismatch (State.inputs s, msg))

let vanish : 'a m =
  escape Status.Finished

let push_branch (dir : k Direction.t) : unit m =
  if Smt.Formula.is_const @@ Direction.to_formula dir
  then return ()
  else modify (fun (s : State.t) -> { s with path = Path.cons dir s.path })

module Step_symbol = Smt.Symbol.Make (Step)

let get_input (type a) (make_key : Step.t -> a Feeder.Key.t) (feeder : Step.t Input_feeder.t) : Value.t m =
  let* s = step in
  let key = make_key s in
  let v = feeder.get key in
  match key with
  | I k ->
    let* () = modify (fun (s : State.t) -> { s with rev_inputs = I v :: s.rev_inputs }) in
    return @@ Value.M.VInt (v, Smt.Formula.symbol (Step_symbol.make_int k))
  | B k ->
    let* () = modify (fun (s : State.t) -> { s with rev_inputs = B v :: s.rev_inputs }) in
    return @@ Value.M.VBool (v, Smt.Formula.symbol (Step_symbol.make_bool k))

let run (x : 'a m) : Status.Eval.t * k Path.t =
  match run x State.empty Value.Env.empty with
  | Ok _, state, _ ->
    Status.Finished, state.path
  | Error e, state, _ -> e, state.path
