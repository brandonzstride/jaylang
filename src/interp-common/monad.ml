
type ('a, 'x) t =
  { run : 'r.
      reject:('err -> 'state -> Step.t -> 'r) ->
      accept:('a -> 'state -> Step.t -> 'r) ->
      'state -> Step.t -> 'env -> 'r
  } constraint 'x = < err : 'err ; env : 'env ; state : 'state >
  [@@unboxed]

module Specialize (State : Utils.Types.T) (Env : Utils.Types.T) (Err : Utils.Types.T) = struct
  type 'a m =
    ( 'a
    , < err : Err.t
      ; env : Env.t
      ; state : State.t >
    ) t

  (* s for "safe"; it does not error *)
  type 'a s =
    ( 'a
    , < err : Utils.Empty.t
      ; env : Env.t
      ; state : State.t >
    ) t
end

let[@inline] bind (x : ('a, 'x) t) (f : 'a -> ('b, 'x) t) : ('b, 'x) t =
  { run = fun ~reject ~accept state step env ->
      x.run state step env ~reject ~accept:(fun x state step ->
          (f x).run ~reject ~accept state step env
        )
  }

let ( let* ) = bind

let[@inline] return (a : 'a) : ('a, 'x) t =
  { run = fun ~reject:_ ~accept state step _ ->
      accept a state step
  }

(*
  -----------
  ENVIRONMENT
  -----------
*)

let read : ('env, < env : 'env ; .. >) t =
  { run = fun ~reject:_ ~accept state step env ->
      accept env state step
  }

let[@inline] local (f : 'env -> 'env) (x : ('a, < env : 'env ; .. > as 'x) t) : ('a, 'x) t =
  { run = fun ~reject ~accept state step env ->
      x.run ~reject ~accept state step (f env)
  }

let local' (env : 'e) (x : ('a, < env : 'e ; .. >) t) : ('a, < env : 'env ; .. >) t =
  { run = fun ~reject ~accept state step _ ->
      x.run ~reject ~accept state step env
  }

(*
  -----
  STATE
  -----
*)

let get : ('state, < state : 'state ; .. >) t =
  { run = fun ~reject:_ ~accept state step _ ->
      accept state state step
  }

let[@inline] modify (f : 'state -> 'state) : (unit, < state : 'state ; .. >) t =
  { run = fun ~reject:_ ~accept state step _ ->
      accept () (f state) step
  }

(*
  -----
  ERROR
  -----
*)

let[@inline] escape (err : 'err) : ('a, < err : 'err ; .. >) t =
  { run = fun ~reject ~accept:_ state step _ ->
      reject err state step
  }

let[@inline always] handle_error (x : ('a, < err : 'e1 ; .. >) t)
    (ok : 'a -> ('b, 'x) t) (err : 'e1 -> ('b, 'x) t)
  : ('b, 'x) t =
  { run = fun ~reject ~accept state step env ->
        x.run state step env
          ~reject:(fun a state step ->
              (err a).run ~reject ~accept state step env
            )
          ~accept:(fun a state step ->
              (ok a).run ~reject ~accept state step env
            )
  }


(*
  ------------------
  ESCAPING THE MONAD
  ------------------
*)

(*
  Ideally we would use a polymorphic error, like
    x : 'err. ('a, < err : 'err ; .. >) t
  but there is no syntax to return such a type from a function, so it is often
  easier to represent safe values as those that have an empty error as opposed
  to a forall type.
*)
let run_safe (x : ('a, < err : Utils.Empty.t ; env : 'env ; state : 'state >) t)
  (init_state : 'state) (init_env : 'env)
  : 'a * 'state * Step.t =
  x.run init_state Step.zero init_env
    ~reject:Utils.Empty.absurd
    ~accept:(fun a state step -> a, state, step)

let run (x : ('a, < err : 'err ; env : 'env ; state : 'state >) t)
  (init_state : 'state) (init_env : 'env)
  : ('a, 'err) result * 'state * Step.t =
  x.run init_state Step.zero init_env
    ~reject:(fun e state step -> Error e, state, step)
    ~accept:(fun a state step -> Ok a, state, step)

(*
  -----------------
  INTERPRETER STUFF
  -----------------
*)

let step : (Step.t, 'x) t =
  { run = fun ~reject:_ ~accept state step _ ->
      accept step state step
  }

let[@inline] incr_step ~max_step ~fail_on_max =
  { run = fun ~reject ~accept state step _ ->
      let step = Step.next step in
      if Step.compare step max_step > 0 then
        let err, state = fail_on_max state in reject err state step
      else
        accept () state step
  }
