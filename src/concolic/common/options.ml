
type t =
  { global_timeout     : Mtime.span
  ; global_max_step    : int
  ; max_tree_depth     : int
  ; n_depth_increments : int
  ; is_random          : bool
  ; in_parallel        : bool }

let default =
  { global_timeout     = Mtime.Span.(90 * s) (* 90 seconds *)
  ; global_max_step    = 1 lsl 17 (* about 10^5 *)
  ; max_tree_depth     = 30
  ; n_depth_increments = 6
  ; is_random          = false
  ; in_parallel        = false }

let cmd_arg_term =
  let open Cmdliner.Term.Syntax in
  let open Cmdliner.Arg in
  let+ global_timeout =
    value & opt Utils.Time.argv_span_conv default.global_timeout
    & info ["t"; "timeout"] ~docv:"TIMEOUT" ~doc:"Global timeout seconds"
  and+ global_max_step =
    value & opt int default.global_max_step
    & info ["m"; "max-step"] ~docv:"MAX_STEP" ~doc:"Global max step"
  and+ max_tree_depth =
    value & opt int default.max_tree_depth
    & info ["d"; "depth"] ~docv:"DEPTH" ~doc:"Max tree depth"
  and+ n_depth_increments =
    value & opt int default.n_depth_increments
    & info ["n"; "num-depth-incr"] ~docv:"NUM_DEPTH_INC" ~doc:"Num depth increments"
  and+ is_random =
    value & flag & info ["r"; "random"] ~docv:"RANDOMIZE" ~doc:"Randomize"
  and+ in_parallel =
    value & flag & info ["p"; "parallel"] ~docv:"PARALLELIZE" ~doc:"Run checks in parallel"
  in
  { global_timeout
  ; global_max_step
  ; max_tree_depth
  ; is_random
  ; n_depth_increments
  ; in_parallel
  }
