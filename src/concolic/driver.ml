
open Common

module type S = sig
  type tape
  module type DRIVER = sig
    val test_some_program :
      options:Options.t ->
      do_wrap:bool ->
      do_type_splay:Translate.Splay.t ->
      Lang.Ast.some_program ->
      Status.Terminal.t * tape

    val test_some_file :
      options:Options.t ->
      do_wrap:bool ->
      do_type_splay:Translate.Splay.t ->
      string ->
      Status.Terminal.t * tape

    val eval : Status.Terminal.t Cmdliner.Cmd.t
  end

  module Make (Key : Smt.Symbol.KEY) (_ : Target_queue.MAKE) (_ : Evaluator.EVAL with type k := Key.t) () : DRIVER

  module Eager : DRIVER

  module Deferred : DRIVER

  module Default = Eager

  include DRIVER (* Is Default *)
end

module Of_logger (Log : Utils.Logger.FULL with type B.a = Stat.t) : S with type tape = Log.tape = struct
  type tape = Log.tape

  module type DRIVER = sig
    val test_some_program :
      options:Options.t ->
      do_wrap:bool ->
      do_type_splay:Translate.Splay.t ->
      Lang.Ast.some_program ->
      Status.Terminal.t * tape

    val test_some_file :
      options:Options.t ->
      do_wrap:bool ->
      do_type_splay:Translate.Splay.t ->
      string ->
      Status.Terminal.t * tape

    val eval : Status.Terminal.t Cmdliner.Cmd.t
  end

  module Make (Key : Smt.Symbol.KEY) (Make_tq : Target_queue.MAKE)
    (C : Evaluator.EVAL with type k := Key.t) () : DRIVER = struct

    module Eval = Evaluator.Make (Key) (Make_tq) (Log)

    let solve = Smt.Solve.main_solve (module Overlays.Typed_z3.Default)

    (*
      ----------------------
      TESTING BY EXPRESSIONS
      ----------------------
    *)

    let test_with_timeout
      : options:Options.t -> Lang.Ast.Embedded.t -> Status.Terminal.t * tape
      = fun ~options prog ->
      Eval.c_loop ~options C.ceval solve prog

    module Compute_result = struct
      open Log

      type t = Status.Terminal.t Log.m

      let neutral : t = Log.return Status.Exhausted_full_tree

      let combine : t -> t -> t = fun a b ->
        let* a in let* b in
        return (Status.min a b)

      let is_signal_to_quit : t -> bool =
        (* TOOD: this is hideous, but I don't see a way around it right now *)
        fun sm -> let s, _tape = run sm in Status.is_error_found s
    end

    (*
      ----------------
      TESTING PROGRAMS
      ----------------
    *)

    let test_without_printing :
      options:Options.t ->
      do_wrap:bool ->
      do_type_splay:Translate.Splay.t ->
      Lang.Ast.some_program ->
      Status.Terminal.t * tape =
      fun ~options ~do_wrap ~do_type_splay program ->
      if options.in_parallel
      then
        let pgms =
          Translate.Convert.some_program_to_many_emb program ~do_wrap ~do_type_splay
        in
        match pgms with
        | pgm :: [] ->
          (* Nothing to do in parallel if only one program *)
          test_with_timeout ~options @@ Lang.Ast_tools.Utils.pgm_to_module pgm
        | _ ->
          let run pgm =
            (* makes a new solver for this thread *)
            let module Z = Overlays.Typed_z3.Make () in
            let solve = Smt.Solve.simplify (Smt.Solve.direct_solve (module Z)) in
            let expr = Lang.Ast_tools.Utils.pgm_to_module pgm in
            Eval.c_loop ~options C.ceval solve expr
          in
          let status_m =
            Overlays.Computation_pool.process_all (module Compute_result) run pgms
          in
          Log.run status_m
      else
        let pgm = Translate.Convert.some_program_to_emb program ~do_wrap ~do_type_splay in
        test_with_timeout ~options @@ Lang.Ast_tools.Utils.pgm_to_module pgm

    let test_some_program :
      options:Options.t ->
      do_wrap:bool ->
      do_type_splay:Translate.Splay.t ->
      Lang.Ast.some_program ->
      Status.Terminal.t * tape =
      fun ~options ~do_wrap ~do_type_splay program ->
      let status, tape =
        match do_type_splay with
        | No -> test_without_printing ~options ~do_wrap ~do_type_splay program
        | Yes_with_depth max_depth ->
          (* Try depth 1 first and work up to max depth, which was the depth provided. *)
          let rec loop cur_depth =
            let status, tape =
              test_without_printing ~options ~do_wrap ~do_type_splay:(Yes_with_depth cur_depth) program
            in
            (* Done if we did the max depth or there was no error, which means there's also no error at one level deeper *)
            if cur_depth >= max_depth || not (Common.Status.is_error_found status)
            then status, tape
            else loop (cur_depth + 1)
          in
          loop 1
      in
      Format.printf "%s\n" (Status.to_loud_string status);
      status, tape

    (*
      -------------------
      TESTING BY FILENAME
      -------------------
    *)

    let test_some_file :
      options:Options.t -> do_wrap:bool -> do_type_splay:Translate.Splay.t -> string ->
      Status.Terminal.t * tape =
      fun ~options ~do_wrap ~do_type_splay file ->
      test_some_program
        ~options
        ~do_wrap
        ~do_type_splay
        (Lang.Parser.parse_program_from_file file)

    (*
      ------------------------------
      TESTING FROM COMMAND LINE ARGS
      ------------------------------
    *)

    let eval : Status.Terminal.t Cmdliner.Cmd.t =
      let open Cmdliner in
      let open Cmdliner.Term.Syntax in
      Cmd.v (Cmd.info "ceval") @@
      let+ options = Options.cmd_arg_term
      and+ `Do_wrap do_wrap, `Do_type_splay do_type_splay = Translate.Convert.cmd_arg_term
      and+ pgm = Lang.Parser.parse_program_from_argv in
      let status, _ = test_some_program ~options ~do_wrap ~do_type_splay pgm in
      status
  end

  module Eager = Make (Interp_common.Step) (Target_queue.Make_BFS) (struct
    let ceval = Eager.Main.eager_eval
  end) ()

  module Deferred = Make (Interp_common.Timestamp) (Target_queue.Make_BFS) (struct
    let ceval = Deferred.Main.deferred_eval
  end) ()

  module Default = Eager

  include Default
end

include Of_logger (Utils.Logger.From_builder (Stat.Unit_builder))
