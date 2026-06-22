
open Concolic.Common

module Driver = Concolic.Driver.Of_logger (Utils.Logger.From_builder (Utils.Builder.List_builder (Stat)))

type tape = Driver.tape

module Basic_test = struct
  module Trial = struct
    type t =
      | Number of int
      | Average
  end

  type[@warning "-69"] t =
    { testname : string
    (* ; test_result : Status.Terminal.t *)
    ; interp_time : Mtime.Span.t
    ; solve_time : Mtime.Span.t
    ; total_time : Mtime.Span.t
    ; n_interps  : int
    ; trial : Trial.t
    ; mode : string }

  let names =
    [ " Test Name" ; "Interp Time" ; "Solve Time" ; "Total" ; "N-interps" ; "Mode" ]

  let round_decimal ~digits x =
    let d = float_of_int digits in
    let order = 10. ** d in
    Float.round (x *. order) /. order

  let round_significant ~digits x =
    let xd = int_of_float (Float.log10 x) + 1 in
    let dd = float_of_int (digits - xd) in
    let order = 10. ** dd in
    Float.round (x *. order) /. order

  let to_strings x =
    let span_to_ms_string =
      fun span ->
        let fl = Utils.Time.span_to_ms span in
        Float.to_string @@
        if Float.compare fl 1. < 0
        then round_decimal fl ~digits:2
        else round_significant fl ~digits:2
    in
    [ Filename.basename x.testname |> String.take_first_while ((<>) '.')
    ; span_to_ms_string x.interp_time
    ; span_to_ms_string x.solve_time
    ; span_to_ms_string x.total_time
    ; Int.to_string x.n_interps
    ; x.mode ]

  let make
    (trial : Trial.t)
    (mode : string)
    (runtest : Lang.Ast.some_program -> Status.Terminal.t * tape) (* promises to update interp and solve time *)
    (testname : string)
    : t =
    let source = Lang.Parser.parse_program_from_file testname in (* span should maybe include this *)
    let span, (_, tape) = Utils.Time.time runtest source in
    { testname
    ; interp_time = Stat.sum_time Stat.Interp_time tape
    ; solve_time = Stat.sum_time Stat.Solve_time tape
    ; total_time = span (* ignores stats measured total time *)
    ; n_interps = Stat.sum_count Stat.N_interps tape
    ; trial
    ; mode }

  let average (tests : t list) : t =
    match tests with
    | [] -> failwith "Cannot take average of empty test"
    | [ test ] -> { test with trial = Average }
    | hd :: tl ->
      let n_trials = List.length tests in
      let init = { hd with trial = Average } in
      let sum =
        List.fold_left (fun acc x ->
          { acc with
            interp_time = Mtime.Span.add acc.interp_time x.interp_time
          ; solve_time = Mtime.Span.add acc.solve_time x.solve_time
          ; total_time = Mtime.Span.add acc.total_time x.total_time
          ; n_interps = acc.n_interps + x.n_interps }
        ) init tl
      in
      { sum with
        interp_time = Utils.Time.divide_span sum.interp_time n_trials
      ; solve_time = Utils.Time.divide_span sum.solve_time n_trials
      ; total_time = Utils.Time.divide_span sum.total_time n_trials
      ; n_interps = sum.n_interps / n_trials
      }
end

module Result_table = struct
  type t = Basic_test.t Latex_tbl.t

  let empty : t =
    { row_module = (module Basic_test)
    ; rows = []
    ; columns = [] }

  (* TODO: we should interweave the computations so as not to favor one *)
  (* Then should have a tester type, which comes with a "mode" name.
    And Basic_test can put many results in the same row *)
  let of_testname
    ?(avg_only : bool = true)
    (mode : string)
    (n_trials : int)
    (runtest : Lang.Ast.some_program -> Status.Terminal.t * tape)
    (testname : string)
    : t =
    Latex_tbl.append_rows empty (
      let results =
        List.init n_trials (fun n ->
          Basic_test.make (Number n) mode runtest testname
        )
      in
      let avg = Basic_test.average results in
      if avg_only
      then [ avg ]
      else avg :: results
    )
  let of_dirs
    ?(avg_only : bool = true)
    (mode : string)
    (n_trials : int)
    (dirs : string list)
    (runtest : Lang.Ast.some_program -> Status.Terminal.t * tape)
    : t =
    let rows =
      dirs
      |> Utils.File_utils.get_all_bjy_files
      |> List.sort (fun a b -> String.compare (Filename.basename a) (Filename.basename b))
      |> List.map (of_testname ~avg_only mode n_trials runtest)
    in
    List.fold_left Latex_tbl.concat (List.hd rows) (List.tl rows)

  let add_average
    (mode : string)
    (tbl : t)
    : t =
    let t0 = Mtime.Span.zero in
    let init = t0, t0, t0, 0, 0 in
    let interp_sum, solve_sum, total_sum, total_interps, n =
      List.fold_left (fun ((acc_interp_time, acc_solve_time, acc_total_time, acc_interps, n) as acc) row_or_hline ->
        match row_or_hline with
        | Latex_tbl.Row_or_hline.Row row ->
          Mtime.Span.add acc_interp_time row.Basic_test.interp_time
          , Mtime.Span.add acc_solve_time row.solve_time
          , Mtime.Span.add acc_total_time row.total_time
          , acc_interps + row.n_interps
          , n + 1
        | Hline -> acc
      ) init tbl.rows
    in
    Latex_tbl.append_rows tbl [
      Basic_test.{
          testname = mode ^ " average"
        ; trial = Average
        ; mode
        ; interp_time = Utils.Time.divide_span interp_sum n
        ; solve_time = Utils.Time.divide_span solve_sum n
        ; total_time = Utils.Time.divide_span total_sum n
        ; n_interps = total_interps / n
      }
    ]
end

open Result_table

let cdbench_args =
  let open Cmdliner.Term.Syntax in
  let open Cmdliner.Arg in
  let+ n_trials = value & opt int 50 & info ["trials"] ~doc:"Number of trials"
  and+ dirs = value & opt (list ~sep:' ' dir) [ "test/bjy/oopsla-24-benchmarks-ill-typed" ] & info ["dirs"] ~doc:"Directories to benchmark" in
  n_trials, dirs

let run () =
  let open Cmdliner in
  let open Cmdliner.Term.Syntax in
  Cmd.v (Cmd.info "cdbenchmark") @@
  let+ options = Options.cmd_arg_term
  and+ n_trials, dirs = cdbench_args in
  (* prepare channels to capture and discard all testing output *)
  let oc_null = Out_channel.open_bin "/dev/null" in
  Format.set_formatter_out_channel oc_null;
  let runtest_eager pgm =
    Driver.Eager.test_some_program
      ~options:{ options with is_random = true }
      ~do_wrap:true        (* always wrap during benchmarking *)
      ~do_type_splay:No    (* never type splay during benchmarking *)
      pgm
  in
  let runtest_deferred pgm =
    Driver.Deferred.test_some_program
      ~options:{ options with is_random = true }
      ~do_wrap:true        (* always wrap during benchmarking *)
      ~do_type_splay:No    (* never type splay during benchmarking *)
      pgm
  in
  let eager_results = of_dirs "Eager" n_trials dirs runtest_eager |> Result_table.add_average "Eager" in
  let deferred_results = of_dirs "Deferred" n_trials dirs runtest_deferred |> Result_table.add_average "Deferred" in
  let results = Latex_tbl.concat eager_results deferred_results in
  (* Testing is done, so we can set back the stdout channel *)
  Format.set_formatter_out_channel Out_channel.stdout;
  Out_channel.close oc_null;
  results
  |> Latex_tbl.show ~hum:true
  |> Format.printf "\n%s\n"

(*
  Common directories to benchmark include

    "test/bjy/soft-contract-ill-typed"
    "test/bjy/deep-type-error"
    "test/bjy/oopsla-24-tests-ill-typed"; "test/bjy/sato-bjy-ill-typed"
    "test/bjy/interp-ill-typed"

  To test multiple directories, put them in single quotes and separate by spaces.
*)

let () =
  match Cmdliner.Cmd.eval_value' @@ run () with
  | `Ok _ -> ()
  | `Exit i -> exit i