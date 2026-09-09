open Utils

let testcase_of_filename (testname : string) : unit Alcotest.test_case option =
  let metadata = Metadata.of_bjy_file testname in
  if metadata.skip then None else
  let is_error_expected =
    match metadata.typing with
    | Ill_typed -> true
    | Well_typed | Exhausted -> false
  in
  let speed_level =
    match metadata.speed with
    | Slow -> `Slow
    | Fast -> `Quick
  in
  Some (Alcotest.test_case testname speed_level
  @@ fun () ->
    Cmdliner.Cmd.eval_value' ~argv:(Array.append [| ""; testname; "-t"; "10.0" |] metadata.flags) Concolic.Driver.Deferred.eval
    |> begin function
      | `Ok status -> Concolic.Common.Status.is_error_found status
      | `Exit i -> raise @@ Invalid_argument (Format.sprintf "Test couldn't evaluate and finished with exit code %d." i)
    end
    |> Bool.equal is_error_expected
    |> Alcotest.check Alcotest.bool "bjy deferred concolic" true
  )

let root_dir = "test/bjy/"

let make_tests (dirs : string list) : unit Alcotest.test list =
  List.map (fun dirname ->
    ( dirname
    , [ root_dir ^ dirname ]
      |> File_utils.get_all_bjy_files
      |> List.filter_map testcase_of_filename
    )
  ) dirs

let () =
  Alcotest.run "deferred concolic"
  @@ make_tests
    [ "oopsla-26-ill-typed"
    ; "oopsla-26-well-typed"

    ; "auklet-ill-typed"
    ; "auklet-well-typed"

    (* ; "deep-type-error" *)

    (* ; "interp-ill-typed" *)
    (* ; "interp-well-typed" *)

    ; "edge-cases-ill-typed"
    ; "edge-cases-well-typed"

    ; "deterministic-functions-well-typed"
    ; "deterministic-functions-ill-typed"

    ; "functors-ill-typed"
    ; "functors-well-typed"

    ; "oopsla-24-tests-ill-typed"
    ; "oopsla-24-tests-well-typed"

    ; "oopsla-24-benchmarks-ill-typed"
    ; "oopsla-24-benchmarks-well-typed"

    ; "soft-contract-ill-typed"
    ; "soft-contract-well-typed"

    ; "soft-contract-splayed-well-typed"

    ; "sato-bjy-ill-typed"
    ; "sato-bjy-well-typed"

    ; "type-splayed-recursion-ill-typed"
    ; "type-splayed-recursion-well-typed"

    ; "nondeterministic-types"
    ]

