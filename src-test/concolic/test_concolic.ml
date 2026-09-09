open Concolic
open Utils

let testcase_of_filename (testname : string) : unit Alcotest.test_case option =
  let metadata = Metadata.of_bjy_file testname in
  if metadata.skip then None else
  let speed_level =
    match metadata.speed with
    | Slow -> `Slow
    | Fast -> `Quick
  in
  Some (Alcotest.test_case testname speed_level
  @@ fun () ->
    Cmdliner.Cmd.eval_value' ~argv:(Array.append [| ""; testname; "-t"; "10.0" |] metadata.flags) Driver.eval
    |> function
      | `Ok status -> begin
        let answer = Common.Status.Terminal.to_answer status in
        let res =
          match metadata.typing, answer with
          | Metadata.Typing.Ill_typed, Common.Answer.Ill_typed
          | Exhausted, Well_typed (* A program is exhaused iff the concolic answer is that the program is well-typed *)
          | Well_typed, Unknown (* if concolic evaluator is unsure, we take that as well-typed *)
          | Well_typed, Well_typed -> true (* answer is allowable with expected typing *)
          | _ -> false
        in
        Alcotest.check Alcotest.bool "bjy concolic" true res
      end
      | `Exit i -> raise @@ Invalid_argument (Format.sprintf "Test couldn't evaluate and finished with exit code %d." i)
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
  Alcotest.run "concolic"
  @@ make_tests
    [ "oopsla-26-ill-typed"
    ; "oopsla-26-well-typed"

    ; "ocaml-functors-ill-typed"
    ; "ocaml-functors-well-typed"

    ; "auklet-ill-typed"
    ; "auklet-well-typed"

    (* ; "deep-type-error" *)

    ; "interp-ill-typed"
    ; "interp-well-typed"

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

    ; "type-splayed-incomplete"

    ; "nondeterministic-types"
    ]

