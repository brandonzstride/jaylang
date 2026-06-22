open Lang
open Utils
open Deferred

(*
  This just runs some acceptance tests on the deferred interpreter
  over the embedded target program.
  * A well-typed program never hits an error
  * An ill-typed program may or may not hit an error
*)
let testcase_of_filename (testname : string) : unit Alcotest.test_case =
  let metadata = Metadata.of_bjy_file testname in
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
  Alcotest.test_case testname speed_level
  @@ fun () ->
  let bjy =
    let content = In_channel.with_open_bin testname In_channel.input_all in
    Parser.Bluejay.parse_single_pgm_string content
  in
  let emb = Translate.Convert.bjy_to_emb ~do_wrap:true ~do_type_splay:No bjy in
  let is_error =
    match Main.deval emb with
    | Ok _
    | Error `XVanish _
    | Error `XReach_max_step _ -> false
    | Error _ -> true
  in
  if is_error_expected
  then (* any result okay because this run may or may not hit error *)
    Alcotest.check Alcotest.pass "deval" () ()
  else (* definitely should not have hit the error *)
    Alcotest.check Alcotest.bool "deval" true (not is_error)

let root_dir = "test/deferred/"

let make_tests (dirs : string list) : unit Alcotest.test list =
  List.map (fun dirname ->
    ( dirname
    , [ root_dir ^ dirname ]
      |> File_utils.get_all_bjy_files
      |> List.map testcase_of_filename
    )
  ) dirs

let () =
  Alcotest.run "deferred"
  @@ make_tests
    [ "basic"
    ; "recursive"
    ]

