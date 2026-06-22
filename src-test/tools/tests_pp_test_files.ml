
open Lang.Parser.Bluejay
open Tests_utils
open Utils

let make_pp_test_from_filename (testname : string) : unit Alcotest.test_case =
  let ast1 = parse_file testname in
  make_test_case_from_ast testname parse_single_pgm_string (fun () -> ast1)

let root_dir = "test/bjy/"

let make_pp_tests (dirs : string list) : unit Alcotest.test list =
  List.map (fun dirname ->
    ( dirname
    , [ root_dir ^ dirname ]
      |> File_utils.get_all_bjy_files
      |> List.map make_pp_test_from_filename
    )
  ) dirs
