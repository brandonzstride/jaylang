
open Lang.Ast.Expr
open Lang.Ast_tools.Utils

let make_test_case_from_ast
    (type a)
    (testname : string)
    (parse : string -> a statement list)
    (ast_gen : unit -> a statement list) =
  Alcotest.test_case testname `Quick (
    fun () ->
      let ast1 = ast_gen () in
      let pp_ast1 =
        String.concat "\n\n" (List.map statement_to_string ast1)
      in
      let ast2 =
        try
          parse pp_ast1
        with
        | exn ->
          print_endline
            (Printf.sprintf "Failure while parsing this text:\n%s" pp_ast1);
          raise exn
      in
      let pp_ast2 =
        String.concat "\n\n" (List.map statement_to_string ast2)
      in
      Alcotest.(check string) "pp_compare" pp_ast1 pp_ast2;
      Alcotest.(check int) "ast_compare" 0
        (compare
           Lang.Ast.Ident.compare
           (pgm_to_module ast1)
           (pgm_to_module ast2)))
