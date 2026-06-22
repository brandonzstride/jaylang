(**
  Module [Cloc_lib].

  Count lines of code in Bluejay files.

  To count the total lines of all test files and get a
  pretty output, try:

    cloc --force-lang="OCaml",bjy ./test

  ... but this `count_bjy_lines` function here is still
  nice to use as a helper within this system.
*)

let count_bjy_lines filename =
  In_channel.with_open_bin filename (fun file ->
    In_channel.fold_lines (fun (`Depth d, `Count c) s ->
      let is_empty, depth =
        let rec loop depth is_empty = function
          | '(' :: '*' :: tl -> loop (depth + 1) is_empty tl (* entered new comment *)
          | '*' :: ')' :: tl -> loop (depth - 1) is_empty tl (* exited comment *)
          | _ :: tl when depth > 0 -> loop depth is_empty tl (* in comment, so don't read character *)
          | c :: tl -> loop depth (is_empty && Char.Ascii.is_white c) tl (* character counts toward code if not whitespace *)
          | [] -> is_empty, depth
        in
        let chars = List.of_seq (String.to_seq s) in
        loop d true chars
      in
      `Depth depth, `Count (c + if is_empty then 0 else 1) (* only increment line number if not empty *)
    ) (`Depth 0, `Count 0) file
    |> fun (_, `Count c) -> c
  )