
(* can be packed into a list *)
include Utils.Pack.Make (Utils.Identity)

let to_string = function
  | I i -> Int.to_string i
  | B b -> Bool.to_string b

let input_conv : t Cmdliner.Arg.Conv.t =
  let parser = function
    | "true" -> Ok (B true)
    | "false" -> Ok (B false)
    | s ->
      match int_of_string_opt s with
      | Some i -> Ok (I i)
      | None -> Error (Format.sprintf "Failed to parse an input from '%s'." s)
  in
  let pp formatter i =
    Format.fprintf formatter "%s" (to_string i)
  in
  Cmdliner.Arg.Conv.make ~parser ~pp () ~docv:"INPUT"

let parse_list =
  let open Cmdliner.Arg in
  value & opt (list ~sep:' ' input_conv) [] & info ["inputs"] ~doc:"Input list for interpreter"
