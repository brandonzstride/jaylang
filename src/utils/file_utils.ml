
let ls_dir dir =
  Sys.readdir dir
  |> Array.to_list
  |> List.map (Filename.concat dir)
  |> List.sort String.compare

(* Gets all files into a flat list from all given dirs *)
let get_all_files ?(filter : string -> bool = fun _ -> true) (dirs : string list) : string list =
  let rec loop outlist = function
    | [] -> outlist
    | f :: fs -> begin
      match Sys.is_directory f with
      | true ->
        ls_dir f
        |> List.append fs
        |> loop outlist
      | _ when filter f -> loop (f :: outlist) fs
      | _ | exception Sys_error _ -> loop outlist fs
    end
  in
  loop [] dirs

let get_all_bjy_files (dirs : string list) : string list =
  get_all_files ~filter:(fun f -> Filename.check_suffix f ".bjy") dirs
