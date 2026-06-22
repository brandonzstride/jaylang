
module type ROW = sig
  type t

  val names : string list
  (** [names] are the column names *)

  val to_strings : t -> string list
  (** [to_strings t] are the strings to be put in the table for the row [t], one string for each column. *)
end

module Row_or_hline = struct
  type 'a t =
    | Row of 'a
    | Hline

  let return x = Row x
end

module Col_option = struct
  type t =
    | Right_align
    | Left_align
    | Center
    | No_space
    | Little_space of { point_size : int } (* incompatible with No_space *)
    | Vertical_line_to_right

  let to_string = function
  | Right_align -> "r"
  | Left_align -> "l"
  | Center -> "c"
  | No_space -> "@{}"
  | Little_space { point_size } -> "@{\\hspace{" ^ Int.to_string point_size ^ "pt}}" (* strangly, Format.sprintf can't show "@{" *)
  | Vertical_line_to_right -> "|"
end

module Column = struct
  type t = Col_option.t list
  (* align style must occur before space and vertical line *)

  let to_string ls =
    let rec loop = function
    | [] -> ""
    | hd :: tl -> Col_option.to_string hd ^ loop tl
    in
    match ls with
    | (Col_option.Center as hd) :: tl
    | (Right_align as hd) :: tl
    | (Left_align as hd) :: tl -> Col_option.to_string hd ^ loop tl
    | _ -> "c" ^ loop ls

  let default = []

  (* n is number of columns in whole table. ls may be shorter *)
  let tabular_cols (n : int) (ls : t list) : string =
    let ss = List.map to_string ls in
    String.concat " "
    begin
    if List.length ss < n
    then ss @ List.init (n - List.length ss) (fun _ -> to_string default)
    else List.take n ss
    end
end

(* This is probably just better as a functor, but this is kind of fun, so I'll leave it *)
type 'row t =
  { row_module : (module ROW with type t = 'row)
  ; rows : 'row Row_or_hline.t list
  ; columns : Column.t list } (* of same or lesser length than (val row_module).to_strings *)

let concat (type row) (a : row t) (b : row t) : row t =
  assert (a.columns = b.columns);
  let module A = (val a.row_module) in
  let module B = (val b.row_module) in
  assert (A.names = B.names);
  { a with rows = a.rows @ b.rows }

let append (type row) (tbl : row t) (rows : row Row_or_hline.t list) : row t =
  { tbl with rows = tbl.rows @ rows }

let append_rows (type row) (tbl : row t) (rows : row list) : row t =
  { tbl with rows = tbl.rows @ List.map Row_or_hline.return rows }

let rec transpose_grid = function
  | [] -> []
  | last_row :: [] ->
    List.map List.singleton last_row
  | row :: tl ->
    List.map2 List.cons row (transpose_grid tl)

let align_on ~(sep : char) (ls : string list) : string list =
  let pad_to_max col =
    let m =
      match col with
      | [] -> failwith "cannot align empty column"
      | hd :: tl ->
        List.fold_left (fun cur_max_len a ->
          let n = String.length a in
          if Int.compare cur_max_len n >= 0 then
            cur_max_len
          else
            n
        ) (String.length hd) tl
    in
    List.map (fun s -> s ^ String.init (m - String.length s) (fun _ -> ' ')) col
  in
  ls
  |> List.map (String.split_on_char sep)
  |> transpose_grid
  |> List.map pad_to_max
  |> transpose_grid
  |> List.map (String.concat (" " ^ String.of_char sep ^ " "))

let show_rows (type row) (row_to_strings : row -> string list) (x : row Row_or_hline.t list) : string list =
  let show_single_row = function
    | Row_or_hline.Hline -> "      \\hline"
    | Row row ->
      row
      |> row_to_strings
      |> String.concat " & " (* column delimiter in latex *)
      |> fun s -> "      " ^ s ^ " \\\\" (* line delimiter in latex *)
  in
  let non_hline_rows =
    align_on ~sep:'&' @@
    List.filter_map (function
      | Row_or_hline.Hline -> None
      | row -> Some (show_single_row row)
    ) x
  in
  List.fold_left (fun (i, acc) row ->
    match row with
    | Row_or_hline.Hline -> (i, show_single_row row :: acc)
    | _ -> (i + 1, List.nth non_hline_rows i :: acc)
    ) (0, []) x
  |> snd
  |> List.rev

let show_full (type row) (x : row t) : string =
  let module R = (val x.row_module) in
  let tabular_cols = Column.tabular_cols (List.length R.names) x.columns
  in
  let table_begin =
    [ "\\begin{table}"
    ; "  \\begin{center}"
    ; "    \\begin{tabular}{" ^ tabular_cols ^ "}" ]
  in
  let table_end =
    [ "    \\end{tabular}"
    ; "  \\end{center}"
    ; "\\end{table}" ]
  in
  table_begin
  @ [ "    " ^ String.concat " & " R.names ^ "\\\\"]
  @ (show_rows R.to_strings x.rows)
  @ table_end
  |> String.concat "\n"

let show_hum (type row) (x : row t) : string =
  let module R = (val x.row_module) in
  List.filter_map (function
    | Row_or_hline.Hline -> None
    | Row row -> Some (String.concat " | " @@ R.to_strings row)
  ) x.rows
  |> List.cons (String.concat " | " R.names) (* Put headers on front *)
  |> align_on ~sep:'|'
  |> String.concat "\n"

let show (type row) ?(hum : bool = true) (x : row t) : string =
  if hum
  then show_hum x
  else show_full x
