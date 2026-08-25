type t =
  | Atom of string
  | Group of t list

let rec to_string (sexp : t) : string =
  match sexp with
  | Atom s -> s
  | Group sexps -> "(" ^ String.concat "," (List.map to_string sexps) ^ ")"
