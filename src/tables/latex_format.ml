
(*
  Assumes \rot has been defined to rotate column headers 90 degrees.
  It can be defined as follows:

    \newcommand*\rot{\rotatebox{90}}
*)
let rotate_90 (s : string) : string =
  Printf.sprintf "\\rot{%s}" s

(*
  Assumes \red has been define:

    \newcommand*\red{\textcolor{red}}
*)
let red (s : string) : string =
  Printf.sprintf "\\red{%s}" s

let texttt (s : string) : string =
  "\\texttt{" ^ String.replace_all ~sub:"_" ~by:"\\_" s ^ "}"