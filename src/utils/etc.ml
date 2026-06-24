(* et cetera *)

let compare_tup2 cmp_x cmp_y (x1, y1) (x2, y2) =
  let c = cmp_x x1 x2 in
  if c = 0 then cmp_y y1 y2 else c

let compare_tup3 cmp_x cmp_y cmp_z (x1, y1, z1) (x2, y2, z2) =
  let cx = cmp_x x1 x2 in
  if cx = 0 then
    let cy = cmp_y y1 y2 in
    if cy = 0 then
      cmp_z z1 z2
    else
      cy
  else
    cx

let list_fold_until f finish init ls =
  let rec go acc = function
    | [] -> finish acc
    | hd :: tl ->
      match f acc hd with
      | `Stop x -> x
      | `Continue a -> go a tl
  in
  go init ls

(**
  Set minor heap size to roughly 1 million machine words.
  On a 64 bit system, this is typically 8 million bytes, i.e. 8 megabytes.

  Precisely, this sets minor heap size to 2^20 words, which is 8 Mebibytes on a
  64 bit architecture.
*)
let set_1M_minor_heap () =
  Gc.set ({ (Gc.get ()) with minor_heap_size = 1 lsl 20 })
