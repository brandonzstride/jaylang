
(*
 I should pass around instances of random number generators.
 And also have a functor to pre-seed a module with such a generator.
*)

let seed =
  String.fold_left (fun acc c -> Char.code c + acc) 0 "jhu-pl-lab"

let reset () : unit =
  Random.init seed

let int : int -> int =
  Random.int

let bool : unit -> bool =
  Random.bool

let int_incl : int -> int -> int = fun min max ->
  Random.int_in_range ~min ~max

let any_pos_int () : int =
  Random.full_int Int.max_int