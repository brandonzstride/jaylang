
module T = struct
  type t = Step of int [@@unboxed]

  let equal (Step a) (Step b) = a = b
  let compare (Step a) (Step b) = Int.compare a b

  let zero = Step 0

  let[@inline always][@specialize] next (Step i) = Step (i + 1)

  let[@inline always][@specialize] to_int (Step i) = i

  let uid = to_int

  let to_string (Step i) = Int.to_string i
end

include T

module Map = Map.Make (T)
module Set = Set.Make (T)
