
module type KEY = sig
  type t
  val uid : t -> int
end

(* Symbols have a phantom 'k (key) parameter. The underlying
  key is actually an integer identifier *)
type ('a, 'k) t = ('a, int) s
and (_, 'b) s =
  | I : 'b -> (int, 'b) s
  | B : 'b -> (bool, 'b) s

let compare (type a) (x : (a, 'k) t) (y : (a, 'k) t) : int =
  match x, y with
  | I xi, I yi
  | B xi, B yi -> Int.compare xi yi

let equal (type a) (x : (a, 'k) t) (y : (a, 'k) t) : bool =
  match x, y with
  | I xi, I yi
  | B xi, B yi -> xi = yi

let make_int (k : 'k) (uid : 'k -> int) : (int, 'k) t =
  I (uid k)

let make_bool (k : 'k) (uid : 'k -> int) : (bool, 'k) t =
  B (uid k)

module Make (Key : KEY) = struct
  type nonrec 'a t = ('a, Key.t) t

  let make_int (k : Key.t) : int t =
    I (Key.uid k)

  let make_bool (k : Key.t) : bool t =
    B (Key.uid k)
end
