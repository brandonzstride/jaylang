(**
  Module [Pack].

  Commonly in this repo when have a parametrized type ['a t], and we
  need to store [int t] and [bool t] together. This functor abstracts
  that pattern.

  We could alternatively have something like [type pack = Pack : 'a t -> pack],
  which means we are not limited to only [int] and [bool] parameters,
  but then we lose "type casing", and that generalization is not
  currently needed.
*)

module type S = sig
  type 'a x
  type t =
    | I of int x
    | B of bool x
  (** Pack [x] into an int [I] case and a bool [B] case. *)

  val compare : t -> t -> int
end

module Make (X : Comparable.S1) : S with type 'a x := 'a X.t = struct
  type t =
    | I of int X.t
    | B of bool X.t

  let compare a b =
    match a, b with
    | I _, B _ -> -1
    | B _, I _ -> 1
    | I x1, I x2 -> X.compare Int.compare x1 x2
    | B x1, B x2 -> X.compare Bool.compare x1 x2
end
