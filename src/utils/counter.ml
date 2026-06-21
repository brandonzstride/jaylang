(**
  Module [Counter].

  Sometimes it's helpful to increment a counter,
  but we may run into trouble if parallel computations
  share a counter. We ease this problem by using an
  atomic reference
*)

type t = int Atomic.t

let create () =
  Atomic.make 0

let next (t : t) : int =
  Atomic.fetch_and_add t 1 + 1
