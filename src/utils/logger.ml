
module type S = sig
  type tape
  type a
  include Types.MONAD
  val log : a -> unit m
  val tell : tape -> unit m
  val listen : 'a m -> ('a * tape) m
end

module type FULL = sig
  module B : Builder.S
  type 'a m = 'a * B.t
  include S with type tape = B.t and type a = B.a and type 'a m := 'a m
  val run : 'a m -> 'a * tape
end

module From_builder (B : Builder.S) = struct
  module B = B
  type a = B.a
  type tape = B.t
  type 'a m = 'a * B.t
  let return a = (a, B.empty)
  let bind (a, t) f =
    let (b, t') = f a in
    (b, B.combine t t')
  let log x = ((), B.cons x B.empty)
  let tell t = ((), t)
  let run m = m
  let listen (a, t) = ((a, t), t)
  let ( let* ) = bind
end
