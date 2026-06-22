
module type T = sig
  type t
end

module type MONAD = sig
  type 'a m
  val return : 'a -> 'a m
  val bind : 'a m -> ('a -> 'b m) -> 'b m
  val ( let* ) : 'a m -> ('a -> 'b m) -> 'b m
  (** [let*] is [bind] *)
end

module type TRANSFORMED = sig
  type 'a lower
  include MONAD
  val upper : 'a lower -> 'a m
  val map_t : ('a lower -> 'b lower) -> ('a m -> 'b m)
end

module type TRANSFORMER = functor (M : MONAD) -> TRANSFORMED with type 'a lower := 'a M.m

module type MONOID = sig
  type t
  val neutral : t
  val combine : t -> t -> t
end
