
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

module type MONOID = sig
  type t
  val neutral : t
  val combine : t -> t -> t
end
