module type S = sig
  include Utils.Types.MONAD
  val pause : unit -> unit m
  (** [pause ()] is a chance to check if timeout has been exceeded. *)

  val with_timeout : Mtime.span -> (unit -> 'a m) -> 'a m

  val run : 'a m -> 'a
end

module Id : S with type 'a m = 'a
(** [Id] is the identity monad. *)
