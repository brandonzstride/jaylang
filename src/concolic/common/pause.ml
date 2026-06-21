module type S = sig
  include Utils.Types.MONAD
  val pause : unit -> unit m

  val with_timeout : Mtime.span -> (unit -> 'a m) -> 'a m

  val run : 'a m -> 'a
end

module Id : S with type 'a m = 'a = struct
  include Utils.Identity.Monad
  let pause () = ()

  let with_timeout _ f = f ()

  let run a = a
end
