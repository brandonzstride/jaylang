
(**
  Module [Computation_pool].

  This module provides an interface to the [Moonpool] library
  to allow parallel computation of items in a list until one of
  this items finishes and signals to quit.
*)

module type COMPUTE_RESULT = sig
  include Utils.Types.MONOID
  val is_signal_to_quit : t -> bool
end

val process_all : (module C : COMPUTE_RESULT) -> ('a -> C.t) -> 'a Nel.t -> C.t
(** [process_all (module C) run work_items] runs the computation [run] on all
    items in [work_items] at once. Each item gets a fresh domain even if there
    is just one work item. *)

