
(*
  Literal Z3 formulas with a solver.

  It is recommended to use Smt.Formula.t to build
  formulas instead, and then transform into Z3 formulas.
*)
module Make () : Smt.Solve.SOLVABLE

module Default : Smt.Solve.SOLVABLE

include Smt.Solve.SOLVABLE with type ('a, 'k) t = ('a, 'k) Default.t
