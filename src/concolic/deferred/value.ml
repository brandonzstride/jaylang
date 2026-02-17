
include Dvalue.Make (Common.Cvalue.Make (Interp_common.Timestamp))

module Time_symbol = Smt.Symbol.Make (Interp_common.Timestamp)

let symbolic_int (i : int) (t : Interp_common.Timestamp.t) : t =
  VInt (i, Smt.Formula.symbol (Time_symbol.make_int t))

let symbolic_bool (b : bool) (t : Interp_common.Timestamp.t) : t =
  VBool (b, Smt.Formula.symbol (Time_symbol.make_bool t))
