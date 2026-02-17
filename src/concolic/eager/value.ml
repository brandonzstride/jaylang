
module Make (K : Smt.Symbol.KEY) = struct
  module Concolic_value = Common.Cvalue.Make (K)

  include Lang.Value.Embedded (Concolic_value)

  include T
end

module Default = Make (Interp_common.Step)

include Default
