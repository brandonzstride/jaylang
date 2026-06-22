
(* We use this instead of Preface's standard list monoid because we want to
  skip work with RHS is `[]`. *)

module Make (X : Types.T) = struct
  include Preface.Make.Monoid.Via_combine_and_neutral (struct
    type t = X.t list
    let neutral : t = []
    let combine : t -> t -> t = fun l r ->
      match r with
      | [] -> l
      | _ -> List.append l r
  end)
end