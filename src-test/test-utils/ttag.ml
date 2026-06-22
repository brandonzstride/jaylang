
(*
  Complete metadata:

(***
  (
    (features (Polymorphic_types Refinement_types Dependent_types Modules Mu_types Parametric_types First_class_types Deterministic_functions Variants Records Recursive_functions Higher_order_functions Subtyping OOP_style Return_error Usage_error Other))
    (reasons (Polymorphic_types Refinement_types Dependent_types Modules Mu_types Parametric_types First_class_types Deterministic_functions Variants Records Recursive_functions Higher_order_functions Subtyping OOP_style Return_error Usage_error Other))
    (speed <Fast or Slow>)
    (typing <Well_typed or Ill_typed>)
    (flags "")
  )
*)

*)

type t =
  | Polymorphic_types
  | Refinement_types
  | Dependent_types
  | Modules
  | Mu_types
  | Parametric_types
  | First_class_types
  | Deterministic_functions
  | Variants
  | Records
  | Recursive_functions
  | Higher_order_functions
  | Subtyping
  | OOP_style
  | Return_error
  | Usage_error
  | Other
  [@@deriving sexp, enumerate, eq]

(* Description, if not totally trivial *)
(* Lists are often excluded because they are trivial, built in, and used so often *)
let to_description = function
  | Polymorphic_types       -> "Untouchable polymorphic values"
  | Refinement_types        -> "Type is refined with a predicate"
  | Dependent_types         -> "Codomain depends on a value"
  | Modules                 -> "Modules and module types are used, or dependent record types"
  | Mu_types                -> "Recursive types, excluding lists"
  | Parametric_types        -> "Type functions with simple parameters, excluding lists"
  | First_class_types       -> "Types are used as values for more than simple parametric types"
  | Deterministic_functions -> "Deterministic function types"
  | Variants                -> "Excluding lists"
  | Records                 -> "Not including dependent records types, which are modules"
  | Recursive_functions     -> "Including mutual recursion"
  | Higher_order_functions  -> "Including intersection/match types"
  | Subtyping               -> "The program uses subtyping, or the error is in the spirit of subtyping"
  | OOP_style               -> "Self-referential records"
  | Return_error            -> "The value returned from a function has the wrong type"
  | Usage_error             -> "A function is used with the wrong type, including built-in operators"
  | Other                   -> "Anything not included in the above"

let to_name = function
  | Polymorphic_types       -> "Polymorphic types"
  | Refinement_types        -> "Refinement types"
  | Dependent_types         -> "Dependent types"
  | Modules                 -> "Modules"
  | Mu_types                -> "Mu types"
  | Parametric_types        -> "Parametric types"
  | First_class_types       -> "First class types"
  | Deterministic_functions -> "Deterministic functions"
  | Variants                -> "Variants"
  | Records                 -> "Records"
  | Recursive_functions     -> "Recursive functions"
  | Higher_order_functions  -> "Higher order functions"
  | Subtyping               -> "Subtyping"
  | OOP_style               -> "OOP-style"
  | Return_error            -> "Return error"
  | Usage_error             -> "Usage error"
  | Other                   -> "Other"

let to_name_with_underline = function
  | Polymorphic_types       -> "\\underline{P}olymorphic types"
  | Refinement_types        -> "Re\\underline{f}inement types"
  | Dependent_types         -> "\\underline{D}ependent types"
  | Modules                 -> "\\underline{M}odules"
  | Mu_types                -> "M\\underline{u} types"
  | Parametric_types        -> "P\\underline{a}rametric types"
  | First_class_types       -> "First class \\underline{t}ypes"
  | Deterministic_functions -> "Determ\\underline{i}nistic functions"
  | Variants                -> "\\underline{V}ariants"
  | Records                 -> "Re\\underline{c}ords"
  | Recursive_functions     -> "\\underline{R}ecursive functions"
  | Higher_order_functions  -> "\\underline{H}igher order functions"
  | Subtyping               -> "\\underline{S}ubtyping"
  | OOP_style               -> "\\underline{O}OP-style"
  | Return_error            -> "Retur\\underline{n} error"
  | Usage_error             -> "Usa\\underline{g}e error"
  | Other                   -> "Oth\\underline{e}r"

let to_char = function
  | Polymorphic_types       -> 'P'
  | Refinement_types        -> 'F'
  | Dependent_types         -> 'D'
  | Modules                 -> 'M'
  | Mu_types                -> 'U'
  | Parametric_types        -> 'A'
  | First_class_types       -> 'T'
  | Deterministic_functions -> 'I'
  | Variants                -> 'V'
  | Records                 -> 'C'
  | Recursive_functions     -> 'R'
  | Higher_order_functions  -> 'H'
  | Subtyping               -> 'S'
  | OOP_style               -> 'O'
  | Return_error            -> 'N'
  | Usage_error             -> 'G'
  | Other                   -> 'E'
