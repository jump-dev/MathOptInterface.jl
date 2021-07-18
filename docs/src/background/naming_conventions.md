# Naming conventions

MOI follows several conventions for naming functions and structures. Ideally, 
these should also be followed by packages extending MOI.

## Sets

Sets encode the structure of constraints. Their names should follow the 
following conventions: 

* The name of concrete sets does not end with `Set`, but the name of abstract
  sets end with `Set`. For instance, `MOI.ZeroOne` is a concrete set
  (it can be instantiated by the user), but `MOI.AbstractScalarSet` is an 
  abstract set (many sets should have it as supertype; an 
  `MOI.AbstractScalarSet` cannot be instantiated).
* The name of conic sets ends with `Cone`, like `NormInfinityCone`, unless 
  the set is matrix-valued. 
* Matrix-valued conic sets provide two versions: one for a standard 
  rectangular representation, whose name ends with `ConeSquare` (like
  `MOI.LogDetConeSquare`); one with a packed representation considering that
  the matrix is symmetric, whose name ends with `ConeTriangle` 
  (like `MOI.PositiveSemidefiniteConeTriangle`).
* The name of the set is singular, instead of plural: prefer `Integer` to 
  `Integers`.
* The sets are named with nouns instead of verbs. Usual sets in mixed-integer
  and convex programming only have such names.
* As much as possible, the names should follow established conventions in the 
  domain where this set is used: for instance, convex sets should have names 
  close to those of CVX, and constraint-programming sets should follow 
  MiniZinc's constraints.
