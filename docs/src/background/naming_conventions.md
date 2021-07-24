# Naming conventions

MOI follows several conventions for naming functions and structures. These 
should also be followed by packages extending MOI.

## Sets

Sets encode the structure of constraints. Their names should follow the 
following conventions: 

* Abstract types in the set hierarchy should begin with `Abstract` and end in
  `Set`, e.g., [`MOI.AbstractScalarSet`](@ref), [`MOI.AbstractVectorSet`](@ref).
* Vector-valued conic sets should end with `Cone`, e.g.,
  [`MOI.NormInfinityCone`](@ref), [`MOI.SecondOrderCone`](@ref).
* Vector-valued Cartesian products should be plural and not end in `Cone`,
  e.g., [`MOI.Nonnegatives`](@ref), not `MOI.NonnegativeCone`.
* Matrix-valued conic sets should provide two representations: `ConeSquare` and
  `ConeTriangle`, e.g., [`MOI.RootDetConeTriangle`](@ref) and
  [`MOI.RootDetConeSquare`](@ref). See [Matrix cones](@ref) for more details.
* Scalar sets should be singular, not plural, e.g., [`MOI.Integer`](@ref), not 
  `MOI.Integers`.
* The sets are named with nouns instead of verbs. Usual sets in mixed-integer
  and convex programming only have such names.
* As much as possible, the names should follow established conventions in the 
  domain where this set is used: for instance, convex sets should have names 
  close to those of [CVX](http://web.cvxr.com/cvx/doc/), and 
  constraint-programming sets should follow 
  [MiniZinc](https://www.minizinc.org/doc-latest/en/)'s constraints.
