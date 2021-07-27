# Naming conventions

MOI follows several conventions for naming functions and structures. These 
should also be followed by packages extending MOI.

## Sets

Sets encode the structure of constraints. Their names should follow the 
following conventions: 

* Abstract types in the set hierarchy should begin with `Abstract` and end in
  `Set`, e.g., [`AbstractScalarSet`](@ref), [`AbstractVectorSet`](@ref).
* Vector-valued conic sets should end with `Cone`, e.g.,
  [`NormInfinityCone`](@ref), [`SecondOrderCone`](@ref).
* Vector-valued Cartesian products should be plural and not end in `Cone`,
  e.g., [`Nonnegatives`](@ref), not `NonnegativeCone`.
* Matrix-valued conic sets should provide two representations: `ConeSquare` and
  `ConeTriangle`, e.g., [`RootDetConeTriangle`](@ref) and
  [`RootDetConeSquare`](@ref). See [Matrix cones](@ref) for more details.
* Scalar sets should be singular, not plural, e.g., [`Integer`](@ref), not 
  `Integers`.
* As much as possible, the names should follow established conventions in the 
  domain where this set is used: for instance, convex sets should have names 
  close to those of [CVX](http://web.cvxr.com/cvx/doc/), and 
  constraint-programming sets should follow 
  [MiniZinc](https://www.minizinc.org/doc-latest/en/)'s constraints.
