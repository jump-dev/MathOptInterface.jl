```@meta
CurrentModule = MathOptInterface
DocTestSetup = quote
    import MathOptInterface as MOI
end
DocTestFilters = [r"MathOptInterface|MOI"]
```

# Standard form

## Functions

```@docs
AbstractFunction
output_dimension
constant
```

## Scalar functions

```@docs
AbstractScalarFunction
VariableIndex
ScalarAffineTerm
ScalarAffineFunction
ScalarQuadraticTerm
ScalarQuadraticFunction
ScalarNonlinearFunction
```

## Vector functions

```@docs
AbstractVectorFunction
VectorOfVariables
VectorAffineTerm
VectorAffineFunction
VectorQuadraticTerm
VectorQuadraticFunction
VectorNonlinearFunction
```

## Sets

```@docs
AbstractSet
AbstractScalarSet
AbstractVectorSet
```

### Utilities

```@docs
dimension
dual_set
dual_set_type
constant(s::EqualTo)
supports_dimension_update
update_dimension
```

## Scalar sets

List of recognized scalar sets.
```@docs
GreaterThan
LessThan
EqualTo
Interval
Integer
ZeroOne
Semicontinuous
Semiinteger
Parameter
```

## Vector sets

List of recognized vector sets.
```@docs
Reals
Zeros
Nonnegatives
Nonpositives
NormInfinityCone
NormOneCone
NormCone
SecondOrderCone
RotatedSecondOrderCone
GeometricMeanCone
ExponentialCone
DualExponentialCone
PowerCone
DualPowerCone
RelativeEntropyCone
NormSpectralCone
NormNuclearCone
SOS1
SOS2
Indicator
ActivationCondition
ACTIVATE_ON_ZERO
ACTIVATE_ON_ONE
Complements
HyperRectangle
Scaled
VectorNonlinearOracle
```

## Constraint programming sets

```@docs
AllDifferent
BinPacking
Circuit
CountAtLeast
CountBelongs
CountDistinct
CountGreaterThan
Cumulative
Path
Reified
Table
```

## Matrix sets

Matrix sets are vectorized to be subtypes of [`AbstractVectorSet`](@ref).

For sets of symmetric matrices, storing both the
`(i, j)` and `(j, i)` elements is redundant. Use the
[`AbstractSymmetricMatrixSetTriangle`](@ref) set to represent only the
vectorization of the upper triangular part of the matrix.

When the matrix of expressions constrained to be in the set is not symmetric,
and hence additional constraints are needed to force the equality of the
`(i, j)` and `(j, i)` elements, use the
[`AbstractSymmetricMatrixSetSquare`](@ref) set.

The [`Bridges.Constraint.SquareBridge`](@ref) can transform a set from the
square form to the [`triangular_form`](@ref) by adding appropriate constraints
if the `(i, j)` and `(j, i)` expressions are different.

```@docs
AbstractSymmetricMatrixSetTriangle
AbstractSymmetricMatrixSetSquare
side_dimension
triangular_form
```

List of recognized matrix sets.
```@docs
PositiveSemidefiniteConeTriangle
PositiveSemidefiniteConeSquare
HermitianPositiveSemidefiniteConeTriangle
LogDetConeTriangle
LogDetConeSquare
RootDetConeTriangle
RootDetConeSquare
```
