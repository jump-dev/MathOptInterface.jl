```@meta
CurrentModule = MathOptInterface
DocTestSetup = quote
    using MathOptInterface
    const MOI = MathOptInterface
end
DocTestFilters = [r"MathOptInterface|MOI"]
```

# Standard form

## Functions

```@docs
AbstractFunction
AbstractScalarFunction
AbstractVectorFunction
SingleVariable
VectorOfVariables
ScalarAffineTerm
ScalarAffineFunction
VectorAffineTerm
VectorAffineFunction
ScalarQuadraticTerm
ScalarQuadraticFunction
VectorQuadraticTerm
VectorQuadraticFunction
```

### Utilities

```@docs
output_dimension
constant(f::Union{ScalarAffineFunction, ScalarQuadraticFunction})
constant(f::Union{VectorAffineFunction, VectorQuadraticFunction})
constant(f::VariableIndex, ::Type)
constant(f::VectorOfVariables, T::Type)
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
Complements
```

## Matrix sets

Matrix sets are vectorized in order to be subtypes of
[`AbstractVectorSet`](@ref). For sets of symmetric matrices, storing both the
`(i, j)` and `(j, i)` elements is redundant so there exists the
[`AbstractSymmetricMatrixSetTriangle`](@ref) set to represent only the
vectorization of the upper triangular part of the matrix. When the matrix
of expressions constrained to be in the set is not symmetric and hence
the `(i, j)` and `(j, i)` elements should be constrained to be symmetric,
the [`AbstractSymmetricMatrixSetSquare`](@ref) set can be used. The
[`Bridges.Constraint.SquareBridge`](@ref) can transform a set from the square
form to the [`triangular_form`](@ref) by adding appropriate constraints if
the `(i, j)` and `(j, i)` expressions are different.

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
LogDetConeTriangle
LogDetConeSquare
RootDetConeTriangle
RootDetConeSquare
```
