```@meta
CurrentModule = MathOptInterface
DocTestSetup = quote
    using MathOptInterface
    const MOI = MathOptInterface
end
DocTestFilters = [r"MathOptInterface|MOI"]
```

# Constraints

## Add a constraint

Use [`add_constraint`](@ref) to add a single constraint.

```jldoctest constraints; setup=:(model = MOI.Utilities.Model{Float64}(); x = MOI.add_variables(model, 2))
julia> c = MOI.add_constraint(model, MOI.VectorOfVariables(x), MOI.Nonnegatives(2))
MathOptInterface.ConstraintIndex{MathOptInterface.VectorOfVariables,MathOptInterface.Nonnegatives}(1)
```

[`add_constraint`](@ref) returns a [`ConstraintIndex`](@ref) type, which should
be used to refer to the added constraint in other calls.

Check if a [`ConstraintIndex`](@ref) is valid using [`is_valid`](@ref).

```jldoctest constraints
julia> MOI.is_valid(model, c)
true
```

Use [`add_constraints`](@ref) to add a number of constraints of the same type.

```jldoctest; setup=:(model = MOI.Utilities.Model{Float64}(); x = MOI.add_variables(model, 2))
julia> c = MOI.add_constraints(
           model,
           [MOI.SingleVariable(x[1]), MOI.SingleVariable(x[2])],
           [MOI.GreaterThan(0.0), MOI.GreaterThan(1.0)]
       )
2-element Array{MathOptInterface.ConstraintIndex{MathOptInterface.SingleVariable,MathOptInterface.GreaterThan{Float64}},1}:
 MathOptInterface.ConstraintIndex{MathOptInterface.SingleVariable,MathOptInterface.GreaterThan{Float64}}(1)
 MathOptInterface.ConstraintIndex{MathOptInterface.SingleVariable,MathOptInterface.GreaterThan{Float64}}(2)
```
This time, a vector of [`ConstraintIndex`](@ref) are returned.

Use [`supports_constraint`](@ref) to check if the model supports adding a
constraint type.
```jldoctest; setup=:(model = MOI.Utilities.Model{Float64}())
julia> MOI.supports_constraint(
           model,
           MOI.SingleVariable,
           MOI.GreaterThan{Float64},
       )
true
```

## Delete a constraint

Use [`delete`](@ref) to delete a constraint.

```jldoctest constraints
julia> MOI.delete(model, c)

julia> MOI.is_valid(model, c)
false
```

## Constraint attributes

The following attributes are available for constraints:

* [`ConstraintName`](@ref)
* [`ConstraintPrimalStart`](@ref)
* [`ConstraintDualStart`](@ref)
* [`ConstraintPrimal`](@ref)
* [`ConstraintDual`](@ref)
* [`ConstraintBasisStatus`](@ref)
* [`ConstraintFunction`](@ref)
* [`CanonicalConstraintFunction`](@ref)
* [`ConstraintSet`](@ref)

Get and set these attributes using [`get`](@ref) and [`set`](@ref).

```jldoctest constraints
julia> MOI.set(model, MOI.ConstraintName(), c, "con_c")

julia> MOI.get(model, MOI.ConstraintName(), c)
"con_c"
```

## Constraints by function-set pairs

Below is a list of common constraint types and how they are represented
as function-set pairs in MOI. In the notation below, ``x`` is a vector of
decision variables, ``x_i`` is a scalar decision variable, ``\alpha, \beta`` are
scalar constants, ``a, b`` are constant vectors, `A` is a constant matrix and
``\mathbb{R}_+`` (resp. ``\mathbb{R}_-``) is the set of nonnegative (resp.
nonpositive) real numbers.

### Linear constraints

| Mathematical Constraint       | MOI Function                 | MOI Set        |
|-------------------------------|------------------------------|----------------|
| ``a^Tx \le \beta``            | `ScalarAffineFunction`       | `LessThan`     |
| ``a^Tx \ge \alpha``           | `ScalarAffineFunction`       | `GreaterThan`  |
| ``a^Tx = \beta``              | `ScalarAffineFunction`       | `EqualTo`      |
| ``\alpha \le a^Tx \le \beta`` | `ScalarAffineFunction`       | `Interval`     |
| ``x_i \le \beta``             | `SingleVariable`             | `LessThan`     |
| ``x_i \ge \alpha``            | `SingleVariable`             | `GreaterThan`  |
| ``x_i = \beta``               | `SingleVariable`             | `EqualTo`      |
| ``\alpha \le x_i \le \beta``  | `SingleVariable`             | `Interval`     |
| ``Ax + b \in \mathbb{R}_+^n`` | `VectorAffineFunction`       | `Nonnegatives` |
| ``Ax + b \in \mathbb{R}_-^n`` | `VectorAffineFunction`       | `Nonpositives` |
| ``Ax + b = 0``                | `VectorAffineFunction`       | `Zeros`        |

By convention, solvers are not expected to support nonzero constant terms in the
[`ScalarAffineFunction`](@ref)s the first four rows above, because they are
redundant with the parameters of the sets. For example, ``2x + 1 \le 2`` should
be encoded as ``2x \le 1``.

Constraints with [`SingleVariable`](@ref) in [`LessThan`](@ref), [`GreaterThan`](@ref),
[`EqualTo`](@ref), or [`Interval`](@ref) sets have a natural interpretation as
variable bounds. As such, it is typically not natural to impose multiple lower-
or upper-bounds on the same variable, and the solver interfaces should throw
respectively [`LowerBoundAlreadySet`](@ref) or [`UpperBoundAlreadySet`](@ref).

Moreover, adding two [`SingleVariable`](@ref) constraints on the same variable
with the same set is impossible because they share the same index as it is the
index of the variable, see [`ConstraintIndex`](@ref).

It is natural, however, to impose upper- and lower-bounds separately as two
different constraints on a single variable. The difference between imposing
bounds by using a single [`Interval`](@ref) constraint and by using separate
[`LessThan`](@ref) and [`GreaterThan`](@ref) constraints is that the latter will
allow the solver to return separate dual multipliers for the two bounds, while
the former will allow the solver to return only a single dual for the interval
constraint.

### Conic constraints

| Mathematical Constraint                                       | MOI Function                 | MOI Set                            |
|---------------------------------------------------------------|------------------------------|------------------------------------|
| ``\lVert Ax + b\rVert_2 \le c^Tx + d``                        | `VectorAffineFunction`       | `SecondOrderCone`                  |
| ``y \ge \lVert x \rVert_2``                                   | `VectorOfVariables`          | `SecondOrderCone`                  |
| ``2yz \ge \lVert x \rVert_2^2, y,z \ge 0``                    | `VectorOfVariables`          | `RotatedSecondOrderCone`           |
| ``(a_1^Tx + b_1,a_2^Tx + b_2,a_3^Tx + b_3) \in \mathcal{E}``  | `VectorAffineFunction`       | `ExponentialCone`                  |
| ``A(x) \in \mathcal{S}_+``                                    | `VectorAffineFunction`       | `PositiveSemidefiniteConeTriangle` |
| ``B(x) \in \mathcal{S}_+``                                    | `VectorAffineFunction`       | `PositiveSemidefiniteConeSquare`   |
| ``x \in \mathcal{S}_+``                                       | `VectorOfVariables`          | `PositiveSemidefiniteConeTriangle` |
| ``x \in \mathcal{S}_+``                                       | `VectorOfVariables`          | `PositiveSemidefiniteConeSquare`   |

where ``\mathcal{E}`` is the exponential cone (see [`ExponentialCone`](@ref)),
``\mathcal{S}_+`` is the set of positive semidefinite symmetric matrices,
``A`` is an affine map that outputs symmetric matrices and
``B`` is an affine map that outputs square matrices.

### Quadratic constraints

| Mathematical Constraint       | MOI Function                 | MOI Set                       |
|-------------------------------|------------------------------|-------------------------------|
| ``x^TQx + a^Tx + b \ge 0``    | `ScalarQuadraticFunction`    | `GreaterThan`                 |
| ``x^TQx + a^Tx + b \le 0``    | `ScalarQuadraticFunction`    | `LessThan`                    |
| ``x^TQx + a^Tx + b = 0``      | `ScalarQuadraticFunction`    | `EqualTo`                     |
| Bilinear matrix inequality    | `VectorQuadraticFunction`    | `PositiveSemidefiniteCone...` |

### Discrete and logical constraints

| Mathematical Constraint                                                                    | MOI Function           | MOI Set                            |
|--------------------------------------------------------------------------------------------|------------------------|------------------------------------|
| ``x_i \in \mathbb{Z}``                                                                     | `SingleVariable`       | `Integer`                          |
| ``x_i \in \{0,1\}``                                                                        | `SingleVariable`       | `ZeroOne`                          |
| ``x_i \in \{0\} \cup [l,u]``                                                               | `SingleVariable`       | `Semicontinuous`                   |
| ``x_i \in \{0\} \cup \{l,l+1,\ldots,u-1,u\}``                                              | `SingleVariable`       | `Semiinteger`                      |
| At most one component of ``x`` can be nonzero                                              | `VectorOfVariables`    | `SOS1`                             |
| At most two components of ``x`` can be nonzero, and if so they must be adjacent components | `VectorOfVariables`    | `SOS2`                             |
| ``y = 1 \implies a^T x \in S``                                                             | `VectorAffineFunction` | `IndicatorSet`                     |

## JuMP mapping

The following bullet points show examples of how JuMP constraints are translated
into MOI function-set pairs:

 - `@constraint(m, 2x + y <= 10)` becomes `ScalarAffineFunction`-in-`LessThan`
 - `@constraint(m, 2x + y >= 10)` becomes `ScalarAffineFunction`-in-`GreaterThan`
 - `@constraint(m, 2x + y == 10)` becomes `ScalarAffineFunction`-in-`EqualTo`
 - `@constraint(m, 0 <= 2x + y <= 10)` becomes `ScalarAffineFunction`-in-`Interval`
 - `@constraint(m, 2x + y in ArbitrarySet())` becomes
   `ScalarAffineFunction`-in-`ArbitrarySet`.

Variable bounds are handled in a similar fashion:

 - `@variable(m, x <= 1)` becomes `SingleVariable`-in-`LessThan`
 - `@variable(m, x >= 1)` becomes `SingleVariable`-in-`GreaterThan`

One notable difference is that a variable with an upper and lower bound is
translated into two constraints, rather than an interval. i.e.:

 - `@variable(m, 0 <= x <= 1)` becomes `SingleVariable`-in-`LessThan` *and*
    `SingleVariable`-in-`GreaterThan`.
