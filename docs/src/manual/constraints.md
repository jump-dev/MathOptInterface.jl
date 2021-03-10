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

All constraints are specified with [`add_constraint`](@ref) by restricting the
output of some function to a set. The interface allows an arbitrary combination
of functions and sets, but of course solvers may decide to support only a small
number of combinations.

For example, linear programming solvers should support, at least, combinations
of affine functions with the [`LessThan`](@ref) and [`GreaterThan`](@ref) sets.
These are simply linear constraints. [`SingleVariable`](@ref) functions combined
with these same sets are used to specify upper- and lower-bounds on variables.

The code example below encodes the linear optimization problem:
```math
\begin{align}
& \max_{x \in \mathbb{R}^2} & 3x_1 + 2x_2 &
\\
& \;\;\text{s.t.} & x_1 + x_2 &\le 5
\\
&& x_1 & \ge 0
\\
&&x_2 & \ge -1
\end{align}
```

```julia
x = MOI.add_variables(model, 2)
MOI.set(
    model,
    MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([3.0, 2.0], x), 0.0),
)
MOI.set(model, MOI.ObjectiveSense(), MAX_SENSE)
MOI.add_constraint(
    model,
    MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(1.0, x), 0.0),
    MOI.LessThan(5.0),
)
MOI.add_constraint(model, MOI.SingleVariable(x[1]), MOI.GreaterThan(0.0))
MOI.add_constraint(model, MOI.SingleVariable(x[2]), MOI.GreaterThan(-1.0))
```

Besides scalar-valued functions in scalar-valued sets, it's also possible to use
vector-valued functions and sets.

The code example below encodes the convex optimization problem:
```math
\begin{align}
& \max_{x,y,z \in \mathbb{R}} & y + z &
\\
& \;\;\text{s.t.} & 3x &= 2
\\
&& x & \ge \lVert (y,z) \rVert_2
\end{align}
```

```julia
x,y,z = MOI.add_variables(model, 3)
MOI.set(
    model,
    MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(1.0, [y, z]), 0.0),
)
MOI.set(model, ObjectiveSense(), MAX_SENSE)
MOI.add_constraint(
    model,
    MOI.VectorAffineFunction(
        [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(3.0, x))], [-2.0]
    ),
    MOI.Zeros(1),
)
MOI.add_constraint(
    model, MOI.VectorOfVariables([x, y, z]), MOI.SecondOrderCone(3)
)
```

[TODO Describe ConstraintIndex objects.]

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
