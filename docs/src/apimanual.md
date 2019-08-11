```@meta
CurrentModule = MathOptInterface
DocTestSetup = quote
    using MathOptInterface
    const MOI = MathOptInterface
end
```

# Manual

## Purpose

Each mathematical optimization solver API has its own concepts and data structures for representing optimization models and obtaining results.
However, it is often desirable to represent an instance of an optimization problem at a higher level so that it is easy to try using different solvers.
MathOptInterface (MOI) is an abstraction layer designed to provide a unified interface to mathematical optimization solvers so that users do not need to understand multiple solver-specific APIs.
MOI can be used directly, or through a higher-level modeling interface like [JuMP](https://github.com/JuliaOpt/JuMP.jl).

MOI has been designed to replace [MathProgBase](https://github.com/JuliaOpt/MathProgBase.jl), which has been used by modeling packages such as [JuMP](https://github.com/JuliaOpt/JuMP.jl) and [Convex.jl](https://github.com/JuliaOpt/Convex.jl).
This second-generation abstraction layer addresses a number of limitations of MathProgBase.
MOI is designed to:
- Be simple and extensible, unifying linear, quadratic, and conic optimization, and seamlessly facilitate extensions to essentially arbitrary constraints and functions (e.g., indicator constraints, complementarity constraints, and piecewise linear functions)
- Be fast by allowing access to a solver's in-memory representation of a problem without writing intermediate files (when possible) and by using multiple dispatch and avoiding requiring containers of nonconcrete types
- Allow a solver to return multiple results (e.g., a pool of solutions)
- Allow a solver to return extra arbitrary information via attributes (e.g., variable- and constraint-wise membership in an irreducible inconsistent subset for infeasibility analysis)
- Provide a greatly expanded set of status codes explaining what happened during the optimization procedure
- Enable a solver to more precisely specify which problem classes it supports
- Enable both primal and dual warm starts
- Enable adding and removing both variables and constraints by indices that are not required to be consecutive
- Enable any modification that the solver supports to an existing model
- Avoid requiring the solver wrapper to store an additional copy of the problem data

This manual introduces the concepts needed to understand MOI and give a high-level picture of how all of the pieces fit together. The primary focus is on MOI from the perspective of a user of the interface. At the end of the manual we have a section on [Implementing a solver interface](@ref).
The [API Reference](@ref) page lists the complete API.

MOI does not export functions, but for brevity we often omit qualifying names with the MOI module. Best practice is to have
```julia
using MathOptInterface
const MOI = MathOptInterface
```
and prefix all MOI methods with `MOI.` in user code. If a name is also available in base Julia, we always explicitly use the module prefix, for example, with `MOI.get`.

## Standard form problem

The standard form problem is:

```math
\begin{align}
    & \min_{x \in \mathbb{R}^n} & f_0(x)
    \\
    & \;\;\text{s.t.} & f_i(x) & \in \mathcal{S}_i & i = 1 \ldots m
\end{align}
```

where:
* the functions ``f_0, f_1, \ldots, f_m`` are specified by
  [`AbstractFunction`](@ref) objects
* the sets ``\mathcal{S}_1, \ldots, \mathcal{S}_m`` are specified by
  [`AbstractSet`](@ref) objects

The current function types are:
* **[`SingleVariable`](@ref)**: ``x_j``, i.e., projection onto a single
  coordinate defined by a variable index ``j``
* **[`VectorOfVariables`](@ref)**: projection onto multiple coordinates (i.e.,
  extracting a subvector)
* **[`ScalarAffineFunction`](@ref)**: ``a^T x + b``, where ``a`` is a vector and
  ``b`` scalar
* **[`VectorAffineFunction`](@ref)**: ``A x + b``, where ``A`` is a matrix and
  ``b`` is a vector
* **[`ScalarQuadraticFunction`](@ref)**: ``\frac{1}{2} x^T Q x + a^T x + b``,
  where ``Q`` is a symmetric matrix, ``a`` is a vector, and ``b`` is a constant
* **[`VectorQuadraticFunction`](@ref)**: a vector of scalar-valued quadratic
  functions

Extensions for nonlinear programming are present but not yet well documented.

MOI defines some commonly used sets, but the interface is extensible to other
sets recognized by the solver.

* **[`LessThan(upper)`](@ref MathOptInterface.LessThan)**:
  ``\{ x \in \mathbb{R} : x \le \mbox{upper} \}``
* **[`GreaterThan(lower)`](@ref MathOptInterface.GreaterThan)**:
  ``\{ x \in \mathbb{R} : x \ge \mbox{lower} \}``
* **[`EqualTo(value)`](@ref MathOptInterface.GreaterThan)**:
  ``\{ x \in \mathbb{R} : x = \mbox{value} \}``
* **[`Interval(lower, upper)`](@ref MathOptInterface.Interval)**:
  ``\{ x \in \mathbb{R} : x \in [\mbox{lower},\mbox{upper}] \}``
* **[`Reals(dimension)`](@ref MathOptInterface.Reals)**:
  ``\mathbb{R}^\mbox{dimension}``
* **[`Zeros(dimension)`](@ref MathOptInterface.Zeros)**: ``0^\mbox{dimension}``
* **[`Nonnegatives(dimension)`](@ref MathOptInterface.Nonnegatives)**:
  ``\{ x \in \mathbb{R}^\mbox{dimension} : x \ge 0 \}``
* **[`Nonpositives(dimension)`](@ref MathOptInterface.Nonpositives)**:
  ``\{ x \in \mathbb{R}^\mbox{dimension} : x \le 0 \}``
* **[`SecondOrderCone(dimension)`](@ref MathOptInterface.SecondOrderCone)**:
  ``\{ (t,x) \in \mathbb{R}^\mbox{dimension} : t \ge ||x||_2 \}``
* **[`RotatedSecondOrderCone(dimension)`](@ref MathOptInterface.RotatedSecondOrderCone)**:
  ``\{ (t,u,x) \in \mathbb{R}^\mbox{dimension} : 2tu \ge ||x||_2^2, t,u \ge 0 \}``
* **[`GeometricMeanCone(dimension)`](@ref MathOptInterface.GeometricMeanCone)**:
  ``\{ (t,x) \in \mathbb{R}^{n+1} : x \ge 0, t \le \sqrt[n]{x_1 x_2 \cdots x_n} \}``
  where ``n`` is ``dimension - 1``
* **[`ExponentialCone()`](@ref MathOptInterface.ExponentialCone)**:
  ``\{ (x,y,z) \in \mathbb{R}^3 : y \exp (x/y) \le z, y > 0 \}``
* **[`DualExponentialCone()`](@ref MathOptInterface.DualExponentialCone)**:
  ``\{ (u,v,w) \in \mathbb{R}^3 : -u \exp (v/u) \le exp(1) w, u < 0 \}``
* **[`PowerCone(exponent)`](@ref MathOptInterface.PowerCone)**:
  ``\{ (x,y,z) \in \mathbb{R}^3 : x^\mbox{exponent} y^{1-\mbox{exponent}} \ge |z|, x,y \ge 0 \}``
* **[`DualPowerCone(exponent)`](@ref MathOptInterface.DualPowerCone)**:
  ``\{ (u,v,w) \in \mathbb{R}^3 : \frac{u}{\mbox{exponent}}^\mbox{exponent}
  \frac{v}{1-\mbox{exponent}}^{1-\mbox{exponent}} \ge |w|, u,v \ge 0 \}``
* **[`PositiveSemidefiniteConeTriangle(dimension)`](@ref MathOptInterface.PositiveSemidefiniteConeTriangle)**:
  ``\{ X \in \mathbb{R}^{\mbox{dimension}(\mbox{dimension}+1)/2} : X \mbox{is
  the upper triangle of a PSD matrix }\}``
* **[`PositiveSemidefiniteConeSquare(dimension)`](@ref MathOptInterface.PositiveSemidefiniteConeSquare)**:
  ``\{ X \in \mathbb{R}^{\mbox{dimension}^2} : X \mbox{is a PSD matrix} \}``
* **[`LogDetConeTriangle(dimension)`](@ref MathOptInterface.LogDetConeTriangle)**:
  ``\{ (t,u,X) \in \mathbb{R}^{2+\mbox{dimension}(1+\mbox{dimension})/2} : t \le
  u\log(\det(X/u)), X \mbox{is the upper triangle of a PSD matrix}, u > 0 \}``
* **[`LogDetConeSquare(dimension)`](@ref MathOptInterface.LogDetConeSquare)**:
  ``\{ (t,u,X) \in \mathbb{R}^{2+\mbox{dimension}^2} : t \le u \log(\det(X/u)),
  X \mbox{is a PSD matrix}, u > 0 \}``
* **[`RootDetConeTriangle(dimension)`](@ref MathOptInterface.RootDetConeTriangle)**:
  ``\{ (t,X) \in \mathbb{R}^{1+\mbox{dimension}(1+\mbox{dimension})/2} : t \le
  det(X)^{1/\mbox{dimension}}, X \mbox{is the upper triangle of a PSD matrix} \}``
* **[`RootDetConeSquare(dimension)`](@ref MathOptInterface.RootDetConeSquare)**:
  ``\{ (t,X) \in \mathbb{R}^{1+\mbox{dimension}^2} : t \le
  \det(X)^{1/\mbox{dimension}}, X \mbox{is a PSD matrix} \}``
* **[`Integer()`](@ref MathOptInterface.Integer)**: ``\mathbb{Z}``
* **[`ZeroOne()`](@ref MathOptInterface.ZeroOne)**: ``\{ 0, 1 \}``
* **[`Semicontinuous(lower,upper)`](@ref MathOptInterface.Semicontinuous)**:
  ``\{ 0\} \cup [lower,upper]``
* **[`Semiinteger(lower,upper)`](@ref MathOptInterface.Semiinteger)**:
  ``\{ 0\} \cup \{lower,lower+1,\ldots,upper-1,upper\}``


## The `ModelLike` and `AbstractOptimizer` APIs

The most significant part of MOI is the definition of the **model API** that is
used to specify an instance of an optimization problem (e.g., by adding
variables and constraints). Objects that implement the model API should inherit
from the [`ModelLike`](@ref) abstract type.

Notably missing from the model API is the method to solve an optimization problem.
`ModelLike` objects may store an instance (e.g., in memory or backed by a file format)
without being linked to a particular solver. In addition to the model API, MOI
defines [`AbstractOptimizer`](@ref). *Optimizers* (or solvers) implement the
model API (inheriting from `ModelLike`) and additionally provide methods to
solve the model.

Through the rest of the manual, `model` is used as a generic `ModelLike`, and
`optimizer` is used as a generic `AbstractOptimizer`.

Models are constructed by
* adding variables using [`add_variables`](@ref) (or [`add_variables`](@ref)),
  see [Adding variables](@ref);
* setting an objective sense and function using [`set`](@ref),
  see [Setting an objective](@ref);
* and adding constraints using [`add_constraint`](@ref) (or
  [`add_constraints`](@ref)), see [Sets and Constraints](@ref).

The way the problem is solved by the optimimizer is controlled by
[`AbstractOptimizerAttribute`](@ref)s, see [Solver-specific attributes](@ref).

## Adding variables

All variables in MOI are scalar variables.
New scalar variables are created with [`add_variable`](@ref) or
[`add_variables`](@ref), which return a [`VariableIndex`](@ref) or
`Vector{VariableIndex}` respectively. `VariableIndex` objects are type-safe
wrappers around integers that refer to a variable in a particular model.

One uses `VariableIndex` objects to set and get variable attributes. For
example, the [`VariablePrimalStart`](@ref) attribute is used to provide an
initial starting point for a variable or collection of variables:
```julia
v = add_variable(model)
set(model, VariablePrimalStart(), v, 10.5)
v2 = add_variables(model, 3)
set(model, VariablePrimalStart(), v2, [1.3,6.8,-4.6])
```

A variable can be deleted from a model with
[`delete(::ModelLike, ::VariableIndex)`](@ref MathOptInterface.delete(::MathOptInterface.ModelLike, ::MathOptInterface.Index)).
Not all models support deleting variables; an [`DeleteNotAllowed`](@ref)
error is thrown if this is not supported.

## Functions

MOI defines six functions as listed in the definition of the
[Standard form problem](@ref). The simplest function is [`SingleVariable`](@ref)
defined as:
```julia
struct SingleVariable <: AbstractFunction
    variable::VariableIndex
end
```

If `v` is a `VariableIndex` object, then `SingleVariable(v)` is simply the scalar-valued function from the complete set of variables in a model that returns the value of variable `v`. One may also call this function a coordinate projection, which is more useful for defining constraints than as an objective function.


A more interesting function is [`ScalarAffineFunction`](@ref), defined as
```julia
struct ScalarAffineFunction{T} <: AbstractScalarFunction
    terms::Vector{ScalarAffineTerm{T}}
    constant::T
end
```

The [`ScalarAffineTerm`](@ref) struct defines
a variable-coefficient pair:
```julia
struct ScalarAffineTerm{T}
    coefficient::T
    variable_index::VariableIndex
end
```

If `x` is a vector of `VariableIndex` objects, then
`ScalarAffineFunction(ScalarAffineTerm.([5.0,-2.3],[x[1],x[2]]),1.0)` represents
the function ``5x_1 - 2.3x_2 + 1``.

!!! note

    `ScalarAffineTerm.([5.0,-2.3],[x[1],x[2]])` is a shortcut for
    `[ScalarAffineTerm(5.0, x[1]), ScalarAffineTerm(-2.3, x[2])]`. This is
    Julia's broadcast syntax and is used quite often.

### Setting an objective

Objective functions are assigned to a model by setting the
[`ObjectiveFunction`](@ref) attribute. The [`ObjectiveSense`](@ref) attribute is
used for setting the optimization sense.
For example,
```julia
x = add_variables(model, 2)
set(model, ObjectiveFunction{ScalarAffineFunction{Float64}}(),
            ScalarAffineFunction(ScalarAffineTerm.([5.0,-2.3],[x[1],x[2]]),1.0))
set(model, ObjectiveSense(), MIN_SENSE)
```
sets the objective to the function just discussed in the minimization sense.

See [Functions and function modifications](@ref) for the complete list of
functions.

## Sets and Constraints

All constraints are specified with [`add_constraint`](@ref) by restricting the
output of some function to a set. The interface allows an arbitrary combination
of functions and sets, but of course solvers may decide to support only a small
number of combinations.

For example, linear programming solvers should support, at least, combinations
of affine functions with the [`LessThan`](@ref) and [`GreaterThan`](@ref) sets.
These are simply linear constraints. `SingleVariable` functions combined with
these same sets are used to specify upper and lower bounds on variables.

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
x = add_variables(model, 2)
set(model, ObjectiveFunction{ScalarAffineFunction{Float64}}(),
            ScalarAffineFunction(ScalarAffineTerm.([3.0, 2.0], x), 0.0))
set(model, ObjectiveSense(), MAX_SENSE)
add_constraint(model, ScalarAffineFunction(ScalarAffineTerm.(1.0, x), 0.0),
                      LessThan(5.0))
add_constraint(model, SingleVariable(x[1]), GreaterThan(0.0))
add_constraint(model, SingleVariable(x[2]), GreaterThan(-1.0))
```

Besides scalar-valued functions in scalar-valued sets it possible to use vector-valued functions and sets.

The code example below encodes the convex optimization problem:
```math
\begin{align}
& \max_{x,y,z \in \mathbb{R}} & y + z &
\\
& \;\;\text{s.t.} & 3x &= 2
\\
&& x & \ge ||(y,z)||_2
\end{align}
```

```julia
x,y,z = add_variables(model, 3)
set(model, ObjectiveFunction{ScalarAffineFunction{Float64}}(),
            ScalarAffineFunction(ScalarAffineTerm.(1.0, [y,z]), 0.0))
set(model, ObjectiveSense(), MAX_SENSE)
vector_terms = [VectorAffineTerm(1, ScalarAffineTerm(3.0, x))]
add_constraint(model, VectorAffineFunction(vector_terms,[-2.0]), Zeros(1))
add_constraint(model, VectorOfVariables([x,y,z]), SecondOrderCone(3))
```

[Describe `ConstraintIndex` objects.]

### Constraints by function-set pairs

Below is a list of common constraint types and how they are represented
as function-set pairs in MOI. In the notation below, ``x`` is a vector of
decision variables, ``x_i`` is a scalar decision variable, ``\alpha, \beta`` are
scalar constants, ``a, b`` are constant vectors, `A` is a constant matrix and
``\mathbb{R}_+`` (resp. ``\mathbb{R}_-``) is the set of nonnegative (resp.
nonpositive) real numbers.

#### Linear constraints

| Mathematical Constraint       | MOI Function                 | MOI Set        |
|-------------------------------|------------------------------|----------------|
| ``a^Tx \le u``                | `ScalarAffineFunction`       | `LessThan`     |
| ``a^Tx \ge l``                | `ScalarAffineFunction`       | `GreaterThan`  |
| ``a^Tx = b``                  | `ScalarAffineFunction`       | `EqualTo`      |
| ``\alpha \le a^Tx \le \beta`` | `ScalarAffineFunction`       | `Interval`     |
| ``x_i \le \beta               | `SingleVariable`             | `LessThan`     |
| ``x_i \ge \alpha              | `SingleVariable`             | `GreaterThan`  |
| ``x_i = \beta                 | `SingleVariable`             | `EqualTo`      |
| ``\alpha \le x_i \le \beta    | `SingleVariable`             | `Interval`     |
| ``Ax + b \in \mathbb{R}_+^n`` | `VectorAffineFunction`       | `Nonnegatives` |
| ``Ax + b \in \mathbb{R}_-^n`` | `VectorAffineFunction`       | `Nonpositives` |
| ``Ax + b = 0``                | `VectorAffineFunction`       | `Zeros`        |

By convention, solvers are not expected to support nonzero constant terms in the `ScalarAffineFunction`s the first four rows above, because they are redundant with the parameters of the sets. For example, ``2x + 1 \le 2`` should be encoded as ``2x \le 1``.

Constraints with `SingleVariable` in `LessThan`, `GreaterThan`, `EqualTo`, or `Interval` sets have a natural interpretation as variable bounds. As such, it is typically not natural to impose multiple lower or upper bounds on the same variable, and by convention we do not ask solver interfaces to support this. It is natural, however, to impose upper and lower bounds separately as two different constraints on a single variable. The difference between imposing bounds by using a single `Interval` constraint and by using separate `LessThan` and `GreaterThan` constraints is that the latter will allow the solver to return separate dual multipliers for the two bounds, while the former will allow the solver to return only a single dual for the interval constraint.

#### Conic constraints


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

#### Quadratic constraints


| Mathematical Constraint       | MOI Function                 | MOI Set                       |
|-------------------------------|------------------------------|-------------------------------|
| ``x^TQx + a^Tx + b \ge 0``    | `ScalarQuadraticFunction`    | `GreaterThan`                 |
| ``x^TQx + a^Tx + b \le 0``    | `ScalarQuadraticFunction`    | `LessThan`                    |
| ``x^TQx + a^Tx + b = 0``      | `ScalarQuadraticFunction`    | `EqualTo`                     |
| Bilinear matrix inequality    | `VectorQuadraticFunction`    | `PositiveSemidefiniteCone...` |


#### Discrete and logical constraints


| Mathematical Constraint                                                                    | MOI Function           | MOI Set                            |
|--------------------------------------------------------------------------------------------|------------------------|------------------------------------|
| ``x_i \in \mathbb{Z}``                                                                     | `SingleVariable`       | `Integer`                          |
| ``x_i \in \{0,1\}``                                                                        | `SingleVariable`       | `ZeroOne`                          |
| ``x_i \in \{0\} \cup [l,u]``                                                               | `SingleVariable`       | `Semicontinuous`                   |
| ``x_i \in \{0\} \cup \{l,l+1,\ldots,u-1,u\}``                                              | `SingleVariable`       | `Semiinteger`                      |
| At most one component of ``x`` can be nonzero                                              | `VectorOfVariables`    | `SOS1`                             |
| At most two components of ``x`` can be nonzero, and if so they must be adjacent components | `VectorOfVariables`    | `SOS2`                             |
| ``y = 1 \implies a^T x \in S``                                                             | `VectorAffineFunction` | `IndicatorSet`                     |

## Solving and retrieving the results

Once an optimizer is loaded with the objective function and all of the
constraints, we can ask the solver to solve the model by calling
[`optimize!`](@ref).
```julia
optimize!(optimizer)
```

The optimization procedure may terminate for a number of reasons. The
[`TerminationStatus`](@ref) attribute of the optimizer returns a
[`TerminationStatusCode`](@ref) object which explains why the solver stopped.
The termination statuses distinguish between proofs of optimality,
infeasibility, local convergence, limits, and termination because of something
unexpected like invalid problem data or failure to converge. A typical usage of
the `TerminationStatus` attribute is as follows:
```julia
status = MOI.get(optimizer, TerminationStatus())
if status == MOI.OPTIMAL
    # Ok, we solved the problem!
else
    # Handle other cases.
end
```

After checking the `TerminationStatus`, one should typically check
[`ResultCount`](@ref). This attribute returns the number of results that the
solver has available to return. *A result is defined as a primal-dual pair,
but either the primal or the dual may be missing from the result.* While the
`OPTIMAL` termination status normally implies that at least one result is
available, other statuses do not. For example, in the case of infeasiblity,
a solver may return no result or a proof of infeasibility. The `ResultCount`
distinguishes between these two cases.

The [`PrimalStatus`](@ref) and [`DualStatus`](@ref) attributes return a
[`ResultStatusCode`](@ref) that indicates if that component of the result
is present (i.e., not `NO_SOLUTION`) and explains how to interpret the result.

If `PrimalStatus` is not `NO_SOLUTION`, then the primal may be retrieved with the
[`VariablePrimal`](@ref) attribute:
```julia
MOI.get(optimizer, VariablePrimal(), x)
```
If `x` is a `VariableIndex` then the function call returns a scalar, and if `x` is a `Vector{VariableIndex}` then the call returns a vector of scalars. `VariablePrimal()` is equivalent to `VariablePrimal(1)`, i.e., the variable primal vector of the first result. Use `VariablePrimal(N)` to access the `N`th result.

See also the attributes [`ConstraintPrimal`](@ref), and
[`ConstraintDual`](@ref).
See [Duals](@ref) for a discussion of the MOI conventions for primal-dual pairs
and certificates.

!!! note
    We omit discussion of how to handle multiple results, i.e., when
    `ResultCount` is greater than 1. This is supported in the API but not yet
    implemented in any solver.

### Common status situations

The sections below describe how to interpret typical or interesting status cases
for three common classes of solvers. The example cases are illustrative, not
comprehensive. Solver wrappers may provide additional information on
how the solver's statuses map to MOI statuses.

`?` in the tables indicate that multiple different values are possible.

#### Primal-dual convex solver

Linear programming and conic optimization solvers fall into this category.

| What happened?                          | `TerminationStatus()` | `ResultCount()` | `PrimalStatus()`                            | `DualStatus()`                              |
| --------------------------------------- | --------------------- | --------------- | ------------------------------------------- | ------------------------------------------- |
| Proved optimality                       | `OPTIMAL`             | 1               | `FEASIBLE_POINT`                            | `FEASIBLE_POINT`                            |
| Proved infeasible                       | `INFEASIBLE`          | 1               | `NO_SOLUTION`                               | `INFEASIBILITY_CERTIFICATE`                 |
| Optimal within relaxed tolerances       | `ALMOST_OPTIMAL`      | 1               | `FEASIBLE_POINT` or `ALMOST_FEASIBLE_POINT` | `FEASIBLE_POINT` or `ALMOST_FEASIBLE_POINT` |
| Detected an unbounded ray of the primal | `DUAL_INFEASIBLE`     | 1               | `INFEASIBILITY_CERTIFICATE`                 | `NO_SOLUTION`                               |
| Stall                                   | `SLOW_PROGRESS`       | 1               | ?                                           | ?                                           |

#### Global branch-and-bound solvers

Mixed-integer programming solvers fall into this category.

| What happened?                                   | `TerminationStatus()`     | `ResultCount()` | `PrimalStatus()`   | `DualStatus()` |
| ------------------------------------------------ | ------------------------- | --------------- | ------------------ | -------------- |
| Proved optimality                                | `OPTIMAL`                 | 1               | `FEASIBLE_POINT`   | `NO_SOLUTION`  |
| Presolve detected infeasibility or unboundedness | `INFEASIBLE_OR_UNBOUNDED` | 0               | `NO_SOLUTION`      | `NO_SOLUTION`  |
| Proved infeasibility                             | `INFEASIBLE`              | 0               | `NO_SOLUTION`      | `NO_SOLUTION`  |
| Timed out (no solution)                          | `TIME_LIMIT`              | 0               | `NO_SOLUTION`      | `NO_SOLUTION`  |
| Timed out (with a solution)                      | `TIME_LIMIT`              | 1               | `FEASIBLE_POINT`   | `NO_SOLUTION`  |
| `CPXMIP_OPTIMAL_INFEAS`                          | `ALMOST_OPTIMAL`          | 1               | `INFEASIBLE_POINT` | `NO_SOLUTION`  |

[`CPXMIP_OPTIMAL_INFEAS`](https://www.ibm.com/support/knowledgecenter/en/SSSA5P_12.6.1/ilog.odms.cplex.help/refcallablelibrary/macros/CPXMIP_OPTIMAL_INFEAS.html)
is a CPLEX status that indicates that a preprocessed problem was solved to
optimality, but the solver was unable to recover a feasible solution to the
original problem.

#### Local search solvers

Nonlinear programming solvers fall into this category. It also includes
non-global tree search solvers like
[Juniper](https://github.com/lanl-ansi/Juniper.jl).

| What happened?                                         | `TerminationStatus()`             | `ResultCount()` | `PrimalStatus()`   | `DualStatus()`   |
| ------------------------------------------------------ | --------------------------------- | --------------- | ------------------ | ---------------- |
| Converged to a stationary point                        | `LOCALLY_SOLVED`                  | 1               | `FEASIBLE_POINT`   | `FEASIBLE_POINT` |
| Completed a non-global tree search (with a solution)   | `LOCALLY_SOLVED`                  | 1               | `FEASIBLE_POINT`   | `FEASIBLE_POINT` |
| Converged to an infeasible point                       | `LOCALLY_INFEASIBLE`              | 1               | `INFEASIBLE_POINT` | ?                |
| Completed a non-global tree search (no solution found) | `LOCALLY_INFEASIBLE`              | 0               | `NO_SOLUTION`      | `NO_SOLUTION`    |
| Iteration limit                                        | `ITERATION_LIMIT`                 | 1               | ?                  | ?                |
| Diverging iterates                                     | `NORM_LIMIT` or `OBJECTIVE_LIMIT` | 1               | ?                  | ?                |


## A complete example: solving a knapsack problem

We first need to select a solver supporting the given problem (see
[`supports`](@ref) and [`supports_constraint`](@ref)). In this example, we
want to solve a binary-constrained knapsack problem:
`max c'x: w'x <= C, x binary`. Suppose we choose GLPK:
```julia
using GLPK
optimizer = GLPK.Optimizer()
```
We first define the constants of the problem:
```jldoctest knapsack; setup = :(optimizer = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}()); MOI.Utilities.set_mock_optimize!(optimizer, mock -> MOI.Utilities.mock_optimize!(mock, ones(3))))
c = [1.0, 2.0, 3.0]
w = [0.3, 0.5, 1.0]
C = 3.2

# output

3.2
```
We create the variables of the problem and set the objective function:
```jldoctest knapsack
x = MOI.add_variables(optimizer, length(c))
objective_function = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(c, x), 0.0)
MOI.set(optimizer, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        objective_function)
MOI.set(optimizer, MOI.ObjectiveSense(), MOI.MAX_SENSE)

# output

MAX_SENSE::OptimizationSense = 1
```
We add the knapsack constraint and integrality constraints:
```jldoctest knapsack
knapsack_function = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(w, x), 0.0)
MOI.add_constraint(optimizer, knapsack_function, MOI.LessThan(C))
for x_i in x
    MOI.add_constraint(optimizer, MOI.SingleVariable(x_i), MOI.ZeroOne())
end

# output

```
We are all set! We can now call [`optimize!`](@ref) and wait for the solver to
find the solution:
```jldoctest knapsack
MOI.optimize!(optimizer)

# output

```
The first thing to check after optimization is why the solver stopped, e.g.,
did it stop because of a time limit or did it stop because it found the optimal
solution?
```jldoctest knapsack
MOI.get(optimizer, MOI.TerminationStatus())

# output


OPTIMAL::TerminationStatusCode = 1
```
It found the optimal solution! Now let's see what is that solution.
```jldoctest knapsack
MOI.get(optimizer, MOI.PrimalStatus())

# output

FEASIBLE_POINT::ResultStatusCode = 1
```
What is its objective value?
```jldoctest knapsack
MOI.get(optimizer, MOI.ObjectiveValue())

# output

6.0
```
And what is the value of the variables `x`?
```jldoctest knapsack
MOI.get(optimizer, MOI.VariablePrimal(), x)

# output

3-element Array{Float64,1}:
 1.0
 1.0
 1.0
```

## Problem modification

In addition to adding and deleting constraints and variables, MathOptInterface
supports modifying, in-place, coefficients in the constraints and the objective
function of a model. These modifications can be grouped into two categories:
modifications which replace the set of function of a constraint with a new set
or function; and modifications which change, in-place, a component of a
function.

In the following, we detail the various ways this can be
achieved. Readers should note that some solvers will not support problem
modification.

### Replacements

First, we discuss how to replace the set or function of a constraint with a new
instance of the same type.

#### The set of a constraint

Given a constraint of type `F`-in-`S` (see [Constraints by function-set pairs](@ref)
 above for an explanation), we can modify parameters (but not the type) of the
 set `S` by replacing it with a new instance of the same type. For example,
 given the variable bound ``x \le 1``:
```julia
c = add_constraint(m, SingleVariable(x), LessThan(1.0))
```
we can modify the set so that the bound now ``x \le 2`` as follows:
```julia
set(m, ConstraintSet(), c, LessThan(2.0))
```
where `m` is our [`ModelLike`](@ref) model. However, the following will fail as
the new set (`GreaterThan`) is of a different type to the original set
(`LessThan`):
```julia
set(m, ConstraintSet(), c, GreaterThan(2.0))  # errors
```
If our constraint is an affine inequality, then this corresponds to modifying
the right-hand side of a constraint in linear programming.

In some special cases, solvers may support efficiently changing the set of a
constraint (for example, from [`LessThan`](@ref) to [`GreaterThan`](@ref)). For
these cases, MathOptInterface provides the [`transform`](@ref) method. For
example, instead of the error we observed above, the following will
work:
```julia
c2 = transform(m, c, GreaterThan(1.0))
```
The [`transform`](@ref) function returns a new constraint index, and the old
constraint index (i.e., `c`) is no longer valid:
```julia
is_valid(m, c)   # false
is_valid(m, c2)  # true
```
Also note that [`transform`](@ref) cannot be called with a set of the same type;
[`set`](@ref) should be used instead.

#### The function of a constraint

Given a constraint of type `F`-in-`S` (see [Constraints by function-set pairs](@ref)
above for an explanation), it is also  possible to modify the function of type
`F` by replacing it with a new instance of the same type. For example, given the
variable bound ``x \le 1``:
```julia
c = add_constraint(m, SingleVariable(x), LessThan(1.0))
```
we can modify the function so that the bound now ``y \le 1`` as follows:
```julia
set(m, ConstraintFunction(), c, SingleVariable(y))
```
where `m` is our [`ModelLike`](@ref) model. However, the following will fail as
the new function is of a different type to the original function:
```julia
set(m, ConstraintFunction(), c,
    ScalarAffineFunction([ScalarAffineTerm(1.0, x)], 0.0)
)
```

### In-place modification

The second type of problem modifications allow the user to modify, in-place, the
coefficients of a function. Currently, four modifications are supported by
MathOptInterface. They are:
  1. change the constant term in a scalar function;
  2. change the constant term in a vector function;
  3. change the affine coefficients in a scalar function; and
  4. change the affine coefficients in a vector function.

To distinguish between the replacement of the function with a new instance
(described above) and the modification of an existing function, the in-place
modifications use the  [`modify`](@ref) method:
```julia
modify(model, index, change::AbstractFunctionModification)
```
[`modify`](@ref) takes three arguments. The first is the [`ModelLike`](@ref)
model `model`, the second is the constraint index, and the third is an instance
of an [`AbstractFunctionModification`](@ref).

We now detail each of these four in-place modifications.

#### Constant term in a scalar function

MathOptInterface supports is the ability to modify the constant term within a
[`ScalarAffineFunction`](@ref) and a [`ScalarQuadraticFunction`](@ref) using
the [`ScalarConstantChange`](@ref) subtype of
[`AbstractFunctionModification`](@ref). This includes the objective function, as
well as the function in a function-pair constraint.

For example, consider a problem `m` with the objective ``\max 1.0x + 0.0``:
```julia
set(m,
    ObjectiveFunction{ScalarAffineFunction{Float64}}(),
    ScalarAffineFunction([ScalarAffineTerm(1.0, x)], 0.0)
)
```
We can modify the constant term in the objective function as follows:
```julia
modify(m,
    ObjectiveFunction{ScalarAffineFunction{Float64}}(),
    ScalarConstantChange(1.0)
)
```
The objective function will now be ``\max 1.0x + 1.0``.

#### Constant terms in a vector function

We can modify the constant terms in a [`VectorAffineFunction`](@ref) or a
[`VectorQuadraticFunction`](@ref) using the [`VectorConstantChange`](@ref)
subtype of [`AbstractFunctionModification`](@ref).

For example, consider a model with the following
`VectorAffineFunction`-in-`Nonpositives` constraint:
```julia
c = add_constraint(m,
    VectorAffineFunction([
            VectorAffineTerm(1, ScalarAffineTerm(1.0, x)),
            VectorAffineTerm(1, ScalarAffineTerm(2.0, y))
        ],
        [0.0, 0.0]
    ),
    Nonpositives(2)
)
```
We can modify the constant vector in the [`VectorAffineFunction`](@ref) from
`[0.0, 0.0]` to `[1.0, 2.0]` as follows:
```julia
modify(m, c, VectorConstantChange([1.0, 2.0])
)
```
The constraints are now ``1.0x + 1.0 \le 0.0`` and ``2.0y + 2.0 \le 0.0``.

#### Affine coefficients in a scalar function

In addition to modifying the constant terms in a function, we can also modify
the affine variable coefficients in an [`ScalarAffineFunction`](@ref) or a
[`ScalarQuadraticFunction`](@ref) using the [`ScalarCoefficientChange`](@ref)
subtype of [`AbstractFunctionModification`](@ref).

For example, given the constraint ``1.0x <= 1.0``:
```julia
c = add_constraint(m,
    ScalarAffineFunction([ScalarAffineTerm(1.0, x)], 0.0),
    LessThan(1.0)
)
```
we can modify the coefficient of the `x` variable so that the constraint becomes
``2.0x <= 1.0`` as follows:
```julia
modify(m, c, ScalarCoefficientChange(x, 2.0))
```

[`ScalarCoefficientChange`](@ref) can also be used to modify the objective
function by passing an instance of [`ObjectiveFunction`](@ref) instead of the
constraint index `c` as we saw above.

#### Affine coefficients in a vector function

Finally, the last modification supported by MathOptInterface is the ability to
modify the affine coefficients of a single variable in a
[`VectorAffineFunction`](@ref) or a [`VectorQuadraticFunction`](@ref) using
the [`MultirowChange`](@ref) subtype of [`AbstractFunctionModification`](@ref).

For example, given the constraint ``Ax \in \mathbb{R}^2_+``, where
``A = [1.0, 2.0]^\top``:
```julia
c = add_constraint(m,
    VectorAffineFunction([
            VectorAffineTerm(1, ScalarAffineTerm(1.0, x)),
            VectorAffineTerm(1, ScalarAffineTerm(2.0, x))
        ],
        [0.0, 0.0]
    ),
    Nonnegatives(2)
)
```
we can modify the coefficients of the `x` variable so that the `A` matrix
becomes ``A = [3.0, 4.0]^\top`` as follows:
```julia
modify(m, c, MultirowChange(x, [3.0, 4.0]))
```

## Advanced

### Duals

Conic duality is the starting point for MOI's duality conventions. When all functions are affine (or coordinate projections), and all constraint sets are closed convex cones, the model may be called a conic optimization problem.
For conic-form minimization problems, the primal is:

```math
\begin{align}
& \min_{x \in \mathbb{R}^n} & a_0^T x + b_0
\\
& \;\;\text{s.t.} & A_i x + b_i & \in \mathcal{C}_i & i = 1 \ldots m
\end{align}
```

and the dual is:

```math
\begin{align}
& \max_{y_1, \ldots, y_m} & -\sum_{i=1}^m b_i^T y_i + b_0
\\
& \;\;\text{s.t.} & a_0 - \sum_{i=1}^m A_i^T y_i & = 0
\\
& & y_i & \in \mathcal{C}_i^* & i = 1 \ldots m
\end{align}
```

where each ``\mathcal{C}_i`` is a closed convex cone and ``\mathcal{C}_i^*`` is its dual cone.

For conic-form maximization problems, the primal is:
```math
\begin{align}
& \max_{x \in \mathbb{R}^n} & a_0^T x + b_0
\\
& \;\;\text{s.t.} & A_i x + b_i & \in \mathcal{C}_i & i = 1 \ldots m
\end{align}
```

and the dual is:

```math
\begin{align}
& \min_{y_1, \ldots, y_m} & \sum_{i=1}^m b_i^T y_i + b_0
\\
& \;\;\text{s.t.} & a_0 + \sum_{i=1}^m A_i^T y_i & = 0
\\
& & y_i & \in \mathcal{C}_i^* & i = 1 \ldots m
\end{align}
```



A linear inequality constraint ``a^T x + b \ge c`` should be interpreted as ``a^T x + b - c \in \mathbb{R}_+``, and similarly ``a^T x + b \le c`` should be interpreted as ``a^T x + b - c \in \mathbb{R}_-``.
Variable-wise constraints should be interpreted as affine constraints with the appropriate identity mapping in place of ``A_i``.

For the special case of minimization LPs, the MOI primal form can be stated as
```math
\begin{align}
& \min_{x \in \mathbb{R}^n} & a_0^T x &+ b_0
\\
& \;\;\text{s.t.}
&A_1 x & \ge b_1\\
&& A_2 x & \le b_2\\
&& A_3 x & = b_3
\end{align}
```

By applying the stated transformations to conic form, taking the dual, and transforming back into linear inequality form, one obtains the following dual:

```math
\begin{align}
& \max_{y_1,y_2,y_3} & b_1^Ty_1 + b_2^Ty_2 + b_3^Ty_3 &+ b_0
\\
& \;\;\text{s.t.}
&A_1^Ty_1 + A_2^Ty_2 + A_3^Ty_3 & = a_0\\
&& y_1 &\ge 0\\
&& y_2 &\le 0
\end{align}
```

For maximization LPs, the MOI primal form can be stated as:
```math
\begin{align}
& \max_{x \in \mathbb{R}^n} & a_0^T x &+ b_0
\\
& \;\;\text{s.t.}
&A_1 x & \ge b_1\\
&& A_2 x & \le b_2\\
&& A_3 x & = b_3
\end{align}
```

and similarly, the dual is:
```math
\begin{align}
& \min_{y_1,y_2,y_3} & -b_1^Ty_1 - b_2^Ty_2 - b_3^Ty_3 &+ b_0
\\
& \;\;\text{s.t.}
&A_1^Ty_1 + A_2^Ty_2 + A_3^Ty_3 & = -a_0\\
&& y_1 &\ge 0\\
&& y_2 &\le 0
\end{align}
```

An important note for the LP case is that the signs of the feasible duals depend only on the sense of the inequality and not on the objective sense.

#### Duality and scalar product

The scalar product is different from the canonical one for the sets
[`PositiveSemidefiniteConeTriangle`](@ref), [`LogDetConeTriangle`](@ref),
[`RootDetConeTriangle`](@ref).
If the set ``C_i`` of the section [Duals](@ref) is one of these three cones,
then the rows of the matrix ``A_i`` corresponding to off-diagonal entries are
twice the value of the `coefficients` field in the `VectorAffineFunction` for
the corresponding rows. See [`PositiveSemidefiniteConeTriangle`](@ref) for
details.

#### Dual for problems with quadratic functions

Given a problem with quadratic functions:
```math
\begin{align*}
& \min_{x \in \mathbb{R}^n} & \frac{1}{2}x^TQ_0x + a_0^T x + b_0
\\
& \;\;\text{s.t.} & \frac{1}{2}x^TQ_ix + a_i^T x + b_i & \in \mathcal{C}_i & i = 1 \ldots m
\end{align*}
```
Consider the Lagrangian function
```math
L(x, y) = \frac{1}{2}x^TQ_0x + a_0^T x + b_0 - \sum_{i = 1}^m y_i (\frac{1}{2}x^TQ_ix + a_i^T x + b_i)
```
A pair of primal-dual variables $(x^\star, y^\star)$ is optimal if
* ``x^\star`` is a minimizer of
  ```math
  \min_{x \in \mathbb{R}^n} L(x, y^\star).
  ```
  That is,
  ```math
  0 = \nabla_x L(x, y^\star) = Q_0x + a_0 - \sum_{i = 1}^m y_i^\star (Q_ix + a_i).
  ```
* and ``y^\star`` is a maximizer of
  ```math
  \max_{y_i \in \mathcal{C}_i^*} L(x^\star, y).
  ```
  That is, for all ``i = 1, \ldots, m``, ``\frac{1}{2}x^TQ_ix + a_i^T x + b_i`` is
  either zero or in the normal cone of ``\mathcal{C}_i^*`` at ``y^\star``.
  For instance, if ``\mathcal{C}_i`` is ``\{ x \in \mathbb{R} : x \le 0 \}``, it means that
  if ``\frac{1}{2}x^TQ_ix + a_i^T x + b_i`` is nonzero then ``\lambda_i = 0``,
  this is the classical complementary slackness condition.

If ``\mathcal{C}_i`` is a vector set, the discussion remains valid with
``y_i(\frac{1}{2}x^TQ_ix + a_i^T x + b_i)`` replaced with the scalar product
between `y_i` and the vector of scalar-valued quadratic functions.

!!! note
    For quadratic programs with only affine constraints, the optimality condition
    ``\nabla_x L(x, y^\star) = 0`` can be simplified as follows
    ```math
    0 = \nabla_x L(x, y^\star) = Q_0x + a_0 - \sum_{i = 1}^m y_i^\star a_i
    ```
    which gives
    ```math
    Q_0x = \sum_{i = 1}^m y_i^\star a_i - a_0
    ```
    The Lagrangian function
    ```math
    L(x, y) = \frac{1}{2}x^TQ_0x + a_0^T x + b_0 - \sum_{i = 1}^m y_i (a_i^T x + b_i)
    ```
    can be rewritten as
    ```math
    L(x, y) = \frac{1}{2}x^TQ_0x - (\sum_{i = 1}^m y_i a_i^T - a_0^T) x + b_0 - \sum_{i = 1}^m y_i (a_i^T x + b_i)
    ```
    which, using the optimality condition ``\nabla_x L(x, y^\star) = 0``, can be simplified as
    ```math
    L(x, y) = -\frac{1}{2}x^TQ_0x + b_0 - \sum_{i = 1}^m y_i (a_i^T x + b_i)
    ```


### Automatic reformulation

#### Constraint reformulation

A constraint often possess different equivalent formulations, but a solver may only support one of them.
It would be duplicate work to implement rewritting rules in every solver wrapper for every different formulation of the constraint to express it in the form supported by the solver.
Constraint bridges provide a way to define a rewritting rule on top of the MOI interface which can be used by any optimizer.
Some rules also implement constraint modifications and constraint primal and duals translations.

For example, the `SplitIntervalBridge` defines the reformulation of a `ScalarAffineFunction`-in-`Interval` constraint into a `ScalarAffineFunction`-in-`GreaterThan` and a `ScalarAffineFunction`-in-`LessThan` constraint.
The `SplitInterval` is the bridge optimizer that applies the `SplitIntervalBridge` rewritting rule.
Given an optimizer `optimizer` implementing `ScalarAffineFunction`-in-`GreaterThan` and `ScalarAffineFunction`-in-`LessThan`, the optimizer
```
bridgedoptimizer = SplitInterval(optimizer)
```
will additionally support `ScalarAffineFunction`-in-`Interval`.

## Implementing a solver interface

[The interface is designed for multiple dispatch, e.g., attributes, combinations of sets and functions.]

### Solver-specific attributes

Solver-specific attributes should either be passed to the optimizer on creation,
e.g., `MyPackage.Optimizer(PrintLevel = 0)`, or through a sub-type of
[`AbstractOptimizerAttribute`](@ref). For example, inside `MyPackage`, we could add
the following:
```julia
struct PrintLevel <: MOI.AbstractOptimizerAttribute end
function MOI.set(model::Optimizer, ::PrintLevel, level::Int)
    # ... set the print level ...
end
```
Then, the user can write:
```julia
model = MyPackage.Optimizer()
MOI.set(model, MyPackage.PrintLevel(), 0)
```

### Supported constrained variables and constraints

The solver interface should only implement support for constrained variables
(see [`add_constrained_variable`](@ref)/[`add_constrained_variables`](@ref))
or constraints that directly map to a structure exploited by the solver
algorithm. There is no need to add support for additional types, this is
handled by the [Automatic reformulation](@ref). Furthermore, this allows
[`supports_constraint`](@ref) to indicate which types are exploited by the
solver and hence allows layers such as [`Bridges.LazyBridgeOptimizer`](@ref)
to accurately select the most appropriate transformations.

As [`add_constrained_variable`](@ref) (resp. [`add_constrained_variables`](@ref))
falls back to [`add_variable`](@ref) (resp. [`add_variables`](@ref)) followed by
[`add_constraint`](@ref), there is no need to implement this function
if `model` supports creating free variables. However, if `model` does not
support creating free variables, then it should only implement
[`add_constrained_variable`](@ref) and not [`add_variable`](@ref) nor
[`add_constraint`](@ref) for [`SingleVariable`](@ref)-in-`typeof(set)`.
In addition, it should implement `supports_constraint(::Optimizer,
::Type{VectorOfVariables}, ::Type{Reals})` and return `false` so that free
variables are bridged, see [`supports_constraint`](@ref).

### Implementing copy

Avoid storing extra copies of the problem when possible. This means that solver
wrappers should not use [`Utilities.CachingOptimizer`](@ref) as part of the wrapper.
Instead, do one of the following to load the problem (assuming the solver
wrapper type is called `Optimizer`):

* If the solver supports loading the problem incrementally, implement
  [`add_variable`](@ref), [`add_constraint`](@ref) for supported constraints and
  [`set`](@ref) for supported attributes and add:
  ```julia
  function MOI.copy_to(dest::Optimizer, src::MOI.ModelLike; kws...)
      return MOI.Utilities.automatic_copy_to(dest, src; kws...)
  end
  ```
  with
  ```julia
  MOI.Utilities.supports_default_copy_to(model::Optimizer, copy_names::Bool) = true
  ```
  or
  ```julia
  MOI.Utilities.supports_default_copy_to(model::Optimizer, copy_names::Bool) = !copy_names
  ```
  depending on whether the solver support names; see
  [`Utilities.supports_default_copy_to`](@ref) for more details.
* If the solver does not support loading the problem incrementally, do not
  implement [`add_variable`](@ref) and [`add_constraint`](@ref) as implementing
  them would require caching the problem. Let users or JuMP decide whether to
  use a `CachingOptimizer` instead. Write either a custom implementation of
  [`copy_to`](@ref) or implement the [Allocate-Load API](@ref). If you choose to
  implement the Allocate-Load API,
  do
  ```julia
  function MOI.copy_to(dest::Optimizer, src::MOI.ModelLike; kws...)
      return MOI.Utilities.automatic_copy_to(dest, src; kws...)
  end
  ```
  with
  ```julia
  MOI.Utilities.supports_allocate_load(model::Optimizer, copy_names::Bool) = true
  ```
  or
  ```julia
  MOI.Utilities.supports_allocate_load(model::Optimizer, copy_names::Bool) = !copy_names
  ```
  depending on whether the solver support names; see
  [`Utilities.supports_allocate_load`](@ref) for more details.

  Note that even if both writing a custom implementation of [`copy_to`](@ref)
  and implementing the [Allocate-Load API](@ref) requires the user to copy the
  model from a cache, the [Allocate-Load API](@ref) allows MOI layers to be
  added between the cache and the solver which allows transformations to be
  applied without the need for additional caching. For instance, with the
  proposed [Light bridges](https://github.com/JuliaOpt/MathOptInterface.jl/issues/523),
  no cache will be needed to store the bridged model when bridges are used by
  JuMP so implementing the [Allocate-Load API](@ref) will allow JuMP to use only
  one cache instead of two.

### JuMP mapping

MOI defines a very general interface, with multiple possible ways to describe the same constraint. This is considered a feature, not a bug. MOI is designed to make it possible to experiment with alternative representations of an optimization problem at both the solving and modeling level. When implementing an interface, it is important to keep in mind that the constraints which a solver supports via MOI will have a near 1-to-1 correspondence with how users can express problems in JuMP, because JuMP does not perform automatic transformations. (Alternative systems like Convex.jl do.) The following bullet points show examples of how JuMP constraints are translated into MOI function-set pairs:
 - `@constraint(m, 2x + y <= 10)` becomes `ScalarAffineFunction`-in-`LessThan`;
 - `@constraint(m, 2x + y >= 10)` becomes `ScalarAffineFunction`-in-`GreaterThan`;
 - `@constraint(m, 2x + y == 10)` becomes `ScalarAffineFunction`-in-`EqualTo`;
 - `@constraint(m, 0 <= 2x + y <= 10)` becomes `ScalarAffineFunction`-in-`Interval`;
 - `@constraint(m, 2x + y in ArbitrarySet())` becomes `ScalarAffineFunction`-in-`ArbitrarySet`.

Variable bounds are handled in a similar fashion:
 - `@variable(m, x <= 1)` becomes `SingleVariable`-in-`LessThan`;
 - `@variable(m, x >= 1)` becomes `SingleVariable`-in-`GreaterThan`.

One notable difference is that a variable with an upper and lower bound is translated into two constraints, rather than an interval. i.e.:
 - `@variable(m, 0 <= x <= 1)` becomes `SingleVariable`-in-`LessThan` *and* `SingleVariable`-in-`GreaterThan`.

Therefore, if a solver wrapper does not support `ScalarAffineFunction`-in-`LessThan` constraints, users will not be able to write: `@constraint(m, 2x + y <= 10)` in JuMP. With this in mind, developers should support all the constraint types that they want to be usable from JuMP. That said, from the perspective of JuMP, solvers can safely choose to not support the following constraints:

- `AbstractScalarFunction` in `GreaterThan`, `LessThan`, `EqualTo`, or
  `Interval` with a nonzero constant in the function. Constants in the affine
  function should instead be moved into the parameters of the corresponding
  sets. The [`ScalarFunctionConstantNotZero`](@ref) exception may be thrown in
  this case.

- `ScalarAffineFunction` in `Nonnegative`, `Nonpositive` or `Zeros`. Alternative constraints are available by using a `VectorAffineFunction` with one output row or `ScalarAffineFunction` with `GreaterThan`, `LessThan`, or `EqualTo`.

- Two `SingleVariable`-in-`LessThan` constraints applied to the same variable (similarly with `GreaterThan`). These should be interpreted as variable bounds, and each variable naturally has at most one upper or lower bound.

### Column Generation

There is no special interface for column generation. If the solver has a special API for setting
coefficients in existing constraints when adding a new variable, it is possible
to queue modifications and new variables and then call the solver's API once all of the
new coefficients are known.

### Problem data

All data passed to the solver should be copied immediately to internal data
structures. Solvers may not modify any input vectors and should assume that
input vectors may be modified by users in the future. This applies, for example,
to the `terms` vector in `ScalarAffineFunction`. Vectors returned to the user,
e.g., via `ObjectiveFunction` or `ConstraintFunction` attributes, should not be
modified by the solver afterwards. The in-place version of `get!` can be used by
users to avoid extra copies in this case.

### Statuses

Solver wrappers should document how the low-level statuses map to the MOI
statuses. Statuses like `NEARLY_FEASIBLE_POINT` and `INFEASIBLE_POINT`, are
designed to be used when the solver explicitly indicates that relaxed tolerances
are satisfied or the returned point is infeasible, respectively.

### Naming

MOI solver interfaces may be in the same package as the solver itself (either
the C wrapper if the solver is accessible through C, or the Julia code if the
solver is written in Julia, for example). The guideline for naming the file
containing the MOI wrapper is `src/MOI_wrapper.jl` and `test/MOI_wrapper.jl` for
the tests. If the MOI wrapper implementation is spread in several files, they
should be stored in a `src/MOI_wrapper` folder and included by a
`src/MOI_wrapper/MOI_wrapper.jl` file. In some cases it may be more appropriate
to host the MOI wrapper in its own package; in this case it is recommended that
the MOI wrapper package be named `MathOptInterfaceXXX` where `XXX` is the solver
name.

By convention, optimizers should not be exported and should be named
`PackageName.Optimizer`. For example, `CPLEX.Optimizer`, `Gurobi.Optimizer`, and
`Xpress.Optimizer`.

### Testing guideline

The skeleton below can be used for the wrapper test file of a solver named `FooBar`.
```julia
using Test

using MathOptInterface
const MOI = MathOptInterface
const MOIT = MOI.Test
const MOIU = MOI.Utilities
const MOIB = MOI.Bridges

import FooBar
const optimizer = FooBar.Optimizer()
MOI.set(optimizer, MOI.Silent(), true)

@testset "SolverName" begin
    @test MOI.get(optimizer, MOI.SolverName()) == "FooBar"
end

@testset "supports_default_copy_to" begin
    @test MOIU.supports_default_copy_to(optimizer, false)
    # Use `@test !...` if names are not supported
    @test MOIU.supports_default_copy_to(optimizer, true)
end

const bridged = MOIB.full_bridge_optimizer(optimizer, Float64)
const config = MOIT.TestConfig(atol=1e-6, rtol=1e-6)

@testset "Unit" begin
    MOIT.unittest(bridged, config)
end

@testset "Modification" begin
    MOIT.modificationtest(bridged, config)
end

@testset "Continuous Linear" begin
    MOIT.contlineartest(bridged, config)
end

@testset "Continuous Conic" begin
    MOIT.contlineartest(bridged, config)
end

@testset "Integer Conic" begin
    MOIT.intconictest(bridged, config)
end
```
The optimizer `bridged` constructed with [`Bridges.full_bridge_optimizer`](@ref)
automatically bridges constraints that are not supported by `optimizer`
using the bridges listed in [Bridges](@ref). It is recommended for an
implementation of MOI to only support constraints that are natively supported
by the solver and let bridges transform the constraint to the appropriate form.
For this reason it is expected that tests may not pass if `optimizer` is used
instead of `bridged`.

To test that a specific problem can be solved without bridges, a specific test can
be run with `optimizer` instead of `bridged`. For instance
```julia
@testset "Interval constraints" begin
    MOIT.linear10test(optimizer, config)
end
```
checks that `optimizer` implements support for
[`ScalarAffineFunction`](@ref)-in-[`Interval`](@ref).

If the wrapper does not support building the model incrementally (i.e. with `add_variable` and `add_constraint`), then `supports_default_copy_to` can be replaced by `supports_allocate_load` if appropriate (see [Implementing copy](@ref)) and the line `const bridged = ...` can be replaced with
```julia
const cache = MOIU.UniversalFallback(MOIU.Model{Float64}())
const cached = MOIU.CachingOptimizer(cache, optimizer)
const bridged = MOIB.full_bridge_optimizer(cached, Float64)
```

### Benchmarking

To aid the development of efficient solver wrappers, MathOptInterface provides
benchmarking functionality. Benchmarking a wrapper follows a two-step process.

First, prior to making changes, run and save the benchmark results on a given
benchmark suite as follows:

```julia
using SolverPackage, MathOptInterface

const MOI = MathOptInterface

suite = MOI.Benchmarks.suite() do
    SolverPackage.Optimizer()
end

MOI.Benchmarks.create_baseline(
    suite, "current"; directory = "/tmp", verbose = true
)
```
Use the `exclude` argument to [`Benchmarks.suite`](@ref) to
exclude benchmarks that the solver doesn't support.

Second, after making changes to the package, re-run the benchmark suite and
compare to the prior saved results:

```julia
using SolverPackage, MathOptInterface

const MOI = MathOptInterface

suite = MOI.Benchmarks.suite() do
    SolverPackage.Optimizer()
end

MOI.Benchmarks.compare_against_baseline(
    suite, "current"; directory = "/tmp", verbose = true
)
```

This comparison will create a report detailing improvements and regressions.
