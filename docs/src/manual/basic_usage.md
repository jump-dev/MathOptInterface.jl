```@meta
CurrentModule = MathOptInterface
DocTestSetup = quote
    using MathOptInterface
    const MOI = MathOptInterface
end
DocTestFilters = [r"MathOptInterface|MOI"]
```

# Basic usage

!!! note
    MOI does not export functions, but for brevity we often omit qualifying
    names with the MOI module. Best practice is to have
    ```julia
    using MathOptInterface
    const MOI = MathOptInterface
    ```
    and prefix all MOI methods with `MOI.` in user code. If a name is also
    available in base Julia, we always explicitly use the module prefix, for
    example, with `MOI.get`.

## The `ModelLike` and `AbstractOptimizer` APIs

The most significant part of MOI is the definition of the **model API** that is
used to specify an instance of an optimization problem (e.g., by adding
variables and constraints). Objects that implement the model API should inherit
from the [`ModelLike`](@ref) abstract type.

Notably missing from the model API is the method to solve an optimization
problem. `ModelLike` objects may store an instance (e.g., in memory or backed by
a file format) without being linked to a particular solver. In addition to the
model API, MOI defines [`AbstractOptimizer`](@ref).

*Optimizers* (or solvers) implement the model API (inheriting from `ModelLike`)
and additionally provide methods to solve the model.

Through the rest of the manual, `model` is used as a generic `ModelLike`, and
`optimizer` is used as a generic `AbstractOptimizer`.

Models are constructed by
* adding variables using [`add_variable`](@ref) (or [`add_variables`](@ref)),
  see [Adding variables](@ref);
* setting an objective sense and function using [`set`](@ref),
  see [Setting an objective](@ref);
* and adding constraints using [`add_constraint`](@ref) (or
  [`add_constraints`](@ref)), see [Sets and Constraints](@ref).

The way the problem is solved by the optimimizer is controlled by
[`AbstractOptimizerAttribute`](@ref)s, see [Solver-specific attributes](@ref).

## Adding variables

All variables in MOI are scalar variables. New scalar variables are created with
[`add_variable`](@ref) or [`add_variables`](@ref), which return a [`VariableIndex`](@ref)
or `Vector{VariableIndex}` respectively. [`VariableIndex`](@ref) objects are
type-safe wrappers around integers that refer to a variable in a particular
model.

!!! note
    The integer does not necessarily corresond to the column inside an optimizer!

One uses [`VariableIndex`](@ref) objects to set and get variable attributes. For
example, the [`VariablePrimalStart`](@ref) attribute is used to provide an
initial starting point for a variable or collection of variables:
```julia
v = MOI.add_variable(model)
MOI.set(model, MOI.VariablePrimalStart(), v, 10.5)
v2 = MOI.add_variables(model, 3)
MOI.set(model, MOI.VariablePrimalStart(), v2, [1.3, 6.8, -4.6])
```

A variable can be deleted from a model with
[`delete(::ModelLike, ::VariableIndex)`](@ref MathOptInterface.delete(::MathOptInterface.ModelLike, ::MathOptInterface.Index)).
Not all models support deleting variables; a [`DeleteNotAllowed`](@ref) error is
thrown if this is not supported.

## Functions

MOI defines six functions as listed in the definition of the [Standard form problem](@ref).
The simplest function is [`SingleVariable`](@ref), defined as:
```julia
struct SingleVariable <: AbstractFunction
    variable::VariableIndex
end
```

If `v` is a [`VariableIndex`](@ref) object, then `SingleVariable(v)` is simply
the scalar-valued function from the complete set of variables in a model that
returns the value of variable `v`. One may also call this function a coordinate
projection, which is more useful for defining constraints than as an objective
function.

A more interesting function is [`ScalarAffineFunction`](@ref), defined as:
```julia
struct ScalarAffineFunction{T} <: AbstractScalarFunction
    terms::Vector{ScalarAffineTerm{T}}
    constant::T
end
```

The [`ScalarAffineTerm`](@ref) struct defines a variable-coefficient pair:
```julia
struct ScalarAffineTerm{T}
    coefficient::T
    variable_index::VariableIndex
end
```

If `x` is a vector of `VariableIndex` objects, then
```julia
MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([5.0, -2.3], [x[1], x[2]]), 1.0)
```
represents the function ``5 x_1 - 2.3 x_2 + 1``.

!!! note
    `MOI.ScalarAffineTerm.([5.0, -2.3], [x[1], x[2]])` is a shortcut for
    `[MOI.ScalarAffineTerm(5.0, x[1]), MOI.ScalarAffineTerm(-2.3, x[2])]`. This
    is Julia's broadcast syntax in action, and is used quite often.

### Setting an objective

Objective functions are assigned to a model by setting the [`ObjectiveFunction`](@ref)
attribute. The [`ObjectiveSense`](@ref) attribute is used for setting the
optimization sense. For example,
```julia
x = MOI.add_variables(model, 2)
MOI.set(
    model,
    MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    MOI.ScalarAffineFunction(
        MOI.ScalarAffineTerm.([5.0, -2.3], [x[1], x[2]]), 1.0),
    )
MOI.set(model, MOI.ObjectiveSense(), MIN_SENSE)
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
MOI.optimize!(optimizer)
```

The optimization procedure may terminate for a number of reasons. The
[`TerminationStatus`](@ref) attribute of the optimizer returns a
[`TerminationStatusCode`](@ref) object which explains why the solver stopped.
The termination statuses distinguish between proofs of optimality,
infeasibility, local convergence, limits, and termination because of something
unexpected like invalid problem data or failure to converge. A typical usage of
the [`TerminationStatus`](@ref) attribute is as follows:
```julia
status = MOI.get(optimizer, TerminationStatus())
if status == MOI.OPTIMAL
    # Ok, we solved the problem!
else
    # Handle other cases.
end
```

After checking the [`TerminationStatus`](@ref), one should typically check
[`ResultCount`](@ref). This attribute returns the number of results that the
solver has available to return. *A result is defined as a primal-dual pair,
but either the primal or the dual may be missing from the result.* While the
`OPTIMAL` termination status normally implies that at least one result is
available, other statuses do not. For example, in the case of infeasiblity, a
solver may return no result or a proof of infeasibility. The [`ResultCount`](@ref)
attribute distinguishes between these two cases.

The [`PrimalStatus`](@ref) and [`DualStatus`](@ref) attributes return a
[`ResultStatusCode`](@ref) that indicates if that component of the result
is present (i.e., not `NO_SOLUTION`) and explains how to interpret the result.

If `PrimalStatus` is not `NO_SOLUTION`, then the primal may be retrieved with
the [`VariablePrimal`](@ref) attribute:
```julia
MOI.get(optimizer, VariablePrimal(), x)
```

If `x` is a [`VariableIndex`](@ref) then the function call returns a scalar, and
if `x` is a `Vector{VariableIndex}` then the call returns a vector of scalars.
`VariablePrimal()` is equivalent to `VariablePrimal(1)`, i.e., the variable
primal vector of the first result. Use `VariablePrimal(N)` to access the `N`th
result.

See also the attributes [`ConstraintPrimal`](@ref), and [`ConstraintDual`](@ref).

See [Duality](@ref) for a discussion of the MOI conventions for primal-dual
pairs and certificates.

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
MOI.set(
    optimizer,
    MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(c, x), 0.0),
)
MOI.set(optimizer, MOI.ObjectiveSense(), MOI.MAX_SENSE)

# output

MAX_SENSE::OptimizationSense = 1
```

We add the knapsack constraint and integrality constraints:
```jldoctest knapsack
MOI.add_constraint(
    optimizer,
    MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(w, x), 0.0),
    MOI.LessThan(C),
)
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
c = MOI.add_constraint(model, MOI.SingleVariable(x), MOI.LessThan(1.0))
```
we can modify the set so that the bound now ``x \le 2`` as follows:
```julia
MOI.set(model, MOI.ConstraintSet(), c, MOI.LessThan(2.0))
```
where `model` is our [`ModelLike`](@ref) model. However, the following will fail
as the new set ([`GreaterThan`](@ref)) is of a different type to the original
set ([`LessThan`](@ref)):
```julia
MOI.set(model, MOI.ConstraintSet(), c, MOI.GreaterThan(2.0))  # errors
```
If our constraint is an affine inequality, then this corresponds to modifying
the right-hand side of a constraint in linear programming.

In some special cases, solvers may support efficiently changing the set of a
constraint (for example, from [`LessThan`](@ref) to [`GreaterThan`](@ref)). For
these cases, MathOptInterface provides the [`transform`](@ref) method. For
example, instead of the error we observed above, the following will
work:
```julia
c2 = MOI.transform(model, c, MOI.GreaterThan(1.0))
```

The [`transform`](@ref) function returns a new constraint index, and the old
constraint index (i.e., `c`) is no longer valid:
```julia
MOI.is_valid(model, c)   # false
MOI.is_valid(model, c2)  # true
```
Also note that [`transform`](@ref) cannot be called with a set of the same type;
[`set`](@ref) should be used instead.

#### The function of a constraint

Given a constraint of type `F`-in-`S` (see [Constraints by function-set pairs](@ref)
above for an explanation), it is also  possible to modify the function of type
`F` by replacing it with a new instance of the same type. For example, given the
variable bound ``x \le 1``:
```julia
c = MOI.add_constraint(model, MOI.SingleVariable(x), MOI.LessThan(1.0))
```
we can modify the function so that the bound now ``y \le 1`` as follows:
```julia
MOI.set(model, MOI.ConstraintFunction(), c, MOI.SingleVariable(y))
```
where `m` is our [`ModelLike`](@ref) model. However, the following will fail as
the new function is of a different type to the original function:
```julia
MOI.set(
    model,
    MOI.ConstraintFunction(),
    c,
    MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0),
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
MOI.modify(model, index, change::AbstractFunctionModification)
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

For example, consider a problem `model` with the objective ``\max 1.0x + 0.0``:
```julia
MOI.set(
    model,
    MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0),
)
```

We can modify the constant term in the objective function as follows:
```julia
MOI.modify(
    model,
    MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    MOI.ScalarConstantChange(1.0)
)
```
The objective function will now be ``\max 1.0x + 1.0``.

#### Constant terms in a vector function

We can modify the constant terms in a [`VectorAffineFunction`](@ref) or a
[`VectorQuadraticFunction`](@ref) using the [`VectorConstantChange`](@ref)
subtype of [`AbstractFunctionModification`](@ref).

For example, consider a model with the following
[`VectorAffineFunction`](@ref)-in-[`Nonpositives`](@ref) constraint:
```julia
c = MOI.add_constraint(
    model,
    MOI.VectorAffineFunction([
            MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, x)),
            MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(2.0, y))
        ],
        [0.0, 0.0],
    ),
    MOI.Nonpositives(2),
)
```
We can modify the constant vector in the [`VectorAffineFunction`](@ref) from
`[0.0, 0.0]` to `[1.0, 2.0]` as follows:
```julia
MOI.modify(model, c, MOI.VectorConstantChange([1.0, 2.0])
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
c = MOI.add_constraint(
    model,
    MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0),
    MOI.LessThan(1.0),
)
```
we can modify the coefficient of the `x` variable so that the constraint becomes
``2.0x <= 1.0`` as follows:
```julia
MOI.modify(model, c, MOI.ScalarCoefficientChange(x, 2.0))
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
c = MOI.add_constraint(
    model,
    MOI.VectorAffineFunction([
            MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, x)),
            MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(2.0, x)),
        ],
        [0.0, 0.0],
    ),
    MOI.Nonnegatives(2),
)
```
we can modify the coefficients of the `x` variable so that the `A` matrix
becomes ``A = [3.0, 4.0]^\top`` as follows:
```julia
MOI.modify(model, c, MOI.MultirowChange(x, [3.0, 4.0]))
```
