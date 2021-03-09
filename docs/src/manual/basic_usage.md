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

## The ModelLike and AbstractOptimizer APIs

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

!!! info
    Throughout the rest of the manual, `model` is used as a generic `ModelLike`,
    and `optimizer` is used as a generic `AbstractOptimizer`.

Models are constructed by
* adding variables using [`add_variable`](@ref) (or [`add_variables`](@ref)),
  see [Add a variable](@ref);
* setting an objective sense and function using [`set`](@ref),
  see [Setting an objective](@ref);
* and adding constraints using [`add_constraint`](@ref) (or
  [`add_constraints`](@ref)), see [Constraints](@ref).

The way the problem is solved by the optimimizer is controlled by
[`AbstractOptimizerAttribute`](@ref)s, see [Solver-specific attributes](@ref).

## Add a variable

All variables in MOI are scalar variables. New scalar variables are created with
[`add_variable`](@ref) or [`add_variables`](@ref), which return a [`VariableIndex`](@ref)
or `Vector{VariableIndex}` respectively. [`VariableIndex`](@ref) objects are
type-safe wrappers around integers that refer to a variable in a particular
model.

!!! note
    The integer does not necessarily corresond to the column inside an
    optimizer!

One uses [`VariableIndex`](@ref) objects to set and get variable attributes. For
example, the [`VariablePrimalStart`](@ref) attribute is used to provide an
initial starting point for a variable or collection of variables:
```julia
v = MOI.add_variable(model)
MOI.set(model, MOI.VariablePrimalStart(), v, 10.5)
v2 = MOI.add_variables(model, 3)
MOI.set(model, MOI.VariablePrimalStart(), v2, [1.3, 6.8, -4.6])
```

## Delete a variable

Delete a variable using
[`delete(::ModelLike, ::VariableIndex)`](@ref MathOptInterface.delete(::MathOptInterface.ModelLike, ::MathOptInterface.Index)).

!!! warning
    Not all `ModelLike` models support deleting variables. A
    [`DeleteNotAllowed`](@ref) error is thrown if this is not supported.

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

