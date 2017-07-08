
# Manual

## Purpose

Each mathematical optimization solver API has its own concepts and data structures for representing optimization instances and obtaining results.
However, it is often desirable to represent an instance of an optimization problem at a higher level so that it is easy to try using different solvers.
MathOptInterface (MOI) is an abstraction layer designed to provide a unified interface to mathematical optimization solvers so that users do not need to understand multiple solver-specific APIs.
MOI can be used directly, or through a higher-level modeling interface like [JuMP](https://github.com/JuliaOpt/JuMP.jl).

MOI has been designed to replace [MathProgBase](https://github.com/JuliaOpt/MathProgBase.jl), which has been used by modeling packages such as [JuMP](https://github.com/JuliaOpt/JuMP.jl) and [Convex.jl](https://github.com/JuliaOpt/Convex.jl).
This second-generation abstraction layer addresses a number of limitations of MathProgBase.
MOI is designed to:
- Be simple and extensible, unifying linear, quadratic, and conic optimization, and seamlessly facilitate extensions to essentially arbitrary constraints and functions (e.g., indicator constraints, complementarity constraints, and piecewise linear functions)
- Be fast by allowing access to a solver's in-memory representation of a problem without writing intermediate files (when possible) and by using multiple dispatch and concrete types
- Allow a solver to return multiple results (e.g., a pool of solutions)
- Allow a solver to return extra arbitrary information via attributes (e.g., variable- and constraint-wise membership in an irreducible inconsistent subset for infeasibility analysis)
- Provide a greatly expanded set of status codes explaining what happened during the optimization procedure
- Enable a solver to more precisely specify which problem classes it supports
- Enable both primal and dual warm starts
- Enable adding and removing both variables and constraints by using reference objects instead of integer indices
- Enable any modification that the solver supports to an existing instance

This manual introduces the concepts needed to understand MOI and give a high-level picture of how all of the pieces fit together. The primary focus is on MOI from the perspective of a user of the interface. At the end of the manual we have a section on [Implementing a solver interface](@ref).
The reference page lists the complete API.

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
* the functions ``f_0, f_1, \ldots, f_m`` are specified by [`AbstractFunction`](@ref MathOptInterface.AbstractFunction) objects
* the sets ``\mathcal{S}_1, \ldots, \mathcal{S}_m`` are specified by [`AbstractSet`](@ref MathOptInterface.AbstractSet) objects

The current function types are:
* **scalar-valued variable-wise**: ``x_j``, a scalar variable defined by a variable reference
* **vector-valued variable-wise**: a vector of variables defined by a list of variable references
* **scalar-valued affine**: ``a^T x + b``, where ``a`` is a vector and ``b`` scalar
* **vector-valued affine**: ``A x + b``, where ``A`` is a matrix and ``b`` is a vector
* **scalar-valued quadratic**: ``\frac{1}{2} x^T Q x + a^T x + b``, where ``Q`` is a symmetric matrix, ``a`` is a vector, and ``b`` is a constant
* **vector-valued quadratic**: a vector of scalar-valued quadratic expressions

In a future version, MOI could be extended to cover functions defined by evaluation oracles (e.g., for nonlinear derivative-based optimization).

MOI defines some commonly used sets, but the interface is extensible to other sets recognized by the solver.
[Describe currently supported sets.]

## Solvers and solver instances

Solvers are "factories" used to specify solver-specific parameters and create new instances of a solver API.
Solver instances should be understood as the representation of the problem *in the solver's API*, just as if one were using its API directly.
When possible, the MOI wrapper for a solver should avoid storing an extra copy of the problem data.

Through the rest of the manual, `m` is used as a generic solver instance.

## Variables

MOI has a concept of a scalar variable (only).
New scalar variables are created with [`addvariable!`](@ref MathOptInterface.addvariable!) or [`addvariables!`](@ref MathOptInterface.addvariables!), which return a [`VariableReference`](@ref MathOptInterface.VariableReference) or `Vector{VariableReference}` respectively. Integer indices are never used to reference variables.

One uses `VariableReference` objects to set and get variable attributes. For example, the [`VariablePrimalStart`](@ref MathOptInterface.VariablePrimalStart) attribute is used to provide an initial starting point for a variable or collection of variables:
```julia
v = addvariable!(m)
setattribute!(m, VariablePrimalStart(), v, 10.5)
v2 = addvariables!(m, 3)
setattribute!(m, VariablePrimalStart(), v2, [1.3,6.8,-4.6])
```

A variable can be deleted from a model with [`delete!(::AbstractSolverInstance, ::VariableReference)`](@ref MathOptInterface.delete!(::MathOptInterface.AbstractSolverInstance, ::MathOptInterface.AnyReference)), if this functionality is supported by the solver.

## Functions

MOI defines six functions as listed in the definition of the [Standard form problem](@ref). The simplest function is [`ScalarVariablewiseFunction`](@ref MathOptInterface.ScalarVariablewiseFunction) defined as:
```julia
struct ScalarVariablewiseFunction <: AbstractFunction
    variable::VariableReference
end
```

If `v` is a `VariableReference` object, then `ScalarVariablewiseFunction(v)` is simply the scalar-valued function from the complete set of variables in an instance that returns the value of variable `v`. This function is useful for defining variablewise constraints.


A more interesting function is [`ScalarAffineFunction`](@ref MathOptInterface.ScalarAffineFunction), defined as
```julia
struct ScalarAffineFunction{T} <: AbstractFunction
    varables::Vector{VariableReference}
    coefficients::Vector{T}
    constant::T
end
```

If `x` is a vector of `VariableReference` objects, then `ScalarAffineFunction([x[1],x[2]],[5.0,-2.3],1.0)` represents the function ``5x_1 - 2.3x_2 + 1``.

Objective functions are assigned to an instance by calling [`setobjective!`](@ref MathOptInterface.setobjective!). For example,
```julia
x = addvariables!(m, 2)
setobjective!(m, MinSense, ScalarAffineFunction([x[1],x[2]],[5.0,-2.3],1.0))
```
sets the objective to the function just discussed in the minimization sense.

See [Functions and function modifications](@ref) for the complete list of functions.

## Sets

All constraints are specified with [`addconstraint!`](@ref MathOptInterface.addconstraint!)
by restricting the output of some function to a set.
The interface allows an arbitrary combination of functions at sets, but of course
solvers may decide to support only a small number of combinations (see [`supportsproblem`](@ref MathOptInterface.supportsproblem)).

For example, linear programming solvers should support, at least, combinations of affine functions with
the [`LessThan`](@ref MathOptInterface.LessThan) and [`GreaterThan`](@ref MathOptInterface.GreaterThan)
sets. These are simply linear constraints.
Scalar variablewise functions combined with these same sets are used to specify upper and lower bounds on variables.

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
x = addvariables!(m, 2)
setobjective!(m, MaxSense, ScalarAffineFunction(x, [3.0,2.0], 0.0))
addconstraint!(m, ScalarAffineFunction(x, [1.0,1.0], 0.0), LessThan(5.0))
addconstraint!(m, ScalarVariablewiseFunction(x[1]), GreaterThan(0.0))
addconstraint!(m, ScalarVariablewiseFunction(x[2]), GreaterThan(-1.0))
```

[Example with vector-valued set.]

[Table of common constraints and which pair of function/set they correspond to. Linear constraints, bounds, nonnegativity, second-order cone, PSD, quadratic, bilinear matrix inequality, indicator, SOS, ...]

## Solving and retrieving the results

Once a solver instance is loaded with the objective function and all of the constraints, we can ask the solver to solve the instance by calling [`optimize!`](@ref MathOptInterface.optimize!).
```julia
optimize!(m)
```

The optimization procedure may terminate for a number of reasons. The [`TerminationStatus`](@ref MathOptInterface.TerminationStatus) attribute of the solver instance returns a [`TerminationStatusCode`](@ref MathOptInterface.TerminationStatusCode) object which explains why the solver stopped. Some statuses indicate generally successful termination, some termination because of limit, and some termination because of something unexpected like invalid problem data or failure to converge. A typical usage of the `TerminationStatus` attribute is as follows:
```julia
status = getattribute(m, TerminationStatus())
if status == Success
    # Ok, the solver has a result to return
else
    # Handle other cases
    # The solver may or may not have a result
end
```

The `Success` status code specifically implies that the solver has a "result" to return. In the case that the solver converged to an optimal solution, this result will just be the optimal solution vector. The [`PrimalStatus`](@ref MathOptInterface.PrimalStatus) attribute returns a [`ResultStatusCode`](@ref MathOptInterface.ResultStatusCode) that explains how to interpret the result. In the case that the solver is known to return globally optimal solutions (up to numerical tolerances), the combination of `Success` termination status and `FeasiblePoint` primal result status implies that the primal result vector should be interpreted as a globally optimal solution. A result may be available even if the status is not `Success`, for example, if the solver stopped because of a time limit and has a feasible but nonoptimal solution. Use the [`ResultCount`](@ref MathOptInterface.ResultCount) attribute to check if one or more results are available.

In addition to the primal status, the [`DualStatus`](@ref MathOptInterface.DualStatus) provides important information for primal-dual solvers. The following table lists common situations and the corresponding termination and result statuses.

[Table of common situations and termination/primal/dual status]

See [Duals](@ref) for a discussion of the MOI conventions for primal-dual pairs and certificates.

Finally, to retrieve the values of each variable in the result, use the [`VariablePrimal`](@ref MathOptInterface.VariablePrimal) attribute:
```julia
getattribute(m, VariablePrimal(), x)
```
If `x` is a `VariableRefrence` then the function call returns a scalar, and if `x` is a `Vector{VariableReference}` then the call returns a vector of scalars. `VariablePrimal()` is equivalent to `VariablePrimal(1)`, i.e., the variable primal vector of the first result. Use `VariablePrimal(N)` to access the `N`th result.

See also the attributes [`ConstraintPrimal`](@ref MathOptInterface.ConstraintPrimal), and [`ConstraintDual`](@ref MathOptInterface.ConstraintDual).

## A complete example: `solveknapsack`


The `solveknapsack` function below demonstrates the complete process from data to solver instance to result vector using MOI.

[ needs formatting help ]

```julia
"""
    solveknapsack(c, w, C)

Solve the binary-constrained knapsack problem: max c'x: w'x <= C, x binary.
Returns the optimal weights and objective value. Throws an error if the solver
does not terminate with a `Success` status.
"""
function solveknapsack(c::Vector{Float64}, w::Vector{Float64}, C::Float64, solver::AbstractSolver)
    if !supportsproblem(solver, ScalarAffineFunction,
            [(ScalarAffineFunction,LessThan),
             (ScalarVariablewiseFunction,ZeroOne)])
        error("Provided solver cannot solve binary knapsack problems")
    end
    numvar = length(c)
    @assert numvar == length(w)

    m = SolverInstance(solver)

    # create the variables in the problem
    x = addvariables!(m, numvar)

    # set the objective function
    setobjective!(m, MaxSense, ScalarAffineFunction(x, c, 0.0))

    # add the knapsack constraint
    addconstraint!(m, ScalarAffineFunction(x, w, 0.0), LessThan(C))

    # add integrality constraints
    for i in 1:numvar
        addconstraint!(m, ScalarVariablewiseFunction(x[i]), ZeroOne())
    end

    # all set
    optimize!(m)

    termination_status = getattribute(m, TerminationStatus())
    objvalue = cangetattribute(m, ObjectiveValue()) ? getattribute(m, ObjectiveValue()) : NaN
    if termination_status != Success
        error("Solver terminated with status $termination_status")
    end

    @assert getattribute(m, ResultCount()) > 0

    result_status = getattribute(m, PrimalStatus())
    if result_status != FeasiblePoint
        error("Solver ran successfully did not return a feasible point. The problem may be infeasible.")
    end
    primal_variable_result = getattribute(m, VariableResult(), x)

    return (objvalue, primal_variable_result)
end
```

## A more complex example: `solveintegerlinear`


[this needs formatting help]

```julia
"""
    IntegerLinearResult

A `struct` returned by `solverintegerlinear` containing solution information.
The fields are as follows:

  - `termination_status`: the `TerminationStatusCode` returned by the solver
  - `result_status`: the `ResultStatusCode` returned by the solver (if any results are available)
  - `primal_variable_result`: the primal result vector returned by the solver; if no result is returned then this vector has length zero
  - `objective_value`: the objective value of the result vector as reported by the solver
  - `objective_bound`: the best known bound on the optimal objective value
"""
struct IntegerLinearResult
    termination_status::TerminationStatusCode
    result_status::ResultStatusCode
    primal_variable_result::Vector{Float64}
    objective_value::Float64
    objective_bound::Float64
end

"""
    solveintegerlinear(c, Ale, ble, Aeq, beq, lb, ub, integerindices, solver)

Solve the mixed-integer linear optimization problem: min c'x s.t. `Ale*x` <= `ble`, `Aeq*x` = `beq`, `lb` <= `x` <= `ub`, and`x[i]` is integer for `i` in `integerindices` using the solver specified by `solver`. Returns a `MixintprogSolution`.
"""
function solverintegerlinear(c, Ale::SparseMatrixCSC, ble, Aeq::SparseMatrixCSC, beq, lb, ub, integerindices, solver)
    if !supportsproblem(solver, ScalarAffineFunction,
            [(ScalarAffineFunction,LessThan),
             (ScalarAffineFunction,Zero),
             (ScalarVariablewiseFunction,LessThan),
             (ScalarVariablewiseFunction,GreaterThan),
             (ScalarVariablewiseFunction,Integers)])
        error("Provided solver does not support mixed-integer linear optimization")
    end
    numvar = size(Ale,2)
    @assert numvar == size(Aeq,2) == length(lb) == length(ub)


    m = SolverInstance(solver)

    # create the variables in the problem
    x = addvariables!(m, numvar)

    # set the objective function
    setobjective!(m, MinSense, ScalarAffineFunction(x, c, 0.0))

    # add variable bound constraints
    for i in 1:numvar
        if isfinite(lb[i])
            addconstraint!(m, ScalarVariablewiseFunction(x[i]), GreaterThan(lb[i]))
        end
        if isfinite(ub[i])
            addconstraint!(m, ScalarVariablewiseFunction(x[i]), LessThan(ub[i]))
        end
    end

    # add integrality constraints
    for i in integerindices
        @assert 1 <= i <= numvar
        addconstraint!(m, ScalarVariablewiseFunction(x[i]), Integers())
    end

    # convert a SparseMatrixCSC into a vector of scalar affine functions
    # meant to be illustrative, not the fastest possible
    function csc_to_affine(A::SparseMatrixCSC)
        nrow = size(A,1)
        variables_by_row = [Vector{VariableReference}(0) for k in 1:nrow]
        coefficients_by_row = [Vector{Float64}(0) for k in 1:nrow]

        I,J,V = findnz(A) # convert the sparse matrix to triplet form
        for p in 1:length(I)
            push!(variables_by_row[I[p]], x[J[p]])
            push!(coefficients_by_row[I[p]], V[p])
        end
        return [ScalarAffineFunction(variables_by_row[k], coefficients_by_row[k], 0.0) for k in 1:nrow]
    end

    # add inequality constraints
    Ale_affine = csc_to_affine(Ale)
    for k in 1:length(Ale_affine)
        addconstraint!(m, Ale_affine[k], LessThan(ble[k]))
    end

    # add equality constraints
    Aeq_affine = csc_to_affine(Aeq)
    for k in 1:length(Aeq_affine)
        addconstraint!(m, Aeq_affine[k], EqualTo(beq[k]))
    end

    # all set
    optimize!(m)

    termination_status = getattribute(m, TerminationStatus())
    objbound = cangetattribute(m, ObjectiveBound()) ? getattribute(m, ObjectiveBound()) : NaN
    objvalue = cangetattribute(m, ObjectiveValue()) ? getattribute(m, ObjectiveValue()) : NaN

    if getattribute(m, ResultCount()) > 0
        result_status = getattribute(m, PrimalStatus())
        primal_variable_result = getattribute(m, VariableResult(), x)
        return IntegerLinearResult(termination_status, result_status, primal_variable_result, objvalue, objbound)
    else
        return IntegerLinearResult(termination_status, UnknownResultStatus, Float64[], objvalue, objbound)
    end
end
```

## Advanced

### Duals

Currently, a convention for duals is not defined for problems with non-conic sets ``\mathcal{S}_i`` or quadratic functions ``f_0, f_i``.
Note that bound constraints are supported by re-interpretation in terms of the nonnegative or nonpositive cones.
An affine constraint ``a^T x + b \ge c`` should be interpreted as ``a^T x + b - c \in \mathbb{R}_+``, and similarly ``a^T x + b \le c`` should be interpreted as ``a^T x + b - c \in \mathbb{R}_-``.
Variable-wise constraints should be interpreted as affine constraints with the appropriate identity mapping in place of ``A_i``.

For such conic form minimization problems, the primal is:

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
& \;\;\text{s.t.} & a_0 - \sum_{i=1}^m A_i^T y_i & \in {0}^n
\\
& & y_i & \in \mathcal{C}_i^* & i = 1 \ldots m
\end{align}
```

where each ``\mathcal{C}_i`` is a closed convex cone and ``\mathcal{C}_i`` is its dual cone.

Note:
* lower bounds have nonnegative duals
* upper bounds have nonpositive duals
* closed convex cones have duals belonging to the corresponding dual cones

### Modifying an instance

[Explain `modifyconstraint!` and `modifyobjective!`.]

### Implementing a solver interface

[The interface is designed for multiple dispatch, e.g., attributes, combinations of sets and functions.]

[Avoid storing extra copies of the problem when possible.]

[It would be nice if solvers supported the multiple different ways to write the same constraint, e.g., `GreaterThan` and `Nonnegative`, second-order cone and rotated second-order cone.]
