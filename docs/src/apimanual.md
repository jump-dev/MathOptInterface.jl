
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
- Be fast by allowing access to a solver's in-memory representation of a problem without writing intermediate files (when possible) and by using multiple dispatch and avoiding requiring containers of nonconcrete types
- Allow a solver to return multiple results (e.g., a pool of solutions)
- Allow a solver to return extra arbitrary information via attributes (e.g., variable- and constraint-wise membership in an irreducible inconsistent subset for infeasibility analysis)
- Provide a greatly expanded set of status codes explaining what happened during the optimization procedure
- Enable a solver to more precisely specify which problem classes it supports
- Enable both primal and dual warm starts
- Enable adding and removing both variables and constraints by using reference objects instead of integer indices
- Enable any modification that the solver supports to an existing instance
- Avoid requiring the solver wrapper to store an additional copy of the problem data

This manual introduces the concepts needed to understand MOI and give a high-level picture of how all of the pieces fit together. The primary focus is on MOI from the perspective of a user of the interface. At the end of the manual we have a section on [Implementing a solver interface](@ref).
The [API Reference](@ref) page lists the complete API.

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
* **projection onto a single coordinate**: ``x_j``, a single variable defined by a variable reference
* **projection onto multiple coordinates**: a subvector of variables defined by a list of variable references
* **scalar-valued affine**: ``a^T x + b``, where ``a`` is a vector and ``b`` scalar
* **vector-valued affine**: ``A x + b``, where ``A`` is a matrix and ``b`` is a vector
* **scalar-valued quadratic**: ``\frac{1}{2} x^T Q x + a^T x + b``, where ``Q`` is a symmetric matrix, ``a`` is a vector, and ``b`` is a constant
* **vector-valued quadratic**: a vector of scalar-valued quadratic expressions

In a future version, MOI could be extended to cover functions defined by evaluation oracles (e.g., for nonlinear derivative-based optimization).

MOI defines some commonly used sets, but the interface is extensible to other sets recognized by the solver.
[Describe currently supported sets.]

## Solvers and solver instances, and standalone instances

MOI defines three high-level objects that most users will interact with.

- A **Solver Instance** should be understood as the representation of an instance of an optimization problem *loaded in the solver's API*. That is, the instance data is often (i.e., whenever possible) stored exclusively in the external API, not duplicated in the MOI translation layer (called the *MOI wrapper*). Hence, the ability to modify data in a solver instance depends on whether the solver's own API supports such modifications. [`AbstractSolverInstance`](@ref MathOptInterface.AbstractSolverInstance) is the abstract type for solver instances.

- A **Solver** is a "factory" used to specify solver-specific parameters (e.g., algorithmic parameters, license keys, etc.) and create new solver instances. These are typically very lightweight objects. [`AbstractSolver`](@ref MathOptInterface.AbstractSolver) is the abstract type for solvers.

- A **Standalone Instance** is an explicit representation of an instance of an optimization problem unattached to any particular solver. The [MathOptInterfaceUtilities](https://github.com/JuliaOpt/MathOptInterfaceUtilities.jl) package provides an implementation of a standalone instance type which implements the MOI interface for setting up a problem and adding variables and constraints, but does not implement any methods related to solving it or querying the solution. There is no abstract base type for standalone instances.

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

MOI defines six functions as listed in the definition of the [Standard form problem](@ref). The simplest function is [`SingleVariable`](@ref MathOptInterface.SingleVariable) defined as:
```julia
struct SingleVariable <: AbstractFunction
    variable::VariableReference
end
```

If `v` is a `VariableReference` object, then `SingleVariable(v)` is simply the scalar-valued function from the complete set of variables in an instance that returns the value of variable `v`. One may also call this function a coordinate projection, which is more useful for defining constraints than as an objective function.


A more interesting function is [`ScalarAffineFunction`](@ref MathOptInterface.ScalarAffineFunction), defined as
```julia
struct ScalarAffineFunction{T} <: AbstractFunction
    variables::Vector{VariableReference}
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
The interface allows an arbitrary combination of functions and sets, but of course
solvers may decide to support only a small number of combinations (see [`supportsproblem`](@ref MathOptInterface.supportsproblem)).

For example, linear programming solvers should support, at least, combinations of affine functions with
the [`LessThan`](@ref MathOptInterface.LessThan) and [`GreaterThan`](@ref MathOptInterface.GreaterThan)
sets. These are simply linear constraints.
`SingleVariable` functions combined with these same sets are used to specify upper and lower bounds on variables.

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
addconstraint!(m, SingleVariable(x[1]), GreaterThan(0.0))
addconstraint!(m, SingleVariable(x[2]), GreaterThan(-1.0))
```

[Example with vector-valued set.]

### Constraints by function-set pairs

Below is a list of common constraint types and how they are represented
as function-set pairs in MOI. In the notation below, ``x`` is a vector of decision variables,
``x_i`` is a scalar decision variable, and all other terms are fixed constants.

[Define notation more precisely. ``a`` vector; ``A`` matrix; don't reuse ``u,l,b`` as scalar and vector]

#### Linear constraints

| Mathematical Constraint       | MOI Function                 | MOI Set        |
|-------------------------------|------------------------------|----------------|
| ``a^Tx \le u``                | `ScalarAffineFunction`       | `LessThan`     |
| ``a^Tx \ge l``                | `ScalarAffineFunction`       | `GreaterThan`  |
| ``a^Tx = b``                  | `ScalarAffineFunction`       | `EqualTo`      |
| ``l \le a^Tx \le u``          | `ScalarAffineFunction`       | `Interval`     |
| ``x_i \le u``                 | `SingleVariable`             | `LessThan`     |
| ``x_i \ge l``                 | `SingleVariable`             | `GreaterThan`  |
| ``x_i = b``                   | `SingleVariable`             | `EqualTo`      |
| ``l \le x_i \le u``           | `SingleVariable`             | `Interval`     |
| ``Ax + b \in \mathbb{R}_+^n`` | `VectorAffineFunction`       | `Nonnegatives` |
| ``Ax + b \in \mathbb{R}_-^n`` | `VectorAffineFunction`       | `Nonpositives` |
| ``Ax + b = 0``                | `VectorAffineFunction`       | `Zeros`        |

By convention, solvers are not expected to support nonzero constant terms in the `ScalarAffineFunction`s the first four rows above, because they are redundant with the parameters of the sets. For example, ``2x + 1 \le 2`` should be encoded as ``2x \le 1``.

Constraints with `SingleVariable` in `LessThan`, `GreaterThan`, `EqualTo`, or `Interval` sets have a natural interpretation as variable bounds. As such, it is typically not natural to impose multiple lower or upper bounds on the same variable, and by convention we do not ask solver interfaces to support this. It is natural, however, to impose upper and lower bounds separately as two different constraints on a single variable. The difference between imposing bounds by using a single `Interval` constraint and by using separate `LessThan` and `GreaterThan` constraints is that the latter will allow the solver to return separate dual multipliers for the two bounds, while the former will allow the solver to return only a single dual for the interval constraint.

[Define ``\mathbb{R}_+, \mathbb{R}_-``]

#### Conic constraints


| Mathematical Constraint                                       | MOI Function                 | MOI Set                            |
|---------------------------------------------------------------|------------------------------|------------------------------------|
| ``\lVert Ax + b\rVert_2 \le c^Tx + d``                        | `VectorAffineFunction`       | `SecondOrderCone`                  |
| ``y \ge \lVert x \rVert_2``                                   | `VectorOfVariables`          | `SecondOrderCone`                  |
| ``2yz \ge \lVert x \rVert_2^2, y,z \ge 0``                    | `VectorOfVariables`          | `RotatedSecondOrderCone`           |
| ``(a_1^Tx + b_1,a_2^Tx + b_2,a_3^Tx + b_3) \in \mathcal{E}``  | `VectorAffineFunction`       | `ExponentialCone`                  |
| ``A(x) \in \mathcal{S}_+``                                    | `VectorAffineFunction`       | `PositiveSemidefiniteConeTriangle` |
| ``A(x) \in \mathcal{S}'_+``                                   | `VectorAffineFunction`       | `PositiveSemidefiniteConeScaled`   |
| ``x \in \mathcal{S}_+``                                       | `VectorOfVariables`          | `PositiveSemidefiniteConeTriangle` |
| ``x \in \mathcal{S}'_+``                                      | `VectorOfVariables`          | `PositiveSemidefiniteConeScaled`   |


[Define ``\mathcal{E}`` (exponential cone), ``\mathcal{S}_+`` (smat), ``\mathcal{S}'_+`` (svec). ``A(x)`` is an affine function of ``x`` that outputs a matrix.]

#### Quadratic constraints


| Mathematical Constraint       | MOI Function                 | MOI Set                       |
|-------------------------------|------------------------------|-------------------------------|
| ``x^TQx + a^Tx + b \ge 0``    | `ScalarQuadraticFunction`    | `GreaterThan`                 |
| ``x^TQx + a^Tx + b \le 0``    | `ScalarQuadraticFunction`    | `LessThan`                    |
| ``x^TQx + a^Tx + b = 0``      | `ScalarQuadraticFunction`    | `EqualTo`                     |
| Bilinear matrix inequality    | `VectorQuadraticFunction`    | `PositiveSemidefiniteCone...` |


#### Discrete and logical constraints


| Mathematical Constraint                                                                    | MOI Function        | MOI Set                            |
|--------------------------------------------------------------------------------------------|---------------------|------------------------------------|
| ``x_i \in \mathbb{Z}``                                                                     | `SingleVariable`    | `Integer`                          |
| ``x_i \in \{0,1\}``                                                                        | `SingleVariable`    | `ZeroOne`                          |
| ``x_i \in \{0\} \cup [l,u]``                                                               | `SingleVariable`    | `Semicontinuous`                   |
| ``x_i \in \{0\} \cup \{l,l+1,\ldots,u-1,u\}``                                              | `SingleVariable`    | `Semiinteger`                      |
| At most one component of ``x`` can be nonzero                                              | `VectorOfVariables` | `SOS1`                             |
| At most two components of ``x`` can be nonzero, and if so they must be adjacent components | `VectorOfVariables` | `SOS2`                             |

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

In addition to the primal status, the [`DualStatus`](@ref MathOptInterface.DualStatus) provides important information for primal-dual solvers.

If a result is available, it may be retrieved with the [`VariablePrimal`](@ref MathOptInterface.VariablePrimal) attribute:
```julia
getattribute(m, VariablePrimal(), x)
```
If `x` is a `VariableRefrence` then the function call returns a scalar, and if `x` is a `Vector{VariableReference}` then the call returns a vector of scalars. `VariablePrimal()` is equivalent to `VariablePrimal(1)`, i.e., the variable primal vector of the first result. Use `VariablePrimal(N)` to access the `N`th result.

See also the attributes [`ConstraintPrimal`](@ref MathOptInterface.ConstraintPrimal), and [`ConstraintDual`](@ref MathOptInterface.ConstraintDual).
See [Duals](@ref) for a discussion of the MOI conventions for primal-dual pairs and certificates.



### Common status situations

The sections below describe how to interpret different status cases for three common classes of solvers. Most importantly, it is essential to know if a solver is expected to provide a global or only locally optimal solution when interpreting the result statuses. Solver wrappers may provide additional information on how the solver's statuses map to MOI statuses.

#### Primal-dual convex solver

A typical primal-dual solver is capable of certifying optimality of a solution to a convex optimization problem by providing a primal-dual feasible solution with matching objective values. It may also certify that either the primal or dual problem is infeasible by providing a certain ray of the dual or primal, respectively. Typically two solves are required to certify unboundedness, one to find a ray and a second to find a feasible point. A solver may also provide a [facial reduction certificate](http://www.optimization-online.org/DB_FILE/2015/09/5104.pdf). When a primal-dual solver terminates with `Success` status, it is reasonable to assume that a primal and dual statuses of `FeasiblePoint` imply that the corresponding primal-dual results are a (numerically) optimal primal-dual pair. The `AlmostSuccess` status implies that the solve has completed to relaxed tolerances, so in this case `FeasiblePoint` or `NearlyFeasiblePoint` statuses would imply a near-optimal primal-dual pair. For all other termination statuses, there are no specific guarantees on the results returned.

#### Global mixed-integer or nonconvex solver

When a global solver returns `Success` and the primal result is a `FeasiblePoint`, then it is implied that the primal result is indeed a globally optimal solution up to the specified tolerances. Typically, no dual certificate is available to certify optimality. The [`ObjectiveBound`](@ref MathOptInterface.ObjectiveBound) should provide extra information on the optimality gap.

#### Local solver

For solvers which perform a search based only on local criteria (for example, gradient descent), without additional knowledge of the structure of the problem, we can say only that `Success` and `FeasiblePoint` imply that the primal result belongs to the class of points which the chosen algorithm is known to converge to. Gradient descent algorithms may converge to saddle points, for example. It is also possible for such algorithms to converge to infeasible points, in which case the termination status would be `Success` and the primal result status would be `InfeasiblePoint`. This does not imply that the problem is infeasible and so cannot be called a certificate of infeasibility.


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
             (SingleVariable,ZeroOne)])
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
        addconstraint!(m, SingleVariable(x[i]), ZeroOne())
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
    primal_variable_result = getattribute(m, VariablePrimal(), x)

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

Solve the mixed-integer linear optimization problem: min c'x s.t. `Ale*x` <= `ble`, `Aeq*x` = `beq`, `lb` <= `x` <= `ub`, and`x[i]` is integer for `i` in `integerindices` using the solver specified by `solver`. Returns an `IntegerLinearResult`.
"""
function solverintegerlinear(c, Ale::SparseMatrixCSC, ble, Aeq::SparseMatrixCSC, beq, lb, ub, integerindices, solver)
    if !supportsproblem(solver, ScalarAffineFunction{Float64},
            [(ScalarAffineFunction,LessThan{Float64}),
             (ScalarAffineFunction,Zeros),
             (SingleVariable,LessThan{Float64}),
             (SingleVariable,GreaterThan{Float64}),
             (SingleVariable,Integer)])
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
            addconstraint!(m, SingleVariable(x[i]), GreaterThan(lb[i]))
        end
        if isfinite(ub[i])
            addconstraint!(m, SingleVariable(x[i]), LessThan(ub[i]))
        end
    end

    # add integrality constraints
    for i in integerindices
        @assert 1 <= i <= numvar
        addconstraint!(m, SingleVariable(x[i]), Integer())
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
        primal_variable_result = getattribute(m, VariablePrimal(), x)
        return IntegerLinearResult(termination_status, result_status, primal_variable_result, objvalue, objbound)
    else
        return IntegerLinearResult(termination_status, UnknownResultStatus, Float64[], objvalue, objbound)
    end
end
```

## Advanced

### Duals


Conic duality is the starting point for MOI's duality conventions. When all functions are affine (or coordinate projections), and all constraint sets are closed convex cone, the instance may be called a conic optimization problem.
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


Currently, a convention for duals is not defined for problems with non-conic sets ``\mathcal{S}_i`` or quadratic functions ``f_0, f_i``.


### Modifying an instance

[Explain `modifyconstraint!` and `modifyobjective!`.]

### Implementing a solver interface

[The interface is designed for multiple dispatch, e.g., attributes, combinations of sets and functions.]

[Avoid storing extra copies of the problem when possible.]

MOI defines a very general interface, with multiple possible ways to describe the same constraint. This is considered a feature, not a bug. MOI is designed to make it possible to experiment with alternative representations of an optimization problem at both the solving and modeling level. When implementing an interface, it is important to keep in mind that the constraints which a solver supports via MOI will have a near 1-to-1 correspondence with how users can express problems in JuMP, because JuMP does not perform automatic transformations. (Alternative systems like Convex.jl do.) For example, if a solver wrapper does not support `ScalarAffineFunction`-in-`LessThan` constraints, then users will not be able to write: `@constraint(m, 2x + y <= 10)` in JuMP. That said, from the perspective of JuMP, solvers can safely choose to not support the following constraints:

- `ScalarAffineFunction` in `GreaterThan`, `LessThan`, or `EqualTo` with a nonzero constant in the function. Constants in the affine function should instead be moved into the parameters of the corresponding sets.

- `ScalarAffineFunction` in `Nonnegative`, `Nonpositive` or `Zeros`. Alternative constraints are available by using a `VectorAffineFunction` with one output row or `ScalarAffineFunction` with `GreaterThan`, `LessThan`, or `EqualTo`.

- Two `SingleVariable`-in-`LessThan` constraints applied to the same variable (similarly with `GreaterThan`). These should be interpreted as variable bounds, and each variable naturally has at most one upper or lower bound.

There is no special interface for column generation. If the solver has a special API for setting
coefficients in existing constraints when adding a new variable, it is possible
to queue modifications and new variables and then call the solver's API once all of the
new coefficients are known.

All data passed to the solver should be copied immediately to internal data structures. Solvers may not modify any input vectors and should not assume that input vectors will not be modified by users in the future. This applies, for example, to the coefficient vector in `ScalarAffineFunction`. Vectors returned to the user, e.g., via `ObjectiveFunction` or `ConstraintFunction` attributes should not be modified by the solver afterwards. The in-place version of `getattribute!` can be used by users to avoid extra copies in this case.

Solver wrappers should document how the low-level solver statuses map to the MOI statuses. In particular, the characterization of a result with status `FeasiblePoint` and termination status `Success` is entirely solver defined. It may or may not be a globally optimal solution. Solver wrappers are not responsible for verifying the feasibility of results. Statuses like `NearlyFeasiblePoint`, `InfeasiblePoint`, `NearlyInfeasiblePoint`, and `NearlyReductionCertificate` are designed to be used when the solver explicitly indicates as much.

MOI solver interfaces may be in the same package as the solver itself (either the C wrapper if the solver is accessible through C, or the Julia code if the solver is written in Julia, for example). In some cases it may be more appropriate to host the MOI wrapper in its own package; in this case it is recommended that the MOI wrapper package be named `MathOptInterfaceXXX` where `XXX` is the solver name.
