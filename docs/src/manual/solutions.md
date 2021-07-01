```@meta
CurrentModule = MathOptInterface
DocTestSetup = quote
    using MathOptInterface
    const MOI = MathOptInterface
end
DocTestFilters = [r"MathOptInterface|MOI"]
```

# [Solutions](@id manual_solutions)

## Solving and retrieving the results

Once an optimizer is loaded with the objective function and all of the
constraints, we can ask the solver to solve the model by calling
[`optimize!`](@ref).
```julia
MOI.optimize!(optimizer)
```

## Why did the solver stop?

The optimization procedure may terminate for a number of reasons. The
[`TerminationStatus`](@ref) attribute of the optimizer returns a
[`TerminationStatusCode`](@ref) object which explains why the solver stopped.

The termination statuses distinguish between proofs of optimality,
infeasibility, local convergence, limits, and termination because of something
unexpected like invalid problem data or failure to converge.

A typical usage of the [`TerminationStatus`](@ref) attribute is as follows:
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

## Primal solutions

Use the [`PrimalStatus`](@ref) optimizer attribute to return a
[`ResultStatusCode`](@ref) describing the status of the primal solution.

Common returns are described below in the [Common status situations](@ref)
section.

Query the primal solution using the [`VariablePrimal`](@ref) and
[`ConstraintPrimal`](@ref) attributes.

Query the objective function value using the [`ObjectiveValue`](@ref) attribute.

## Dual solutions

!!! warning
    See [Duality](@ref) for a discussion of the MOI conventions for primal-dual
    pairs and certificates.

Use the [`DualStatus`](@ref) optimizer attribute to return a
[`ResultStatusCode`](@ref) describing the status of the dual solution.

Query the dual solution using the [`ConstraintDual`](@ref) attribute.

Query the dual objective function value using the [`DualObjectiveValue`](@ref)
attribute.

## Common status situations

The sections below describe how to interpret typical or interesting status cases
for three common classes of solvers. The example cases are illustrative, not
comprehensive. Solver wrappers may provide additional information on
how the solver's statuses map to MOI statuses.

!!! info
    `*` in the tables indicate that multiple different values are possible.

### Primal-dual convex solver

Linear programming and conic optimization solvers fall into this category.

| What happened?                          | `TerminationStatus` | `ResultCount` | `PrimalStatus`              | `DualStatus`                |
| --------------------------------------- | --------------------- | ----------- | --------------------------- | --------------------------- |
| Proved optimality                       | `OPTIMAL`             | 1           | `FEASIBLE_POINT`            | `FEASIBLE_POINT`            |
| Proved infeasible                       | `INFEASIBLE`          | 1           | `NO_SOLUTION`               | `INFEASIBILITY_CERTIFICATE` |
| Optimal within relaxed tolerances       | `ALMOST_OPTIMAL`      | 1           | `FEASIBLE_POINT`            | `FEASIBLE_POINT`            |
| Optimal within relaxed tolerances       | `ALMOST_OPTIMAL`      | 1           | `ALMOST_FEASIBLE_POINT`     | `ALMOST_FEASIBLE_POINT`     |
| Detected an unbounded ray of the primal | `DUAL_INFEASIBLE`     | 1           | `INFEASIBILITY_CERTIFICATE` | `NO_SOLUTION`               |
| Stall                                   | `SLOW_PROGRESS`       | 1           | *                           | *                           |

### Global branch-and-bound solvers

Mixed-integer programming solvers fall into this category.

| What happened?                                   | `TerminationStatus`       | `ResultCount` | `PrimalStatus`     | `DualStatus`  |
| ------------------------------------------------ | ------------------------- | ------------- | ------------------ | ------------- |
| Proved optimality                                | `OPTIMAL`                 | 1             | `FEASIBLE_POINT`   | `NO_SOLUTION` |
| Presolve detected infeasibility or unboundedness | `INFEASIBLE_OR_UNBOUNDED` | 0             | `NO_SOLUTION`      | `NO_SOLUTION` |
| Proved infeasibility                             | `INFEASIBLE`              | 0             | `NO_SOLUTION`      | `NO_SOLUTION` |
| Timed out (no solution)                          | `TIME_LIMIT`              | 0             | `NO_SOLUTION`      | `NO_SOLUTION` |
| Timed out (with a solution)                      | `TIME_LIMIT`              | 1             | `FEASIBLE_POINT`   | `NO_SOLUTION` |
| `CPXMIP_OPTIMAL_INFEAS`                          | `ALMOST_OPTIMAL`          | 1             | `INFEASIBLE_POINT` | `NO_SOLUTION` |

!!! info
    [`CPXMIP_OPTIMAL_INFEAS`](https://www.ibm.com/support/knowledgecenter/en/SSSA5P_12.6.1/ilog.odms.cplex.help/refcallablelibrary/macros/CPXMIP_OPTIMAL_INFEAS.html)
    is a CPLEX status that indicates that a preprocessed problem was solved to
    optimality, but the solver was unable to recover a feasible solution to the
    original problem. Handling this status was one of the motivating drivers
    behind the design of MOI.

### Local search solvers

Nonlinear programming solvers fall into this category. It also includes
non-global tree search solvers like
[Juniper](https://github.com/lanl-ansi/Juniper.jl).

| What happened?                                         | `TerminationStatus`             | `ResultCount` | `PrimalStatus`   | `DualStatus`   |
| ------------------------------------------------------ | --------------------------------- | --------------- | ------------------ | ---------------- |
| Converged to a stationary point                        | `LOCALLY_SOLVED`                  | 1               | `FEASIBLE_POINT`   | `FEASIBLE_POINT` |
| Completed a non-global tree search (with a solution)   | `LOCALLY_SOLVED`                  | 1               | `FEASIBLE_POINT`   | `FEASIBLE_POINT` |
| Converged to an infeasible point                       | `LOCALLY_INFEASIBLE`              | 1               | `INFEASIBLE_POINT` | *                |
| Completed a non-global tree search (no solution found) | `LOCALLY_INFEASIBLE`              | 0               | `NO_SOLUTION`      | `NO_SOLUTION`    |
| Iteration limit                                        | `ITERATION_LIMIT`                 | 1               | *                  | *                |
| Diverging iterates                                     | `NORM_LIMIT` or `OBJECTIVE_LIMIT` | 1               | *                  | *                |
