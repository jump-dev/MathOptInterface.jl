
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

[Examples of sets and how to use them. How to add constraints.]

## Solving and retrieving the results

[Example of calling `optimize!` and getting the status and results back.]


## A complete example: `mixintprog`

[Showcase of how to go from data to MOI instance to `optimize!` to results, by implementing the `mixintprog` function]

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

[Discussion for potential authors of solver interfaces]
