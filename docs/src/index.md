# MathOptInterface

Each mathematical optimization solver API has its own concepts and data
structures for representing optimization models and obtaining results.
However, it is often desirable to represent an instance of an optimization
problem at a higher level so that it is easy to try using different solvers.

[MathOptInterface.jl](https://github.com/jump-dev/MathOptInterface.jl) (MOI) is
an abstraction layer designed to provide a unified interface to mathematical
optimization solvers so that users do not need to understand multiple
solver-specific APIs.

MOI can be used directly, or through a higher-level modeling interface like
[JuMP](https://github.com/jump-dev/JuMP.jl).

## Background

MOI has been designed to replace [MathProgBase](https://github.com/JuliaOpt/MathProgBase.jl),
which was been used by modeling packages such as [JuMP](https://github.com/jump-dev/JuMP.jl)
and [Convex.jl](https://github.com/jump-dev/Convex.jl).

This second-generation abstraction layer addresses a number of limitations of
MathProgBase. MOI is designed to:
- Be simple and extensible, unifying linear, quadratic, and conic optimization,
  and seamlessly facilitate extensions to essentially arbitrary constraints and
  functions (e.g., indicator constraints, complementarity constraints, and
  piecewise-linear functions)
- Be fast by allowing access to a solver's in-memory representation of a problem
  without writing intermediate files (when possible) and by using multiple
  dispatch and avoiding requiring containers of nonconcrete types
- Allow a solver to return multiple results (e.g., a pool of solutions)
- Allow a solver to return extra arbitrary information via attributes (e.g.,
  variable- and constraint-wise membership in an irreducible inconsistent subset
  for infeasibility analysis)
- Provide a greatly expanded set of status codes explaining what happened during
  the optimization procedure
- Enable a solver to more precisely specify which problem classes it supports
- Enable both primal and dual warm starts
- Enable adding and removing both variables and constraints by indices that are
  not required to be consecutive
- Enable any modification that the solver supports to an existing model
- Avoid requiring the solver wrapper to store an additional copy of the problem
  data

## Sections of this documentation

There are two main sections to this documentation.

This manual introduces the concepts needed to understand MOI and give a
high-level picture of how all of the pieces fit together. The primary focus is
on MOI from the perspective of a user of the interface. At the end of the manual
we have a section on [Implementing a solver interface](@ref).

The [API Reference](@ref) page lists the complete API.
