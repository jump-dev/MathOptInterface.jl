# Motivation

MathOptInterface (MOI) is a replacement for
[MathProgBase](https://github.com/JuliaOpt/MathProgBase.jl),
the first-generation abstraction layer for mathematical optimization previously
used by
[JuMP](https://github.com/jump-dev/JuMP.jl) and
[Convex.jl](https://github.com/jump-dev/Convex.jl).

To address a number of limitations of MathProgBase, MOI is designed to:

- Be simple and extensible
  * unifying linear, quadratic, and conic optimization,
  * seamlessly facilitating extensions to essentially arbitrary constraints and
    functions (e.g., indicator constraints, complementarity constraints, and
    piecewise-linear functions)
- Be fast
   * by allowing access to a solver's in-memory representation of a problem
     without writing intermediate files (when possible)
   * by using multiple dispatch and avoiding requiring containers of nonconcrete
     types
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
