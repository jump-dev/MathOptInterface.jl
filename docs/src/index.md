# MathOptInterface

!!! warning
    This documentation is still under construction. If you need help with JuMP,
    read the [JuMP documentation](https://jump.dev/JuMP.jl/stable/) instead. If
    you are writing a solver interface and need help with MOI, join the
    [developer chatroom](https://gitter.im/JuliaOpt/JuMP-dev) and ask away!

[MathOptInterface.jl](https://github.com/jump-dev/MathOptInterface.jl) (MOI) is
an abstraction layer designed to provide a unified interface to mathematical
optimization solvers so that users do not need to understand multiple
solver-specific APIs.

!!! tip
    While MOI can be used directly, we encourage you instead to use MOI through
    a higher-level modeling interface like [JuMP](https://github.com/jump-dev/JuMP.jl)
    or [Convex.jl](https://github.com/jump-dev/Convex.jl).

## How the documentation is structured

Having a high-level overview of how this documentation is structured will help
you know where to look for certain things.

* The **Manual** contains short code-snippets that explain how to use the MOI
  API. Look here if you want to write a model in MOI.
* The **API Reference** contains a complete list of functions and types that
  comprise the MOI API. Look here is you want to know how to use (or implement)
  a particular function.
* The **Submodules** section contains stand-alone documentation for each of the
  submodules within MOI. These submodules are not required to interface a solver
  with MOI, but they make the job much easier.

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

## Citing JuMP

A [paper describing the design and features of MathOptInterface](https://arxiv.org/abs/2002.03447)
is available on [arXiv](https://arxiv.org).

If you find MathOptInterface useful in your work, we kindly request that you
cite the following paper:
```
@misc{
    legat2020mathoptinterface,
    title = {MathOptInterface: a data structure for mathematical optimization problems},
    author = {Beno{\^i}t Legat and Oscar Dowson and Joaquim Dias Garcia and Miles Lubin},
    year = {2020},
    eprint = {2002.03447},
    archivePrefix = {arXiv},
    primaryClass = {math.OC},
    url = {https://arxiv.org/abs/2002.03447},
}
```
