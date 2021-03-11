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
    This documentation is aimed at developers writing software interfaces to
    solvers and modeling languages using the MathOptInterface API. If you are a
    user interested in solving optimization problems, we encourage you instead
    to use MOI through a higher-level modeling interface like
    [JuMP](https://github.com/jump-dev/JuMP.jl) or
    [Convex.jl](https://github.com/jump-dev/Convex.jl).

## How the documentation is structured

Having a high-level overview of how this documentation is structured will help
you know where to look for certain things.

* The **Background** section contains articles on the motivation and theory
  behind MathOptInterface. Look here if you want to understand _why_, rather
  than _how_.
* The **Manual** contains short code-snippets that explain how to use the MOI
  API. Look here if you want to write a model in MOI.
* The **API Reference** contains a complete list of functions and types that
  comprise the MOI API. Look here is you want to know how to use (or implement)
  a particular function.
* The **Submodules** section contains stand-alone documentation for each of the
  submodules within MOI. These submodules are not required to interface a solver
  with MOI, but they make the job much easier.

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
