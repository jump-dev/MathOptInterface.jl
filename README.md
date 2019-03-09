# MathOptFormat.jl
#### _A new file format for mathematical optimization_

| **Build Status** | **Coverage** |
|:--------------------:|:----------------:|
| [![Build Status][build-img]][build-url] | [![Codecov branch][codecov-img]][codecov-url]

[build-img]: https://travis-ci.org/odow/MathOptFormat.jl.svg?branch=master
[build-url]: https://travis-ci.org/odow/MathOptFormat.jl

[codecov-img]: https://codecov.io/github/odow/MathOptFormat.jl/coverage.svg?branch=master
[codecov-url]: https://codecov.io/github/odow/MathOptFormat.jl?branch=master

Read and write [MathOptInterface](https://github.com/JuliaOpt/MathOptInterface.jl)
models to a variety of mathematical optimization file formats.

In order to read or write a mathematical optimization problem to/from a file,
first create a [MathOptInterface](https://github.com/JuliaOpt/MathOptInterface.jl)
model. This can be done using [JuMP](https://github.com/JuliaOpt/JuMP.jl) or via
one of the solver packages, for example, [GLPK.jl](https://github.com/JuliaOpt/GLPK.jl).

In the rest of this documentation, we assume that this model is a Julia variable
named `user_model`.

## Write to file

To write `user_model` to file `filename`, follow these steps.

1. Create a MathOptFormat model `mathoptformat_model`
    - `mathoptformat_model = MathOptFormat.MOF.Model()`
2. Copy `user_model` into `mathoptformat_model`
    - `MOI.copy_to(mathoptformat_model, user_model)`
3. Write `mathoptformat_model` to `filename`
    - `MOI.write_to_file(mathoptformat_model, filename)`

Step 1) assumes that you want to write a MathOptFormat file (.mof.json). For
other formats, replace `MathOptFormat.MOF.Model` with an appropriate model. See
[Supported file formats](@ref) for details.

## Read from file

To read a file `filename` into `user_model`, steps 2 and 3 are reversed:

1. Create a MathOptFormat model `mathoptformat_model`
    - `mathoptformat_model = MathOptFormat.MOF.Model()`
2. Read `filename` into `mathoptformat_model`
    - `MOI.read_from_file(mathoptformat_moodel, filename)`
3. Copy `mathoptformat_model` into `user_model`
    - `MOI.copy_to(user_model, mathoptformat_model)`

Step 1) assumes that you want to read a MathOptFormat file (.mof.json). For
other formats, replace `MathOptFormat.MOF.Model` with an appropriate model. See
[Supported file formats](@ref) for details.

## Supported file formats

File-formats supported are

 - Conic benchmark format (.cbf):
    - Use `MathOptFormat.CBF.Model`
 - Linear programming format (.lp):
    - Use `MathOptFormat.LP.Model`
 - MathOptFormat (.mof.json):
    - Use `MathOptFormat.MOF.Model`
 - Mathematical programming system (.mps):
    - Use `MathOptFormat.MPS.Model`

#### Notes

 - The LP file format does not (yet) support `MOI.read_from_file`.
 - The MathOptFormat file format (.mof.json) is under active development. No
backward compatibility yet!

## GZipped files

To read and write GZip'ed files, append `.gz` to the filename. For example:
```julia
# Uncompressed version:
MOI.write_to_file(model, "my_model.mps")
# Compressed version:
MOI.write_to_file(model, "my_model.mps.gz")
```
