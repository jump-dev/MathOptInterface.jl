# MathOptFormat.jl

| **Build Status** | **Coverage** |
|:--------------------:|:----------------:|
| [![Build Status][build-img]][build-url] | [![Codecov branch][codecov-img]][codecov-url]

[build-img]: https://travis-ci.org/odow/MathOptFormat.jl.svg?branch=master
[build-url]: https://travis-ci.org/odow/MathOptFormat.jl

[codecov-img]: https://codecov.io/github/odow/MathOptFormat.jl/coverage.svg?branch=master
[codecov-url]: https://codecov.io/github/odow/MathOptFormat.jl?branch=master

Read and write [MathOptInterface](https://github.com/JuliaOpt/MathOptInterface.jl)
models to a variety of mathematical optimization file formats.

### Using JuMP? Read the [JuMP integration](#jump-integration) section

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

#### Example

```julia
using MathOptInterface, MathOptFormat, GLPK
const MOI = MathOptInterface

user_model = GLPK.Optimizer()
x = MOI.add_variable(user_model)
MOI.set(user_model, MOI.VariableName(), x, "x")
MOI.add_constraint(user_model, MOI.SingleVariable(x), MOI.GreaterThan(0.0))

mathoptformat_model = MathOptFormat.MPS.Model()
MOI.copy_to(mathoptformat_model, user_model)
MOI.write_to_file(mathoptformat_model, "my_model.mps")
```

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

#### Example

```julia
using MathOptInterface, MathOptFormat, GLPK
const MOI = MathOptInterface

mathoptformat_model = MathOptFormat.MPS.Model()
MOI.read_from_file(mathoptformat_model, "my_model.mps")

user_model = GLPK.Optimizer()
MOI.copy_to(user_model, mathoptformat_model)

x = MOI.get(user_model, MOI.VariableIndex, "x")
```

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

## JuMP integration

To write a JuMP model to a file, pass `backend(jump_model)` to `MOI.copy_to` 
instead of `user_model`:
```julia
using JuMP, MathOptFormat

jump_model = Model()
@variable(jump_model, x, Int)
@constraint(jump_model, my_con, 2x + 1 <= 2)
@objective(jump_model, Max, x)

mps_model = MathOptFormat.MPS.Model()
MOI.copy_to(mps_model, backend(jump_model))
MOI.write_to_file(mps_model, "my_model.mps")
```
