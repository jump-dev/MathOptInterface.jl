# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module Benchmarks

import MathOptInterface as MOI

const BENCHMARKS = Dict{String,Function}()

"""
    suite(
        new_model::Function;
        exclude::Vector{Regex} = Regex[]
    )

Create a suite of benchmarks. `new_model` should be a function that takes no
arguments, and returns a new instance of the optimizer you wish to benchmark.

Use `exclude` to exclude a subset of benchmarks.

## BenchmarkTools

To use this function you must first install and load the `BenchmarkTools.jl`
package.

## Example

```julia
julia> import BenchmarkTools, GLPK, Gurobi

julia> MOI.Benchmarks.suite(GLPK.Optimizer)

julia> MOI.Benchmarks.suite(; exclude = [r"delete"]) do
           return Gurobi.Optimizer()
       end
```
"""
function suite end

"""
    create_baseline(suite, name::String; directory::String = ""; kwargs...)

Run all benchmarks in `suite` and save to files called `name` in `directory`.

Extra `kwargs` are based to `BenchmarkTools.run`.

## BenchmarkTools

To use this function you must first install and load the `BenchmarkTools.jl`
package.

## Example

```julia
julia> import BenchmarkTools, GLPK

julia> my_suite = MOI.Benchmarks.suite(GLPK.Optimizer);

julia> MOI.Benchmarks.create_baseline(
           my_suite,
           "glpk_master";
           directory = "/tmp",
           verbose = true,
       )
```
"""
function create_baseline end

"""
    compare_against_baseline(
        suite, name::String; directory::String = "",
        report_filename::String = "report.txt"
    )

Run all benchmarks in `suite` and compare against files called `name` in
`directory` that were created by a call to `create_baseline`.

A report summarizing the comparison is written to `report_filename` in
`directory`.

Extra `kwargs` are based to `BenchmarkTools.run`.

## BenchmarkTools

To use this function you must first install and load the `BenchmarkTools.jl`
package.

## Example

```julia
julia> import BenchmarkTools, GLPK

julia> my_suite = MOI.Benchmarks.suite(GLPK.Optimizer);

julia> MOI.Benchmarks.compare_against_baseline(
           my_suite,
           "glpk_master";
           directory = "/tmp",
           verbose = true,
       )
```
"""
function compare_against_baseline end

###
### Benchmarks
###

macro add_benchmark(f)
    name = f.args[1].args[1]
    return quote
        $(esc(f))
        BENCHMARKS[String($(Base.Meta.quot(name)))] = $(esc(name))
    end
end

@add_benchmark function add_variable(new_model)
    model = new_model()
    for i in 1:10_000
        MOI.add_variable(model)
    end
    return model
end

@add_benchmark function add_variables(new_model)
    model = new_model()
    MOI.add_variables(model, 10_000)
    return model
end

@add_benchmark function add_variable_constraint(new_model)
    model = new_model()
    x = MOI.add_variables(model, 10_000)
    for (i, xi) in enumerate(x)
        MOI.add_constraint(model, xi, MOI.LessThan(1.0 * i))
    end
    return model
end

@add_benchmark function add_variable_constraints(new_model)
    model = new_model()
    x = MOI.add_variables(model, 10_000)
    MOI.add_constraints(model, x, MOI.LessThan.(1.0:10_000.0))
    return model
end

@add_benchmark function delete_variable(new_model)
    model = new_model()
    x = MOI.add_variables(model, 1_000)
    MOI.add_constraint.(model, x, Ref(MOI.LessThan(1.0)))
    MOI.delete.(model, x)
    return model
end

@add_benchmark function delete_variable_constraint(new_model)
    model = new_model()
    x = MOI.add_variables(model, 1_000)
    cons = MOI.add_constraint.(model, x, Ref(MOI.LessThan(1.0)))
    for con in cons
        MOI.delete(model, con)
    end
    cons = MOI.add_constraint.(model, x, Ref(MOI.LessThan(1.0)))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(1.0, x), 0.0),
    )
    MOI.optimize!(model)
    for con in cons
        MOI.delete(model, con)
    end
    return model
end

@add_benchmark function add_constraint(new_model)
    model = new_model()
    index = MOI.add_variables(model, 10_000)
    for (i, x) in enumerate(index)
        MOI.add_constraint(
            model,
            MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0),
            MOI.LessThan(1.0 * i),
        )
    end
    return model
end

@add_benchmark function add_constraints(new_model)
    model = new_model()
    x = MOI.add_variables(model, 10_000)
    MOI.add_constraints(
        model,
        MOI.ScalarAffineFunction{Float64}[
            MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, xi)], 0.0) for
            xi in x
        ],
        MOI.LessThan.(1:1.0:10_000),
    )
    return model
end

@add_benchmark function delete_constraint(new_model)
    model = new_model()
    index = MOI.add_variables(model, 1_000)
    cons = Vector{
        MOI.ConstraintIndex{
            MOI.ScalarAffineFunction{Float64},
            MOI.LessThan{Float64},
        },
    }(
        undef,
        1_000,
    )
    for (i, x) in enumerate(index)
        cons[i] = MOI.add_constraint(
            model,
            MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0),
            MOI.LessThan(1.0 * i),
        )
    end
    for con in cons
        MOI.delete(model, con)
    end
    return model
end

@add_benchmark function copy_model(new_model)
    model = new_model()
    index = MOI.add_variables(model, 1_000)
    cons = Vector{
        MOI.ConstraintIndex{
            MOI.ScalarAffineFunction{Float64},
            MOI.LessThan{Float64},
        },
    }(
        undef,
        1_000,
    )
    for (i, x) in enumerate(index)
        cons[i] = MOI.add_constraint(
            model,
            MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0),
            MOI.LessThan(1.0 * i),
        )
    end

    model2 = new_model()
    MOI.copy_to(model2, model)
    # MOI.copy_to(model2, model, filter_constraints=(x) -> x in cons[1:500])

    return model2
end

end
