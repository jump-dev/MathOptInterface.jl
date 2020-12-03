module Benchmarks

using BenchmarkTools, MathOptInterface

const MOI = MathOptInterface
const BENCHMARKS = Dict{String, Function}()

"""
    suite(
        new_model::Function;
        exclude::Vector{Regex} = Regex[]
    )

Create a suite of benchmarks. `new_model` should be a function that takes no
arguments, and returns a new instance of the optimizer you wish to benchmark.

Use `exclude` to exclude a subset of benchmarks.

### Examples

```julia
suite() do
    GLPK.Optimizer()
end
suite(exclude = [r"delete"]) do
    Gurobi.Optimizer(OutputFlag=0)
end
```
"""
function suite(new_model::Function; exclude::Vector{Regex} = Regex[])
    group = BenchmarkGroup()
    for (name, func) in BENCHMARKS
        any(occursin.(exclude, Ref(name))) && continue
        group[name] = @benchmarkable $func($new_model)
    end
    return group
end

"""
    create_baseline(suite, name::String; directory::String = ""; kwargs...)

Run all benchmarks in `suite` and save to files called `name` in `directory`.

Extra `kwargs` are based to `BenchmarkTools.run`.

### Examples

```julia
my_suite = suite(() -> GLPK.Optimizer())
create_baseline(my_suite, "glpk_master"; directory = "/tmp", verbose = true)
```
"""
function create_baseline(
    suite::BenchmarkTools.BenchmarkGroup, name::String; directory::String = "",
    kwargs...
)
    tune!(suite)
    BenchmarkTools.save(joinpath(directory, name * "_params.json"), params(suite))
    results = run(suite; kwargs...)
    BenchmarkTools.save(joinpath(directory, name * "_baseline.json"), results)
    return
end

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

### Examples

```julia
my_suite = suite(() -> GLPK.Optimizer())
compare_against_baseline(
    my_suite, "glpk_master"; directory = "/tmp", verbose = true
)
```
"""
function compare_against_baseline(
    suite::BenchmarkTools.BenchmarkGroup, name::String;
    directory::String = "", report_filename::String = "report.txt", kwargs...
)
    params_filename = joinpath(directory, name * "_params.json")
    baseline_filename = joinpath(directory, name * "_baseline.json")
    if !isfile(params_filename) || !isfile(baseline_filename)
        error("You create a baseline with `create_baseline` first.")
    end
    loadparams!(
        suite, BenchmarkTools.load(params_filename)[1], :evals, :samples
    )
    new_results = run(suite; kwargs...)
    old_results = BenchmarkTools.load(baseline_filename)[1]
    open(joinpath(directory, report_filename), "w") do io
        println(stdout, "\n========== Results ==========")
        println(io,     "\n========== Results ==========")
        for key in keys(new_results)
            judgement = judge(
                BenchmarkTools.median(new_results[key]),
                BenchmarkTools.median(old_results[key])
            )
            println(stdout, "\n", key)
            println(io,     "\n", key)
            show(stdout, MIME"text/plain"(), judgement)
            show(io, MIME"text/plain"(), judgement)
        end
    end
    return
end

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
        MOI.add_constraint(model, MOI.SingleVariable(xi), MOI.LessThan(1.0 * i))
    end
    return model
end

@add_benchmark function add_variable_constraints(new_model)
    model = new_model()
    x = MOI.add_variables(model, 10_000)
    MOI.add_constraints(
        model,
        MOI.SingleVariable.(x),
        MOI.LessThan.(1.0:10_000.0)
    )
    return model
end

@add_benchmark function delete_variable(new_model)
    model = new_model()
    x = MOI.add_variables(model, 1_000)
    MOI.add_constraint.(model, MOI.SingleVariable.(x), Ref(MOI.LessThan(1.0)))
    MOI.delete.(model, x)
    return model
end

@add_benchmark function delete_variable_constraint(new_model)
    model = new_model()
    x = MOI.add_variables(model, 1_000)
    cons = MOI.add_constraint.(model, MOI.SingleVariable.(x), Ref(MOI.LessThan(1.0)))
    for con in cons
        MOI.delete(model, con)
    end
    cons = MOI.add_constraint.(model, MOI.SingleVariable.(x), Ref(MOI.LessThan(1.0)))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.set(model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(1.0, x), 0.0)
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
            MOI.LessThan(1.0 * i)
        )
    end
    return model
end

@add_benchmark function add_constraints(new_model)
    model = new_model()
    x = MOI.add_variables(model, 10_000)
    MOI.add_constraints(
        model,
        [MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, xi)], 0.0) for xi in x],
        MOI.LessThan.(1:1.0:10_000)
    )
    return model
end

@add_benchmark function delete_constraint(new_model)
    model = new_model()
    index = MOI.add_variables(model, 1_000)
    cons = Vector{
        MOI.ConstraintIndex{MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}}
    }(undef, 1_000)
    for (i, x) in enumerate(index)
        cons[i] = MOI.add_constraint(
            model,
            MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0),
            MOI.LessThan(1.0 * i)
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
        MOI.ConstraintIndex{MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}}
    }(undef, 1_000)
    for (i, x) in enumerate(index)
        cons[i] = MOI.add_constraint(
            model,
            MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0),
            MOI.LessThan(1.0 * i)
        )
    end
    
    model2 = new_model()
    MOI.copy_to(model2, model)
    # MOI.copy_to(model2, model, filter_constraints=(x) -> x in cons[1:500])

    return model2
end

end
