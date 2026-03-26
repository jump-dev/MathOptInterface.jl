# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module MathOptInterfaceBenchmarkToolsExt

import BenchmarkTools
import MathOptInterface as MOI

function MOI.Benchmarks.suite(
    new_model::Function;
    exclude::Vector{Regex} = Regex[],
)
    group = BenchmarkTools.BenchmarkGroup()
    for (name, func) in MOI.Benchmarks.BENCHMARKS
        if any(occursin.(exclude, Ref(name)))
            continue
        end
        group[name] = BenchmarkTools.@benchmarkable $func($new_model)
    end
    return group
end

function MOI.Benchmarks.create_baseline(
    suite::BenchmarkTools.BenchmarkGroup,
    name::String;
    directory::String = "",
    kwargs...,
)
    BenchmarkTools.tune!(suite)
    BenchmarkTools.save(
        joinpath(directory, name * "_params.json"),
        BenchmarkTools.params(suite),
    )
    results = BenchmarkTools.run(suite; kwargs...)
    BenchmarkTools.save(joinpath(directory, name * "_baseline.json"), results)
    return
end

function MOI.Benchmarks.compare_against_baseline(
    suite::BenchmarkTools.BenchmarkGroup,
    name::String;
    directory::String = "",
    report_filename::String = "report.txt",
    kwargs...,
)
    params_filename = joinpath(directory, name * "_params.json")
    baseline_filename = joinpath(directory, name * "_baseline.json")
    if !isfile(params_filename) || !isfile(baseline_filename)
        error("You create a baseline with `create_baseline` first.")
    end
    BenchmarkTools.loadparams!(
        suite,
        BenchmarkTools.load(params_filename)[1],
        :evals,
        :samples,
    )
    new_results = BenchmarkTools.run(suite; kwargs...)
    old_results = BenchmarkTools.load(baseline_filename)[1]
    open(joinpath(directory, report_filename), "w") do io
        println(stdout, "\n========== Results ==========")
        println(io, "\n========== Results ==========")
        for key in keys(new_results)
            judgement = BenchmarkTools.judge(
                BenchmarkTools.median(new_results[key]),
                BenchmarkTools.median(old_results[key]),
            )
            println(stdout, "\n", key)
            println(io, "\n", key)
            show(stdout, MIME"text/plain"(), judgement)
            show(io, MIME"text/plain"(), judgement)
        end
    end
    return
end

end # module
