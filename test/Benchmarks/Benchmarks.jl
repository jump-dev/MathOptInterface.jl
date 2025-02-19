# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestBenchmarks

using Test

import MathOptInterface as MOI

function runtests()
    for name in names(@__MODULE__; all = true)
        if startswith("$(name)", "test_")
            @testset "$(name)" begin
                getfield(@__MODULE__, name)()
            end
        end
    end
    return
end

function test_suite()
    suite = MOI.Benchmarks.suite() do
        return MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    end
    @test length(suite.data) == length(MOI.Benchmarks.BENCHMARKS)
    suite = MOI.Benchmarks.suite(exclude = [r"delete_"]) do
        return MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    end
    # Note: update this value whenever more benchmarks are added to
    # `src/Benchmarks/Benchmarks.jl`.
    @test 6 <= length(suite.data) <= length(MOI.Benchmarks.BENCHMARKS) - 3
    return
end

function test_baseline()
    params = joinpath(@__DIR__, "baseline_params.json")
    baseline = joinpath(@__DIR__, "baseline_baseline.json")
    @test !isfile(params)
    @test !isfile(baseline)
    suite = MOI.Benchmarks.suite() do
        return MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    end
    @test_throws(
        ErrorException("You create a baseline with `create_baseline` first."),
        MOI.Benchmarks.compare_against_baseline(
            suite,
            "baseline";
            directory = @__DIR__,
            samples = 1,
            verbose = true,
        ),
    )
    MOI.Benchmarks.create_baseline(
        suite,
        "baseline";
        directory = @__DIR__,
        samples = 1,
        verbose = true,
    )
    @test isfile(params)
    @test isfile(baseline)
    suite = MOI.Benchmarks.suite() do
        return MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    end
    MOI.Benchmarks.compare_against_baseline(
        suite,
        "baseline";
        directory = @__DIR__,
        samples = 1,
        verbose = true,
    )
    rm(params)
    rm(baseline)
    report = read(joinpath(@__DIR__, "report.txt"), String)
    @test occursin("=> invariant", report)
    rm(joinpath(@__DIR__, "report.txt"))
    return
end

end

TestBenchmarks.runtests()
