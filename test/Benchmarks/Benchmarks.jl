using MathOptInterface, Test

const MOI = MathOptInterface
const MOIU = MOI.Utilities

const NUM_BENCHMARKS = length(MOI.Benchmarks.BENCHMARKS)

@testset "suite" begin
    suite = MOI.Benchmarks.suite() do
        return MOIU.MockOptimizer(MOIU.Model{Float64}())
    end
    @test length(suite.data) == NUM_BENCHMARKS

    suite = MOI.Benchmarks.suite(exclude = [r"delete_"]) do
        return MOIU.MockOptimizer(MOIU.Model{Float64}())
    end
    # Note: update this value whenever more benchmarks are added to
    # `src/Benchmarks/Benchmarks.jl`.
    @test 6 <= length(suite.data) <= NUM_BENCHMARKS - 3
end

@testset "Perform benchmark" begin
    params = joinpath(@__DIR__, "baseline_params.json")
    baseline = joinpath(@__DIR__, "baseline_baseline.json")
    @test !isfile(params)
    @test !isfile(baseline)
    @testset "create_baseline" begin
        suite = MOI.Benchmarks.suite() do
            return MOIU.MockOptimizer(MOIU.Model{Float64}())
        end
        MOI.Benchmarks.create_baseline(
            suite,
            "baseline";
            directory = @__DIR__,
            seconds = 2,
            verbose = true,
        )
    end
    @test isfile(params)
    @test isfile(baseline)
    @testset "compare_against_baseline" begin
        suite = MOI.Benchmarks.suite() do
            return MOIU.MockOptimizer(MOIU.Model{Float64}())
        end
        MOI.Benchmarks.compare_against_baseline(
            suite,
            "baseline";
            directory = @__DIR__,
            seconds = 2,
            verbose = true,
        )
    end
    rm(params)
    rm(baseline)
    @testset "Report" begin
        report = read(joinpath(@__DIR__, "report.txt"), String)
        @test occursin("=> invariant", report)
    end
    rm(joinpath(@__DIR__, "report.txt"))
end
