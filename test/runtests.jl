using MathOptFormat
using Test

const MOI = MathOptFormat.MOI
const MOIU = MOI.Utilities

@testset "MathOptFormat tests" begin
    @testset "$(file)" for file in ["CBF", "LP", "MOF", "MPS"]
        include("$(file)/$(file).jl")
    end

    @testset "Copying options" begin
        models = [
            MathOptFormat.CBF.Model,
            MathOptFormat.LP.Model,
            MathOptFormat.MOF.Model,
            MathOptFormat.MPS.Model
        ]
        for src in models
            model_src = src()
            for dest in models
                model_dest = dest()
                MOI.copy_to(model_dest, model_src)
                @test !isempty(sprint(io -> MOI.write_to_file(model_dest, io)))
            end
            model_dest = MOIU.MockOptimizer(MOIU.Model{Float64}())
            MOI.copy_to(model_dest, model_src)
        end
    end

    @testset "Calling MOF._compressed_open" begin
        for cs in [MathOptFormat.Bzip2(), MathOptFormat.Gzip()]
            for open_type in ["a", "r+", "w+", "a+"]
                @test_throws ArgumentError MathOptFormat._compressed_open(
                    (x) -> nothing, "dummy.gz", open_type, cs
                )
            end
        end
    end

    @testset "Provided compression schemes" begin
        model = MathOptFormat.read_from_file(
            joinpath(@__DIR__, "MPS", "free_integer.mps")
        )
        filename = joinpath(@__DIR__, "free_integer.mps")
        MOI.write_to_file(model, filename * ".garbage")
        for ext in ["", ".bz2", ".gz"]
            MOI.write_to_file(model, filename * ext)
            MathOptFormat.read_from_file(filename * ext)
        end

        sleep(1.0)  # Allow time for unlink to happen.
        for ext in ["", ".garbage", ".bz2", ".gz"]
            rm(filename * ext, force = true)
        end
    end
end
