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
                @test !isempty(sprint(write, model_dest))
            end
            model_dest = MOIU.MockOptimizer(MOIU.Model{Float64}())
            MOI.copy_to(model_dest, model_src)
        end
    end

    @testset "Calling MOF.compressed_open" begin
        for cs in [MathOptFormat.Bzip2(), MathOptFormat.Gzip()]
            for open_type in ["a", "r+", "w+", "a+"]
                @test_throws ArgumentError MathOptFormat.compressed_open(
                    (x) -> nothing, "dummy.gz", open_type, cs
                )
            end
        end
    end

    @testset "Provided compression schemes" begin
        filename = joinpath(@__DIR__, "MPS", "free_integer.mps")
        model = MathOptFormat.Model(filename = filename)
        MOI.read_from_file(model, filename)
        filename = joinpath(@__DIR__, "free_integer.mps")
        MOI.write_to_file(model, filename * ".garbage")
        for ext in ["", ".bz2", ".gz"]
            MOI.write_to_file(model, filename * ext)
            model2 = MathOptFormat.Model(filename = filename * ext)
            MOI.read_from_file(model2, filename)
        end

        sleep(1.0)  # Allow time for unlink to happen.
        for ext in ["", ".garbage", ".bz2", ".gz"]
            rm(filename * ext, force = true)
        end
    end

    @testset "Model" begin
        for (format, model) in [
            (MathOptFormat.FORMAT_CBF, MathOptFormat.CBF.Model()),
            (MathOptFormat.FORMAT_LP, MathOptFormat.LP.Model()),
            (MathOptFormat.FORMAT_MOF, MathOptFormat.MOF.Model()),
            (MathOptFormat.FORMAT_MPS, MathOptFormat.MPS.Model()),
        ]
            @test typeof(
                MathOptFormat.Model(format = format, filename = "foo.bar")
            ) == typeof(model)
        end
        @test_throws(
            ErrorException(
                "When `format==FORMAT_AUTOMATIC` you must pass a `filename`."
            ),
            MathOptFormat.Model(format = MathOptFormat.FORMAT_AUTOMATIC)
        )
        for (ext, model) in [
            (".cbf", MathOptFormat.CBF.Model()),
            (".lp", MathOptFormat.LP.Model()),
            (".mof.json", MathOptFormat.MOF.Model()),
            (".mps", MathOptFormat.MPS.Model()),
        ]
            @test typeof(MathOptFormat.Model(filename = "a$(ext)")) ==
                typeof(model)
            @test typeof(MathOptFormat.Model(filename = "a$(ext).gz")) ==
                typeof(model)
        end
        @test_throws(
            ErrorException("Unable to automatically detect format of a.b."),
            MathOptFormat.Model(filename = "a.b")
        )
    end
end
