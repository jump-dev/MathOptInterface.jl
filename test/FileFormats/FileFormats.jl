using MathOptInterface
using Test

const MOI = MathOptInterface

@testset "MOI.FileFormats tests" begin
    @testset "$(file)" for file in ["CBF", "LP", "MOF", "MPS", "SDPA"]
        include(joinpath(@__DIR__, file, "$(file).jl"))
    end

    @testset "Copying options" begin
        models = [
            MOI.FileFormats.CBF.Model,
            MOI.FileFormats.LP.Model,
            MOI.FileFormats.MOF.Model,
            MOI.FileFormats.MPS.Model,
            MOI.FileFormats.SDPA.Model,
        ]
        for src in models
            model_src = src()
            for dest in models
                model_dest = dest()
                MOI.copy_to(model_dest, model_src)
                @test !isempty(sprint(write, model_dest))
            end
            model_dest =
                MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
            MOI.copy_to(model_dest, model_src)
        end
    end

    @testset "Calling MOF.compressed_open" begin
        for cs in [MOI.FileFormats.Bzip2(), MOI.FileFormats.Gzip()]
            for open_type in ["a", "r+", "w+", "a+"]
                @test_throws ArgumentError MOI.FileFormats.compressed_open(
                    (x) -> nothing,
                    "dummy.gz",
                    open_type,
                    cs,
                )
            end
        end
    end

    @testset "Provided compression schemes" begin
        filename = joinpath(@__DIR__, "MPS", "free_integer.mps")
        model = MOI.FileFormats.Model(filename = filename)
        MOI.read_from_file(model, filename)
        filename = joinpath(@__DIR__, "free_integer.mps")
        MOI.write_to_file(model, filename * ".garbage")
        for ext in ["", ".bz2", ".gz"]
            MOI.write_to_file(model, filename * ext)
            model2 = MOI.FileFormats.Model(filename = filename * ext)
            MOI.read_from_file(model2, filename)
        end

        sleep(1.0)  # Allow time for unlink to happen.
        for ext in ["", ".garbage", ".bz2", ".gz"]
            rm(filename * ext, force = true)
        end
    end

    @testset "Model" begin
        for (format, model) in [
            (MOI.FileFormats.FORMAT_CBF, MOI.FileFormats.CBF.Model()),
            (MOI.FileFormats.FORMAT_LP, MOI.FileFormats.LP.Model()),
            (MOI.FileFormats.FORMAT_MOF, MOI.FileFormats.MOF.Model()),
            (MOI.FileFormats.FORMAT_MPS, MOI.FileFormats.MPS.Model()),
            (MOI.FileFormats.FORMAT_SDPA, MOI.FileFormats.SDPA.Model()),
        ]
            @test typeof(
                MOI.FileFormats.Model(format = format, filename = "foo.bar"),
            ) == typeof(model)
        end
        @test_throws(
            ErrorException(
                "When `format==FORMAT_AUTOMATIC` you must pass a `filename`.",
            ),
            MOI.FileFormats.Model(format = MOI.FileFormats.FORMAT_AUTOMATIC)
        )
        for (ext, model) in [
            (".cbf", MOI.FileFormats.CBF.Model()),
            (".lp", MOI.FileFormats.LP.Model()),
            (".mof.json", MOI.FileFormats.MOF.Model()),
            (".mps", MOI.FileFormats.MPS.Model()),
            (".sdpa", MOI.FileFormats.SDPA.Model()),
        ]
            @test typeof(MOI.FileFormats.Model(filename = "a$(ext)")) ==
                  typeof(model)
            @test typeof(MOI.FileFormats.Model(filename = "a$(ext).gz")) ==
                  typeof(model)
        end
        @test_throws(
            ErrorException("Unable to automatically detect format of a.b."),
            MOI.FileFormats.Model(filename = "a.b")
        )
    end
end
