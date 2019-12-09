using MathOptInterface
using Test

const MOI = MathOptInterface

@testset "MOI.Formats tests" begin
    @testset "$(file)" for file in ["CBF", "LP", "MOF", "MPS", "SDPA"]
        include(joinpath(@__DIR__, file, "$(file).jl"))
    end

    @testset "Copying options" begin
        models = [
            MOI.Formats.CBF.Model,
            MOI.Formats.LP.Model,
            MOI.Formats.MOF.Model,
            MOI.Formats.MPS.Model,
            MOI.Formats.SDPA.Model,
        ]
        for src in models
            model_src = src()
            for dest in models
                model_dest = dest()
                MOI.copy_to(model_dest, model_src)
                @test !isempty(sprint(write, model_dest))
            end
            model_dest = MOI.Utilities.MockOptimizer(
                MOI.Utilities.Model{Float64}()
            )
            MOI.copy_to(model_dest, model_src)
        end
    end

    @testset "Calling MOF.compressed_open" begin
        for cs in [MOI.Formats.Bzip2(), MOI.Formats.Gzip()]
            for open_type in ["a", "r+", "w+", "a+"]
                @test_throws ArgumentError MOI.Formats.compressed_open(
                    (x) -> nothing, "dummy.gz", open_type, cs
                )
            end
        end
    end

    @testset "Provided compression schemes" begin
        filename = joinpath(@__DIR__, "MPS", "free_integer.mps")
        model = MOI.Formats.Model(filename = filename)
        MOI.read_from_file(model, filename)
        filename = joinpath(@__DIR__, "free_integer.mps")
        MOI.write_to_file(model, filename * ".garbage")
        for ext in ["", ".bz2", ".gz"]
            MOI.write_to_file(model, filename * ext)
            model2 = MOI.Formats.Model(filename = filename * ext)
            MOI.read_from_file(model2, filename)
        end

        sleep(1.0)  # Allow time for unlink to happen.
        for ext in ["", ".garbage", ".bz2", ".gz"]
            rm(filename * ext, force = true)
        end
    end

    @testset "Model" begin
        for (format, model) in [
            (MOI.Formats.FORMAT_CBF, MOI.Formats.CBF.Model()),
            (MOI.Formats.FORMAT_LP, MOI.Formats.LP.Model()),
            (MOI.Formats.FORMAT_MOF, MOI.Formats.MOF.Model()),
            (MOI.Formats.FORMAT_MPS, MOI.Formats.MPS.Model()),
            (MOI.Formats.FORMAT_SDPA, MOI.Formats.SDPA.Model()),
        ]
            @test typeof(
                MOI.Formats.Model(format = format, filename = "foo.bar")
            ) == typeof(model)
        end
        @test_throws(
            ErrorException(
                "When `format==FORMAT_AUTOMATIC` you must pass a `filename`."
            ),
            MOI.Formats.Model(format = MOI.Formats.FORMAT_AUTOMATIC)
        )
        for (ext, model) in [
            (".cbf", MOI.Formats.CBF.Model()),
            (".lp", MOI.Formats.LP.Model()),
            (".mof.json", MOI.Formats.MOF.Model()),
            (".mps", MOI.Formats.MPS.Model()),
            (".sdpa", MOI.Formats.SDPA.Model()),
        ]
            @test typeof(MOI.Formats.Model(filename = "a$(ext)")) ==
                typeof(model)
            @test typeof(MOI.Formats.Model(filename = "a$(ext).gz")) ==
                typeof(model)
        end
        @test_throws(
            ErrorException("Unable to automatically detect format of a.b."),
            MOI.Formats.Model(filename = "a.b")
        )
    end
end
