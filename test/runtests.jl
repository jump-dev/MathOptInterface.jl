using MathOptFormat, Test

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
        for cs in [MathOptFormat.Bzip2(), MathOptFormat.Gzip()] # MathOptFormat.Xz()
            @test_throws ArgumentError MathOptFormat._compressed_open((x) -> nothing,
                                                          "dummy.gz", "a", cs)
            @test_throws ArgumentError MathOptFormat._compressed_open((x) -> nothing,
                                                          "dummy.gz", "r+", cs)
            @test_throws ArgumentError MathOptFormat._compressed_open((x) -> nothing,
                                                          "dummy.gz", "w+", cs)
            @test_throws ArgumentError MathOptFormat._compressed_open((x) -> nothing,
                                                          "dummy.gz", "a+", cs)
        end
    end

    @testset "Provided compression schemes" begin
        file_to_read = joinpath(@__DIR__, "MPS", "free_integer.mps")
        m = MathOptFormat.read_from_file(file_to_read)

        @testset "Automatic detection from extension" begin
            MOI.write_to_file(m, file_to_read * ".garbage")
            for ext in ["", ".bz2", ".gz"] # ".xz"
                MOI.write_to_file(m, file_to_read * ext)
                MathOptFormat.read_from_file(file_to_read * ext)
            end

            # Clean up
            sleep(1.0)  # Allow time for unlink to happen.
            for ext in ["", ".garbage", ".bz2", ".gz"] # ".xz"
                rm(file_to_read * ext, force = true)
            end
        end
    end
end
