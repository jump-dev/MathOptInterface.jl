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
        end
    end
end
