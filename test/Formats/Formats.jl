using MathOptInterface, Test
const MOI = MathOptInterface

@testset "MOI.Formats tests" begin
    @testset "$(file)" for file in ["CBF", "LP", "MOF", "MPS"]
        include(joinpath(@__DIR__, file, "$(file).jl"))
    end

    @testset "Copying options" begin
        models = [
            MOI.Formats.CBF.Model,
            MOI.Formats.LP.Model,
            MOI.Formats.MOF.Model,
            MOI.Formats.MPS.Model
        ]
        for src in models
            model_src = src()
            for dest in models
                model_dest = dest()
                MOI.copy_to(model_dest, model_src)
                @test !isempty(sprint(io -> MOI.write_to_file(model_dest, io)))
            end
            model_dest = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
            MOI.copy_to(model_dest, model_src)
        end
    end
end
