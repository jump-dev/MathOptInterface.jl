using MathOptFormat, Compat.Test

const MOI = MathOptFormat.MOI
const MOIU = MOI.Utilities

@testset "MathOptFormat tests" begin
    @testset "$(file)" for file in ["CBF", "LP", "MOF", "MPS"]
        include("$(file)/$(file).jl")
    end
end
