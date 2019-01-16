using MathOptFormat, Compat.Test

const MOI = MathOptFormat.MOI
const MOIU = MathOptFormat.MOIU

@testset "MathOptFormat tests" begin
    @testset "MOF" begin
        include("MOF/MOF.jl")
    end
    @testset "MPS" begin
        include("MPS/MPS.jl")
    end
    @testset "CBF" begin
        include("CBF/CBF.jl")
    end
end
