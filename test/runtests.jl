using MathOptFormat, Compat.Test

const MOI = MathOptFormat.MOI
const MOIU = MathOptFormat.MOIU

@testset "MOF" begin
    include("MOF/MOF.jl")
end

@testset "MPS" begin
    include("MPS/MPS.jl")
end
