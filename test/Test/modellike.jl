using Test
import MathOptInterface
const MOI = MathOptInterface
const MOIT = MOI.Test

@testset "Failed copy" begin
    include("../dummy.jl")
    model = DummyModelWithAdd()
    MOIT.failcopytestc(model)
    MOIT.failcopytestia(model)
    MOIT.failcopytestva(model)
    MOIT.failcopytestca(model)
end
