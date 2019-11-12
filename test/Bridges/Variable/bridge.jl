using Test

using MathOptInterface
const MOI = MathOptInterface
const MOIT = MathOptInterface.Test
const MOIU = MathOptInterface.Utilities
const MOIB = MathOptInterface.Bridges

struct VariableDummyBridge <: MOIB.Variable.AbstractBridge end

@testset "AbstractBridge" begin
    model = MOIU.Model{Float64}()
    bridge = VariableDummyBridge()
    attr = MOI.VariablePrimalStart()
    @test !MOI.supports(model, attr, typeof(bridge))
    i = MOIB.Variable.IndexInVector(1)
    @test_throws MOI.UnsupportedAttribute(attr) MOI.set(model, attr, bridge, 1.0)
    @test_throws MOI.UnsupportedAttribute(attr) MOI.set(model, attr, bridge, 1.0, i)
    attr = MOI.VariablePrimal()
    err = MOI.SetAttributeNotAllowed(attr)
    @test_throws err MOI.set(model, attr, bridge, 1.0)
    @test_throws err MOI.set(model, attr, bridge, 1.0, i)
end
