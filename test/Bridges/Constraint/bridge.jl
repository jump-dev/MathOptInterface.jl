using Test

using MathOptInterface
const MOI = MathOptInterface
const MOIT = MathOptInterface.Test
const MOIU = MathOptInterface.Utilities
const MOIB = MathOptInterface.Bridges

struct DummyBridge <: MOIB.Constraint.AbstractBridge end

@testset "AbstractBridge" begin
    model = MOIU.Model{Float64}()
    bridge = DummyBridge()
    attr = MOI.ConstraintPrimalStart()
    @test !MOI.supports(model, attr, typeof(bridge))
    @test_throws MOI.UnsupportedAttribute(attr) MOI.set(
        model,
        attr,
        bridge,
        1.0,
    )
    attr = MOI.ConstraintFunction()
    err = MOI.SetAttributeNotAllowed(attr)
    @test_throws err MOI.set(model, attr, bridge, MOI.EqualTo(1.0))
end
