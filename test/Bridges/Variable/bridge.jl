module TestVariableBridge

using Test

using MathOptInterface
const MOI = MathOptInterface

function runtests()
    for name in names(@__MODULE__; all = true)
        if startswith("$(name)", "test_")
            @testset "$(name)" begin
                getfield(@__MODULE__, name)()
            end
        end
    end
    return
end

struct DummyVariableBridge <: MOI.Bridges.Variable.AbstractBridge end

function test_AbstractBridge()
    model = MOI.Utilities.Model{Float64}()
    bridge = DummyVariableBridge()
    attr = MOI.VariablePrimalStart()
    @test !MOI.supports(model, attr, typeof(bridge))
    i = MOI.Bridges.IndexInVector(1)
    @test_throws MOI.UnsupportedAttribute(attr) MOI.set(
        model,
        attr,
        bridge,
        1.0,
    )
    @test_throws MOI.UnsupportedAttribute(attr) MOI.set(
        model,
        attr,
        bridge,
        1.0,
        i,
    )
    attr = MOI.VariablePrimal()
    err = MOI.SetAttributeNotAllowed(attr)
    @test_throws err MOI.set(model, attr, bridge, 1.0)
    @test_throws err MOI.set(model, attr, bridge, 1.0, i)
    return
end

end  # module

TestVariableBridge.runtests()
