# We need to test this in a module at the top level because it can't be defined
# in a testset. If it runs without error, then we're okay.
module TestExternalBridge
    using MathOptInterface

    struct StrictlyGreaterThan <: MathOptInterface.AbstractScalarSet end
    struct StrictlyGreaterBridge{T} <: MathOptInterface.Bridges.AbstractBridge end

    function StrictlyGreaterBridge(
            model,
            func::MathOptInterface.SingleVariable,
            set::StrictlyGreaterThan)
        return StrictlyGreaterBridge{Float64}()
    end

    function MathOptInterface.supports_constraint(
            ::Type{StrictlyGreaterBridge{T}},
            ::Type{MathOptInterface.SingleVariable},
            ::Type{StrictlyGreaterThan}) where {T}
        return true
    end

    function MathOptInterface.Bridges.added_constraint_types(
            ::Type{StrictlyGreaterBridge{T}},
            ::Type{MathOptInterface.SingleVariable},
            ::Type{StrictlyGreaterThan}) where {T}
        return [(
            MathOptInterface.SingleVariable,
            MathOptInterface.GreaterThan{T}
        )]
    end

    MathOptInterface.Bridges.@bridge(StrictlyGreater, StrictlyGreaterBridge,
        (StrictlyGreaterThan, ),
        (),
        (),
        (),
        (MathOptInterface.SingleVariable, ),
        (),
        (),
        ()
    )
end

@testset "@bridge with external components" begin
    model = SimpleModel{Float64}();
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.GreaterThan{Float64})
    @test !MOI.supports_constraint(model, MOI.SingleVariable, TestExternalBridge.StrictlyGreaterThan)

    bridge = TestExternalBridge.StrictlyGreater{Float64}(model);
    @test MOI.supports_constraint(bridge, MOI.SingleVariable, MOI.GreaterThan{Float64})
    @test MOI.supports_constraint(bridge, MOI.SingleVariable, TestExternalBridge.StrictlyGreaterThan)
end
