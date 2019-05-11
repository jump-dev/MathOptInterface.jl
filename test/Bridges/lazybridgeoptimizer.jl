using Test

using MathOptInterface
const MOI = MathOptInterface
const MOIT = MathOptInterface.Test
const MOIU = MathOptInterface.Utilities
const MOIB = MathOptInterface.Bridges

include("utilities.jl")

# Model not supporting RotatedSecondOrderCone
MOIU.@model(NoRSOCModel,
            (),
            (MOI.EqualTo, MOI.GreaterThan, MOI.LessThan),
            (MOI.Zeros, MOI.Nonnegatives, MOI.Nonpositives, MOI.SecondOrderCone,
             MOI.ExponentialCone, MOI.PositiveSemidefiniteConeTriangle),
            (),
            (),
            (MOI.ScalarAffineFunction, MOI.ScalarQuadraticFunction),
            (MOI.VectorOfVariables,),
            (MOI.VectorAffineFunction, MOI.VectorQuadraticFunction))

# Model not supporting VectorOfVariables and SingleVariable
MOIU.@model(NoVariableModel,
            (MOI.ZeroOne, MOI.Integer),
            (MOI.EqualTo, MOI.GreaterThan, MOI.LessThan),
            (MOI.Zeros, MOI.Nonnegatives, MOI.Nonpositives, MOI.SecondOrderCone),
            (),
            (),
            (MOI.ScalarAffineFunction,),
            (),
            (MOI.VectorAffineFunction,))
function MOI.supports_constraint(::NoVariableModel, ::Type{MOI.SingleVariable},
                                 ::Type{<:MOI.AbstractScalarSet})
    return false
end

MOIU.@model(GreaterNonnegModel,
            (),
            (MOI.GreaterThan,),
            (MOI.Nonnegatives,),
            (),
            (),
            (MOI.ScalarAffineFunction, MOI.ScalarQuadraticFunction),
            (MOI.VectorOfVariables,),
            (MOI.VectorAffineFunction, MOI.VectorQuadraticFunction))


MOIU.@model(ModelNoVAFinSOC,
            (),
            (MOI.EqualTo, MOI.GreaterThan, MOI.LessThan, MOI.Interval),
            (MOI.Zeros, MOI.Nonnegatives, MOI.Nonpositives, MOI.SecondOrderCone,
             MOI.RotatedSecondOrderCone, MOI.GeometricMeanCone,
             MOI.PositiveSemidefiniteConeTriangle, MOI.ExponentialCone),
            (MOI.PowerCone, MOI.DualPowerCone),
            (),
            (MOI.ScalarAffineFunction, MOI.ScalarQuadraticFunction),
            (MOI.VectorOfVariables,),
            (MOI.VectorAffineFunction, MOI.VectorQuadraticFunction))

MOI.supports_constraint(::ModelNoVAFinSOC{Float64},
                        ::Type{MOI.VectorAffineFunction{Float64}},
                        ::Type{MOI.SecondOrderCone}) = false

# Model supporting nothing
MOIU.@model NothingModel () () () () () () () ()

struct BridgeAddingNoConstraint{T} <: MOI.Bridges.AbstractBridge end
MOIB.added_constraint_types(::Type{BridgeAddingNoConstraint{T}}) where {T} = []
function MOI.supports_constraint(::Type{<:BridgeAddingNoConstraint},
                                 ::Type{MOI.SingleVariable},
                                 ::Type{MOI.Integer})
    return true
end
function MOIB.concrete_bridge_type(::Type{<:BridgeAddingNoConstraint{T}},
                                   ::Type{MOI.SingleVariable},
                                   ::Type{MOI.Integer}) where {T}
    return BridgeAddingNoConstraint{T}
end

@testset "Bridge adding no constraint" begin
    mock = MOIU.MockOptimizer(NothingModel{Int}())
    bridged = MOIB.LazyBridgeOptimizer(mock)
    MOI.Bridges.add_bridge(bridged, BridgeAddingNoConstraint{Float64})
    @test MOI.Bridges.supports_bridging_constraint(bridged,
                                                   MOI.SingleVariable,
                                                   MOI.Integer)
end

@testset "Unsupported constraint with cycles" begin
    # Test that `supports_constraint` works correctly when it is not
    # supported but the bridges forms a cycle
    mock = MOIU.MockOptimizer(NothingModel{Float64}())
    bridged = MOIB.full_bridge_optimizer(mock, Float64)
    @test !MOI.supports_constraint(
        bridged, MOI.SingleVariable, MOI.GreaterThan{Float64})
    @test !MOI.supports_constraint(
        bridged, MOI.VectorAffineFunction{Float64}, MOI.Nonpositives)
end

mock = MOIU.MockOptimizer(NoRSOCModel{Float64}())
bridged_mock = MOIB.LazyBridgeOptimizer(mock)

@testset "UnsupportedConstraint when it cannot be bridged" begin
    x = MOI.add_variables(bridged_mock, 4)
    err = MOI.UnsupportedConstraint{MOI.VectorOfVariables,
                                    MOI.RotatedSecondOrderCone}()
    @test_throws err begin
        MOI.add_constraint(bridged_mock, MOI.VectorOfVariables(x),
                           MOI.RotatedSecondOrderCone(4))
    end
end

MOIB.add_bridge(bridged_mock, MOIB.SplitIntervalBridge{Float64})
MOIB.add_bridge(bridged_mock, MOIB.RSOCtoPSDBridge{Float64})
MOIB.add_bridge(bridged_mock, MOIB.SOCtoPSDBridge{Float64})
MOIB.add_bridge(bridged_mock, MOIB.RSOCBridge{Float64})

@testset "Name test" begin
    MOIT.nametest(bridged_mock)
end

@testset "Copy test" begin
    MOIT.failcopytestc(bridged_mock)
    MOIT.failcopytestia(bridged_mock)
    MOIT.failcopytestva(bridged_mock)
    MOIT.failcopytestca(bridged_mock)
    MOIT.copytest(bridged_mock, NoRSOCModel{Float64}())
end

# Test that RSOCtoPSD is used instead of RSOC+SOCtoPSD as it is a shortest path.
@testset "Bridge selection" begin
    MOI.empty!(bridged_mock)
    @test !(MOI.supports_constraint(bridged_mock,
                                    MOI.VectorAffineFunction{Float64},
                                    MOI.LogDetConeTriangle))
    x = MOI.add_variables(bridged_mock, 3)
    err = MOI.UnsupportedConstraint{MOI.VectorAffineFunction{Float64},
                                    MOI.LogDetConeTriangle}()
    @test_throws err begin
        MOIB.bridge_type(bridged_mock, MOI.VectorAffineFunction{Float64},
                         MOI.LogDetConeTriangle)
    end
    c = MOI.add_constraint(bridged_mock, MOI.VectorOfVariables(x),
                           MOI.RotatedSecondOrderCone(3))
    @test MOIB.bridge_type(bridged_mock, MOI.VectorOfVariables,
                MOI.RotatedSecondOrderCone) == MOIB.RSOCtoPSDBridge{Float64}
    @test MOIB.bridge(bridged_mock, c) isa MOIB.RSOCtoPSDBridge
    @test bridged_mock.dist[(MOI.VectorOfVariables,
                            MOI.RotatedSecondOrderCone)] == 1
end

@testset "Supports" begin
    full_bridged_mock = MOIB.full_bridge_optimizer(mock, Float64)
    greater_nonneg_mock = MOIU.MockOptimizer(GreaterNonnegModel{Float64}())
    full_bridged_greater_nonneg = MOIB.full_bridge_optimizer(
        greater_nonneg_mock, Float64)
    for F in [MOI.SingleVariable, MOI.ScalarAffineFunction{Float64},
              MOI.ScalarQuadraticFunction{Float64}]
        @test MOI.supports_constraint(full_bridged_mock, F,
                                      MOI.Interval{Float64})
        @test !MOI.supports_constraint(
            greater_nonneg_mock, F, MOI.LessThan{Float64})
        @test MOI.supports_constraint(
            full_bridged_greater_nonneg, F, MOI.LessThan{Float64})
    end
    for F in [MOI.VectorOfVariables, MOI.VectorAffineFunction{Float64},
              MOI.VectorQuadraticFunction{Float64}]
        @test MOI.supports_constraint(full_bridged_mock, F,
                                      MOI.PositiveSemidefiniteConeSquare)
        @test MOI.supports_constraint(full_bridged_mock, F,
                                      MOI.GeometricMeanCone)
        @test !MOI.supports_constraint(
            greater_nonneg_mock, F, MOI.Nonpositives)
        @test MOI.supports_constraint(
            full_bridged_greater_nonneg, F, MOI.Nonnegatives)
    end
    for F in [MOI.VectorOfVariables, MOI.VectorAffineFunction{Float64}]
        # The bridges in this for loop do not support yet
        # VectorQuadraticFunction. See TODO's for the reason.
        # TODO: Missing vcat for quadratic for supporting quadratic.
        @test MOI.supports_constraint(full_bridged_mock, F,
                                      MOI.RotatedSecondOrderCone)
        # TODO: Det bridges need to use MOIU.operate to support quadratic.
        @test MOI.supports_constraint(full_bridged_mock, F,
                                      MOI.LogDetConeTriangle)
        @test MOI.supports_constraint(full_bridged_mock, F,
                                      MOI.RootDetConeTriangle)
    end
    mock2 = MOIU.MockOptimizer(ModelNoVAFinSOC{Float64}())
    @test !MOI.supports_constraint(mock2, MOI.VectorAffineFunction{Float64},
                                   MOI.SecondOrderCone)
    full_bridged_mock2 = MOIB.full_bridge_optimizer(mock2, Float64)
    @test MOI.supports_constraint(full_bridged_mock2, MOI.VectorAffineFunction{Float64},
                                  MOI.SecondOrderCone)
    @testset "Unslack" begin
        for T in [Float64, Int]
            no_variable_mock = MOIU.MockOptimizer(NoVariableModel{T}())
            full_bridged_no_variable = MOIB.full_bridge_optimizer(
                no_variable_mock, T)
            for S in [MOI.LessThan{T}, MOI.GreaterThan{T}, MOI.EqualTo{T},
                      MOI.ZeroOne, MOI.Integer]
                @test MOI.supports_constraint(
                    full_bridged_no_variable, MOI.SingleVariable, S)
            end
            for S in [MOI.Nonpositives, MOI.Nonnegatives,
                      MOI.Zeros, MOI.SecondOrderCone]
                @test MOI.supports_constraint(
                    full_bridged_no_variable, MOI.VectorOfVariables, S)
            end
        end
    end
end

@testset "Combining two briges" begin
    full_bridged_mock = MOIB.full_bridge_optimizer(mock, Float64)
    mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1, 1, 0, 1, 1, 0, 1, √2])
    config = MOIT.TestConfig()
    MOIT.rootdett1vtest(full_bridged_mock, config)
    MOIT.rootdett1ftest(full_bridged_mock, config)
    # Dual is not yet implemented for RootDet and GeoMean bridges
    ci = first(MOI.get(full_bridged_mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.RootDetConeTriangle}()))
    test_delete_bridge(full_bridged_mock, ci, 4, ((MOI.VectorAffineFunction{Float64}, MOI.RotatedSecondOrderCone, 0),
                                                (MOI.VectorAffineFunction{Float64}, MOI.GeometricMeanCone, 0),
                                                (MOI.VectorAffineFunction{Float64}, MOI.PositiveSemidefiniteConeTriangle, 0)),
                       used_bridges = 3)
end

@testset "Continuous Linear" begin
    exclude = ["partial_start"] # VariablePrimalStart not supported.
    MOIT.contlineartest(bridged_mock, MOIT.TestConfig(solve=false), exclude)
end

@testset "Continuous Conic" begin
    MOIT.contconictest(MOIB.full_bridge_optimizer(mock, Float64), MOIT.TestConfig(solve=false), ["logdets", "rootdets", "psds"])
end
