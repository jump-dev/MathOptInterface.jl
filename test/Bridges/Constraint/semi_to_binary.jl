using Test

using MathOptInterface
const MOI = MathOptInterface
const MOIT = MathOptInterface.Test
const MOIU = MathOptInterface.Utilities
const MOIB = MathOptInterface.Bridges

include("../utilities.jl")

mock = MOIU.MockOptimizer(MOIU.UniversalFallback(MOIU.Model{Float64}()))
config = MOIT.TestConfig()

@testset "SemiToBinary" begin
    bridged_mock = MOIB.Constraint.SemiToBinary{Float64}(mock)

    MOIT.basic_constraint_tests(
        bridged_mock, config,
        include = [(F, S)
                   for F in [MOI.SingleVariable]
                   for S in [MOI.Semiinteger{Float64}, MOI.Semicontinuous{Float64}]])

        MOIU.set_mock_optimize!(mock,
        (mock::MOIU.MockOptimizer) -> begin
            MOI.set(mock, MOI.ObjectiveBound(), 0.0)
            MOIU.mock_optimize!(mock, [0.0, 0.0, 0.0])
        end,
        (mock::MOIU.MockOptimizer) -> begin
            MOI.set(mock, MOI.ObjectiveBound(), 2.0)
            MOIU.mock_optimize!(mock, [2.0, 1.0, 1.0])
        end,
        (mock::MOIU.MockOptimizer) -> begin
            MOI.set(mock, MOI.ObjectiveBound(), 2.0)
            MOIU.mock_optimize!(mock, [2.0, 2.0, 1.0])
        end,
        (mock::MOIU.MockOptimizer) -> begin
            MOI.set(mock, MOI.ObjectiveBound(), 2.5)
            MOIU.mock_optimize!(mock, [2.5, 2.5, 1.0])
        end,
        (mock::MOIU.MockOptimizer) -> begin
            MOI.set(mock, MOI.ObjectiveBound(), 3.0)
            MOIU.mock_optimize!(mock, [3.0, 3.0, 1.0])
        end,
        (mock::MOIU.MockOptimizer) -> MOI.set(mock, MOI.TerminationStatus(), MOI.INFEASIBLE)
    )
    MOIT.semiconttest(bridged_mock,config)

    ci = first(MOI.get(
        bridged_mock,
        MOI.ListOfConstraintIndices{MOI.SingleVariable,
                                    MOI.Semicontinuous{Float64}}()))

    test_delete_bridge(bridged_mock, ci, 2, (
        (MOI.SingleVariable, MOI.EqualTo{Float64}, 1),
        (MOI.SingleVariable, MOI.ZeroOne, 0),
        (MOI.SingleVariable, MOI.Integer, 0),
        (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}, 0),
        (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}, 1),
        ))

    MOIU.set_mock_optimize!(mock,
        (mock::MOIU.MockOptimizer) -> begin
            MOI.set(mock, MOI.ObjectiveBound(), 0.0)
            MOIU.mock_optimize!(mock, [0.0, 0.0, 0.0])
        end,
        (mock::MOIU.MockOptimizer) -> begin
            MOI.set(mock, MOI.ObjectiveBound(), 2.0)
            MOIU.mock_optimize!(mock, [2.0, 1.0, 1.0])
        end,
        (mock::MOIU.MockOptimizer) -> begin
            MOI.set(mock, MOI.ObjectiveBound(), 2.0)
            MOIU.mock_optimize!(mock, [2.0, 2.0, 1.0])
        end,
        (mock::MOIU.MockOptimizer) -> begin
            MOI.set(mock, MOI.ObjectiveBound(), 3.0)
            MOIU.mock_optimize!(mock, [3.0, 2.5, 1.0])
        end,
        (mock::MOIU.MockOptimizer) -> begin
            MOI.set(mock, MOI.ObjectiveBound(), 3.0)
            MOIU.mock_optimize!(mock, [3.0, 3.0, 1.0])
        end,
        (mock::MOIU.MockOptimizer) -> MOI.set(mock, MOI.TerminationStatus(), MOI.INFEASIBLE)
    )
    MOIT.semiinttest(bridged_mock,config)

    ci = first(MOI.get(
        bridged_mock,
        MOI.ListOfConstraintIndices{MOI.SingleVariable,
                                    MOI.Semiinteger{Float64}}()))

    @testset "$attr" for attr in [MOI.ConstraintPrimalStart(),]
        @test MOI.supports(bridged_mock, attr, typeof(ci))
        value = 2.0
        MOI.set(bridged_mock, attr, ci, value)
        @test MOI.get(bridged_mock, attr, ci) ≈ value
    end

    test_delete_bridge(bridged_mock, ci, 2, (
        (MOI.SingleVariable, MOI.EqualTo{Float64}, 1),
        (MOI.SingleVariable, MOI.ZeroOne, 0),
        (MOI.SingleVariable, MOI.Integer, 0),
        (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}, 0),
        (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}, 1),
        ))
end
