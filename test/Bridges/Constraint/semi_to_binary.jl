using Test

using MathOptInterface
const MOI = MathOptInterface
const MOIT = MathOptInterface.Test
const MOIU = MathOptInterface.Utilities
const MOIB = MathOptInterface.Bridges
const MOIBC = MathOptInterface.Bridges.Constraint

include("../utilities.jl")

mock = MOIU.MockOptimizer(MOIU.UniversalFallback(MOIU.Model{Float64}()))
config = MOIT.Config()

@testset "SemiToBinary" begin
    bridged_mock = MOIBC.SemiToBinary{Float64}(mock)

    bridge_type = MOIBC.SemiToBinaryBridge{Float64,MOI.Semiinteger{Float64}}
    @test MOI.supports_constraint(
        bridge_type,
        MOI.SingleVariable,
        MOI.Semiinteger{Float64},
    )
    @test MOIBC.concrete_bridge_type(
        bridge_type,
        MOI.SingleVariable,
        MOI.Semiinteger{Float64},
    ) == bridge_type

    @test MOI.supports(bridged_mock, MOI.ConstraintPrimalStart(), bridge_type)

    MOIT.basic_constraint_tests(
        bridged_mock,
        config,
        include = [
            (F, S) for F in [MOI.SingleVariable] for
            S in [MOI.Semiinteger{Float64}, MOI.Semicontinuous{Float64}]
        ],
    )

    MOIU.set_mock_optimize!(
        mock,
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
        (mock::MOIU.MockOptimizer) ->
            MOI.set(mock, MOI.TerminationStatus(), MOI.INFEASIBLE),
    )
    MOIT.test_Semicontinuous_integration(bridged_mock, config)

    ci = first(
        MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{
                MOI.SingleVariable,
                MOI.Semicontinuous{Float64},
            }(),
        ),
    )

    test_delete_bridge(
        bridged_mock,
        ci,
        2,
        (
            (MOI.SingleVariable, MOI.EqualTo{Float64}, 1),
            (MOI.SingleVariable, MOI.ZeroOne, 0),
            (MOI.SingleVariable, MOI.Integer, 0),
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}, 0),
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}, 1),
        ),
    )

    MOIU.set_mock_optimize!(
        mock,
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
        (mock::MOIU.MockOptimizer) ->
            MOI.set(mock, MOI.TerminationStatus(), MOI.INFEASIBLE),
    )
    MOIT.test_Semiinteger_integration(bridged_mock, config)

    ci = first(
        MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{
                MOI.SingleVariable,
                MOI.Semiinteger{Float64},
            }(),
        ),
    )

    @test MOI.get(bridged_mock, MOI.ConstraintPrimal(), ci) == 3
    new_set = MOI.Semiinteger{Float64}(19.0, 20.0)
    MOI.set(bridged_mock, MOI.ConstraintSet(), ci, new_set)
    @test MOI.get(bridged_mock, MOI.ConstraintSet(), ci) == new_set

    @testset "$attr" for attr in [MOI.ConstraintPrimalStart()]
        @test MOI.supports(bridged_mock, attr, typeof(ci))
        value = 2.0
        MOI.set(bridged_mock, attr, ci, value)
        @test MOI.get(bridged_mock, attr, ci) ≈ value
    end

    test_delete_bridge(
        bridged_mock,
        ci,
        2,
        (
            (MOI.SingleVariable, MOI.EqualTo{Float64}, 1),
            (MOI.SingleVariable, MOI.ZeroOne, 0),
            (MOI.SingleVariable, MOI.Integer, 0),
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}, 0),
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}, 1),
        ),
    )

    s = """
    variables: x, y
    y == 4.0
    x in Semiinteger(2.0, 3.0)
    minobjective: x
    """
    model = MOIU.Model{Float64}()
    MOIU.loadfromstring!(model, s)
    sb = """
    variables: x, y, z
    y == 4.0
    z in ZeroOne()
    x in Integer()
    clo: x + -2.0z >= 0.0
    cup: x + -3.0z <= 0.0
    minobjective: x
    """
    modelb = MOIU.Model{Float64}()
    MOIU.loadfromstring!(modelb, sb)

    MOI.empty!(bridged_mock)
    @test MOI.is_empty(bridged_mock)
    MOIU.loadfromstring!(bridged_mock, s)
    MOIU.test_models_equal(
        bridged_mock,
        model,
        ["x", "y"],
        String[],
        [
            ("x", MOI.Semiinteger{Float64}(2.0, 3.0)),
            ("y", MOI.EqualTo{Float64}(4.0)),
        ],
    )

    # setting names on mock
    ci = first(
        MOI.get(
            mock,
            MOI.ListOfConstraintIndices{MOI.SingleVariable,MOI.ZeroOne}(),
        ),
    )
    z = MOI.VariableIndex(ci.value)
    MOI.set(mock, MOI.VariableName(), z, "z")
    ci = first(
        MOI.get(
            mock,
            MOI.ListOfConstraintIndices{MOI.SingleVariable,MOI.Integer}(),
        ),
    )
    ci = first(
        MOI.get(
            mock,
            MOI.ListOfConstraintIndices{
                MOI.ScalarAffineFunction{Float64},
                MOI.GreaterThan{Float64},
            }(),
        ),
    )
    MOI.set(mock, MOI.ConstraintName(), ci, "clo")
    ci = first(
        MOI.get(
            mock,
            MOI.ListOfConstraintIndices{
                MOI.ScalarAffineFunction{Float64},
                MOI.LessThan{Float64},
            }(),
        ),
    )
    MOI.set(mock, MOI.ConstraintName(), ci, "cup")

    MOIU.test_models_equal(
        mock,
        modelb,
        ["x", "y", "z"],
        ["clo", "cup"],
        [
            ("x", MOI.Integer()),
            ("y", MOI.EqualTo{Float64}(4.0)),
            ("z", MOI.ZeroOne()),
        ],
    )
end
