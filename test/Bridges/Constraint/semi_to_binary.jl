module TestConstraintSemiToBinary

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

include("../utilities.jl")

function test_SemiToBinary()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    config = MOI.Test.Config()
    bridged_mock = MOI.Bridges.Constraint.SemiToBinary{Float64}(mock)
    bridge_type = MOI.Bridges.Constraint.SemiToBinaryBridge{
        Float64,
        MOI.Semiinteger{Float64},
    }
    @test MOI.supports_constraint(
        bridge_type,
        MOI.VariableIndex,
        MOI.Semiinteger{Float64},
    )
    @test MOI.Bridges.Constraint.concrete_bridge_type(
        bridge_type,
        MOI.VariableIndex,
        MOI.Semiinteger{Float64},
    ) == bridge_type
    @test MOI.supports(bridged_mock, MOI.ConstraintPrimalStart(), bridge_type)
    MOI.Test.runtests(
        bridged_mock,
        config,
        include = [
            "test_basic_VariableIndex_Semiinteger",
            "test_basic_VariableIndex_Semicontinuous",
        ],
    )
    MOI.empty!(bridged_mock)
    MOI.Utilities.set_mock_optimize!(
        mock,
        (mock::MOI.Utilities.MockOptimizer) -> begin
            MOI.set(mock, MOI.ObjectiveBound(), 0.0)
            MOI.Utilities.mock_optimize!(mock, [0.0, 0.0, 0.0])
        end,
        (mock::MOI.Utilities.MockOptimizer) -> begin
            MOI.set(mock, MOI.ObjectiveBound(), 2.0)
            MOI.Utilities.mock_optimize!(mock, [2.0, 1.0, 1.0])
        end,
        (mock::MOI.Utilities.MockOptimizer) -> begin
            MOI.set(mock, MOI.ObjectiveBound(), 2.0)
            MOI.Utilities.mock_optimize!(mock, [2.0, 2.0, 1.0])
        end,
        (mock::MOI.Utilities.MockOptimizer) -> begin
            MOI.set(mock, MOI.ObjectiveBound(), 2.5)
            MOI.Utilities.mock_optimize!(mock, [2.5, 2.5, 1.0])
        end,
        (mock::MOI.Utilities.MockOptimizer) -> begin
            MOI.set(mock, MOI.ObjectiveBound(), 3.0)
            MOI.Utilities.mock_optimize!(mock, [3.0, 3.0, 1.0])
        end,
        (mock::MOI.Utilities.MockOptimizer) ->
            MOI.set(mock, MOI.TerminationStatus(), MOI.INFEASIBLE),
    )
    MOI.Test.test_linear_Semicontinuous_integration(bridged_mock, config)
    ci = first(
        MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{
                MOI.VariableIndex,
                MOI.Semicontinuous{Float64},
            }(),
        ),
    )
    _test_delete_bridge(
        bridged_mock,
        ci,
        2,
        (
            (MOI.VariableIndex, MOI.EqualTo{Float64}, 1),
            (MOI.VariableIndex, MOI.ZeroOne, 0),
            (MOI.VariableIndex, MOI.Integer, 0),
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}, 0),
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}, 1),
        ),
    )
    MOI.empty!(bridged_mock)
    MOI.Utilities.set_mock_optimize!(
        mock,
        (mock::MOI.Utilities.MockOptimizer) -> begin
            MOI.set(mock, MOI.ObjectiveBound(), 0.0)
            MOI.Utilities.mock_optimize!(mock, [0.0, 0.0, 0.0])
        end,
        (mock::MOI.Utilities.MockOptimizer) -> begin
            MOI.set(mock, MOI.ObjectiveBound(), 2.0)
            MOI.Utilities.mock_optimize!(mock, [2.0, 1.0, 1.0])
        end,
        (mock::MOI.Utilities.MockOptimizer) -> begin
            MOI.set(mock, MOI.ObjectiveBound(), 2.0)
            MOI.Utilities.mock_optimize!(mock, [2.0, 2.0, 1.0])
        end,
        (mock::MOI.Utilities.MockOptimizer) -> begin
            MOI.set(mock, MOI.ObjectiveBound(), 3.0)
            MOI.Utilities.mock_optimize!(mock, [3.0, 2.5, 1.0])
        end,
        (mock::MOI.Utilities.MockOptimizer) -> begin
            MOI.set(mock, MOI.ObjectiveBound(), 3.0)
            MOI.Utilities.mock_optimize!(mock, [3.0, 3.0, 1.0])
        end,
        (mock::MOI.Utilities.MockOptimizer) ->
            MOI.set(mock, MOI.TerminationStatus(), MOI.INFEASIBLE),
    )
    MOI.Test.test_linear_Semiinteger_integration(bridged_mock, config)
    ci = first(
        MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{
                MOI.VariableIndex,
                MOI.Semiinteger{Float64},
            }(),
        ),
    )

    @test MOI.get(bridged_mock, MOI.ConstraintPrimal(), ci) == 3
    new_set = MOI.Semiinteger{Float64}(19.0, 20.0)
    MOI.set(bridged_mock, MOI.ConstraintSet(), ci, new_set)
    @test MOI.get(bridged_mock, MOI.ConstraintSet(), ci) == new_set
    for attr in [MOI.ConstraintPrimalStart()]
        @test MOI.supports(bridged_mock, attr, typeof(ci))
        value = 2.0
        MOI.set(bridged_mock, attr, ci, value)
        @test MOI.get(bridged_mock, attr, ci) ≈ value
    end
    _test_delete_bridge(
        bridged_mock,
        ci,
        2,
        (
            (MOI.VariableIndex, MOI.EqualTo{Float64}, 1),
            (MOI.VariableIndex, MOI.ZeroOne, 0),
            (MOI.VariableIndex, MOI.Integer, 0),
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
    model = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(model, s)
    sb = """
    variables: x, y, z
    y == 4.0
    z in ZeroOne()
    x in Integer()
    clo: x + -2.0z >= 0.0
    cup: x + -3.0z <= 0.0
    minobjective: x
    """
    modelb = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(modelb, sb)
    MOI.empty!(bridged_mock)
    @test MOI.is_empty(bridged_mock)
    MOI.Utilities.loadfromstring!(bridged_mock, s)
    MOI.Utilities.test_models_equal(
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
            MOI.ListOfConstraintIndices{MOI.VariableIndex,MOI.ZeroOne}(),
        ),
    )
    z = MOI.VariableIndex(ci.value)
    MOI.set(mock, MOI.VariableName(), z, "z")
    ci = first(
        MOI.get(
            mock,
            MOI.ListOfConstraintIndices{MOI.VariableIndex,MOI.Integer}(),
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
    MOI.Utilities.test_models_equal(
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
    return
end

end  # module

TestConstraintSemiToBinary.runtests()
