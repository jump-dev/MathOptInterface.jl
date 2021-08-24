module TestConstraintZeroOne

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

function test_ZeroOne()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    config = MOI.Test.Config()
    bridged_mock = MOI.Bridges.Constraint.ZeroOne{Float64}(mock)

    bridge_type = MOI.Bridges.Constraint.ZeroOneBridge{Float64}
    @test MOI.supports_constraint(bridge_type, MOI.SingleVariable, MOI.ZeroOne)
    @test MOI.Bridges.Constraint.concrete_bridge_type(
        bridge_type,
        MOI.SingleVariable,
        MOI.ZeroOne,
    ) == bridge_type

    @test MOI.supports(bridged_mock, MOI.ConstraintPrimalStart(), bridge_type)
    MOI.Test.test_basic_SingleVariable_ZeroOne(bridged_mock, config)
    MOI.empty!(bridged_mock)
    MOI.Utilities.set_mock_optimize!(
        mock,
        (mock::MOI.Utilities.MockOptimizer) ->
            MOI.Utilities.mock_optimize!(mock, [1, 0, 0, 1, 1]),
    )
    MOI.Test.test_linear_integer_knapsack(bridged_mock, config)
    ci = first(
        MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{MOI.SingleVariable,MOI.ZeroOne}(),
        ),
    )
    @test MOI.get(bridged_mock, MOI.ConstraintPrimal(), ci) == 1
    _test_delete_bridge(
        bridged_mock,
        ci,
        5,
        (
            (MOI.SingleVariable, MOI.Integer, 0),
            (MOI.SingleVariable, MOI.Interval{Float64}, 0),
        ),
        num_bridged = 5,
    )
    MOI.empty!(bridged_mock)
    MOI.Utilities.set_mock_optimize!(
        mock,
        (mock::MOI.Utilities.MockOptimizer) -> begin
            MOI.set(mock, MOI.ObjectiveBound(), 20.0)
            MOI.Utilities.mock_optimize!(mock, [4, 5, 1])
        end,
    )
    MOI.Test.test_linear_integer_integration(bridged_mock, config)
    MOI.empty!(bridged_mock)
    MOI.Utilities.set_mock_optimize!(
        mock,
        (mock::MOI.Utilities.MockOptimizer) ->
            MOI.Utilities.mock_optimize!(mock, [1.0; zeros(10)]),
    )
    MOI.Test.test_linear_integer_solve_twice(bridged_mock, config)
    ci = first(
        MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{MOI.SingleVariable,MOI.ZeroOne}(),
        ),
    )
    for attr in [MOI.ConstraintPrimalStart()]
        @test MOI.supports(bridged_mock, attr, typeof(ci))
        value = 1.0
        MOI.set(bridged_mock, attr, ci, value)
        @test MOI.get(bridged_mock, attr, ci) ≈ value
    end
    s = """
    variables: x, y
    y == 1.0
    x in ZeroOne()
    minobjective: x
    """
    model = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(model, s)
    sb = """
    variables: x, y
    y == 1.0
    x in Integer()
    x in Interval(0.0,1.0)
    minobjective: x
    """
    modelb = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(modelb, sb)
    MOI.empty!(bridged_mock)
    @test MOI.is_empty(bridged_mock)
    MOI.Utilities.loadfromstring!(bridged_mock, s)
    MOI.Test.test_models_equal(
        bridged_mock,
        model,
        ["x", "y"],
        String[],
        [("y", MOI.EqualTo{Float64}(1.0)), ("x", MOI.ZeroOne())],
    )
    MOI.Test.test_models_equal(
        mock,
        modelb,
        ["x", "y"],
        String[],
        [
            ("y", MOI.EqualTo{Float64}(1.0)),
            ("x", MOI.Integer()),
            ("x", MOI.Interval{Float64}(0.0, 1.0)),
        ],
    )
    return
end

end  # module

TestConstraintZeroOne.runtests()
