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

@testset "ZeroOne" begin
    bridged_mock = MOIBC.ZeroOne{Float64}(mock)

    bridge_type = MOIBC.ZeroOneBridge{Float64}
    @test MOI.supports_constraint(bridge_type, MOI.SingleVariable, MOI.ZeroOne)
    @test MOIBC.concrete_bridge_type(
        bridge_type,
        MOI.SingleVariable,
        MOI.ZeroOne,
    ) == bridge_type

    @test MOI.supports(bridged_mock, MOI.ConstraintPrimalStart(), bridge_type)

    MOIT.basic_constraint_tests(
        bridged_mock,
        config,
        include = [(MOI.SingleVariable, MOI.ZeroOne)],
    )

    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) ->
            MOIU.mock_optimize!(mock, [1, 0, 0, 1, 1]),
    )
    MOIT.knapsacktest(bridged_mock, config)

    ci = first(
        MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{MOI.SingleVariable,MOI.ZeroOne}(),
        ),
    )

    @test MOI.get(bridged_mock, MOI.ConstraintPrimal(), ci) == 1

    test_delete_bridge(
        bridged_mock,
        ci,
        5,
        (
            (MOI.SingleVariable, MOI.Integer, 0),
            (MOI.SingleVariable, MOI.Interval{Float64}, 0),
        ),
        num_bridged = 5,
    )

    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> begin
            MOI.set(mock, MOI.ObjectiveBound(), 20.0)
            MOIU.mock_optimize!(mock, [4, 5, 1])
        end,
    )
    MOIT.int1test(bridged_mock, config)

    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) ->
            MOIU.mock_optimize!(mock, [1.0; zeros(10)]),
    )
    MOIT.int3test(bridged_mock, config)

    ci = first(
        MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{MOI.SingleVariable,MOI.ZeroOne}(),
        ),
    )

    @testset "$attr" for attr in [MOI.ConstraintPrimalStart()]
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
    model = MOIU.Model{Float64}()
    MOIU.loadfromstring!(model, s)
    sb = """
    variables: x, y
    y == 1.0
    x in Integer()
    x in Interval(0.0,1.0)
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
        [("y", MOI.EqualTo{Float64}(1.0)), ("x", MOI.ZeroOne())],
    )
    MOIU.test_models_equal(
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
end
