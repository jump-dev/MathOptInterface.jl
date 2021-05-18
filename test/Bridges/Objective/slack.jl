using Test

using MathOptInterface
const MOI = MathOptInterface
const MOIT = MathOptInterface.Test
const MOIU = MathOptInterface.Utilities
const MOIB = MathOptInterface.Bridges

include("../utilities.jl")

mock = MOIU.MockOptimizer(MOIU.Model{Float64}())
config = MOIT.TestConfig()

bridged_mock = MOIB.Objective.Slack{Float64}(mock)

@testset "Set objective before sense" begin
    err = ErrorException(
        "Set `MOI.ObjectiveSense` before `MOI.ObjectiveFunction` when" *
        " using `MOI.Bridges.Objective.SlackBridge`.",
    )
    F = MOI.ScalarAffineFunction{Float64}
    @test_throws err MOI.set(bridged_mock, MOI.ObjectiveFunction{F}(), zero(F))
end

@testset "solve_qp_edge_cases" begin
    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [1.0, 2.0, 5.0]),
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [1.0, 2.0, 7.0]),
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [1.0, 2.0, 2.0]),
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [1.0, 2.0, 7.0]),
        ),
    )
    MOIT.solve_qp_edge_cases(bridged_mock, config)

    @test MOIB.is_objective_bridged(bridged_mock)
    @test MOI.get(bridged_mock, MOI.ObjectiveFunctionType()) ==
          MOI.ScalarQuadraticFunction{Float64}
    @test MOI.get(bridged_mock, MOI.ListOfModelAttributesSet()) == [
        MOI.ObjectiveSense(),
        MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{Float64}}(),
    ]

    var_names = ["x", "y"]
    xy = MOI.get(bridged_mock, MOI.ListOfVariableIndices())
    MOI.set(bridged_mock, MOI.VariableName(), xy, var_names)
    @testset "Test mock model" begin
        abs = MOI.get(mock, MOI.ListOfVariableIndices())
        @test length(abs) == 3
        MOI.set(mock, MOI.VariableName(), abs[3], "s")
        cquad = MOI.get(
            mock,
            MOI.ListOfConstraintIndices{
                MOI.ScalarQuadraticFunction{Float64},
                MOI.LessThan{Float64},
            }(),
        )
        MOI.set(bridged_mock, MOI.ConstraintName(), cquad[1], "quad")

        s = """
        variables: x, y, s
        x >= 1.0
        y >= 2.0
        quad: 1.0 * x * x + 1.0 * x * y + 1.0 * y * y + -1.0 * s <= 0.0
        minobjective: s
        """
        model = MOIU.Model{Float64}()
        MOIU.loadfromstring!(model, s)
        MOIU.test_models_equal(
            mock,
            model,
            [var_names; "s"],
            ["quad"],
            [
                ("x", MOI.GreaterThan{Float64}(1.0)),
                ("y", MOI.GreaterThan{Float64}(2.0)),
            ],
        )
    end

    bridged_var_names = ["x", "y"]
    @testset "Test bridged model" begin
        s = """
        variables: x, y
        x >= 1.0
        y >= 2.0
        minobjective: 1.0 * x * x + 1.0 * x * y + 1.0 * y * y
        """
        model = MOIU.Model{Float64}()
        MOIU.loadfromstring!(model, s)
        MOIU.test_models_equal(
            bridged_mock,
            model,
            var_names,
            String[],
            [
                ("x", MOI.GreaterThan{Float64}(1.0)),
                ("y", MOI.GreaterThan{Float64}(2.0)),
            ],
        )
    end

    err = ArgumentError(
        "Objective bridge of type `$(MathOptInterface.Bridges.Objective.SlackBridge{Float64,MathOptInterface.ScalarQuadraticFunction{Float64},MathOptInterface.ScalarQuadraticFunction{Float64}})`" *
        " does not support modifying the objective sense. As a workaround, set" *
        " the sense to `MOI.FEASIBILITY_SENSE` to clear the objective function" *
        " and bridges.",
    )
    @test_throws err MOI.set(bridged_mock, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    obj = MOI.get(
        bridged_mock,
        MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{Float64}}(),
    )
    MOI.set(bridged_mock, MOI.ObjectiveSense(), MOI.FEASIBILITY_SENSE)
    @test !MOIB.is_objective_bridged(bridged_mock)
    @test MOI.get(bridged_mock, MOI.ObjectiveSense()) == MOI.FEASIBILITY_SENSE
    @test MOI.get(bridged_mock, MOI.ListOfModelAttributesSet()) ==
          [MOI.ObjectiveSense()]
    MOI.set(bridged_mock, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.set(
        bridged_mock,
        MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{Float64}}(),
        obj,
    )

    @testset "Test mock model" begin
        abs = MOI.get(mock, MOI.ListOfVariableIndices())
        @test length(abs) == 3
        MOI.set(mock, MOI.VariableName(), abs[3], "s")
        cquad = MOI.get(
            mock,
            MOI.ListOfConstraintIndices{
                MOI.ScalarQuadraticFunction{Float64},
                MOI.GreaterThan{Float64},
            }(),
        )
        MOI.set(bridged_mock, MOI.ConstraintName(), cquad[1], "quad")

        s = """
        variables: x, y, s
        x >= 1.0
        y >= 2.0
        quad: 1.0 * x * x + 1.0 * x * y + 1.0 * y * y + -1.0 * s >= 0.0
        maxobjective: s
        """
        model = MOIU.Model{Float64}()
        MOIU.loadfromstring!(model, s)
        MOIU.test_models_equal(
            mock,
            model,
            [var_names; "s"],
            ["quad"],
            [
                ("x", MOI.GreaterThan{Float64}(1.0)),
                ("y", MOI.GreaterThan{Float64}(2.0)),
            ],
        )
    end

    @testset "Test bridged model" begin
        s = """
        variables: x, y
        x >= 1.0
        y >= 2.0
        maxobjective: 1.0 * x * x + 1.0 * x * y + 1.0 * y * y
        """
        model = MOIU.Model{Float64}()
        MOIU.loadfromstring!(model, s)
        MOIU.test_models_equal(
            bridged_mock,
            model,
            var_names,
            String[],
            [
                ("x", MOI.GreaterThan{Float64}(1.0)),
                ("y", MOI.GreaterThan{Float64}(2.0)),
            ],
        )
    end

    test_delete_objective(
        bridged_mock,
        2,
        (
            (MOI.ScalarQuadraticFunction{Float64}, MOI.GreaterThan{Float64}, 0),
            (MOI.ScalarQuadraticFunction{Float64}, MOI.LessThan{Float64}, 0),
        ),
    )
end

@testset "QP" begin
    @testset "QP1" begin
        MOIU.set_mock_optimize!(
            mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                [4 / 7, 3 / 7, 6 / 7, 13 / 7],
                (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) => [5 / 7, 6 / 7],
            ),
        )
        MOIT.qp1test(bridged_mock, config)
    end
    @testset "QP2" begin
        MOIU.set_mock_optimize!(
            mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                [4 / 7, 3 / 7, 6 / 7, 13 / 7],
                (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) => [5 / 7, 6 / 7],
            ),
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                [4 / 7, 3 / 7, 6 / 7, -2 * 13 / 7],
                (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) => [10 / 7, 12 / 7],
            ),
        )
        MOIT.qp2test(bridged_mock, config)
    end
    @testset "QP3" begin
        MOIU.set_mock_optimize!(
            mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                [1 / 4, 3 / 4, 2.875],
                (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                    [11 / 4],
                (MOI.ScalarQuadraticFunction{Float64}, MOI.LessThan{Float64}) => [-1.0],
            ),
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                [1.0, 0.0, 3.0],
                (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                    [-2.0],
                (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) => [1.0],
            ),
        )
        MOIT.qp3test(bridged_mock, config)
    end
end
