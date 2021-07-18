using Test

using MathOptInterface
const MOI = MathOptInterface
const MOIT = MathOptInterface.DeprecatedTest
const MOIU = MathOptInterface.Utilities
const MOIB = MathOptInterface.Bridges

include("../utilities.jl")

mock = MOIU.MockOptimizer(MOIU.UniversalFallback(MOIU.Model{Float64}()))
config = MOIT.Config()

bridged_mock = MOIB.Variable.Free{Float64}(mock)

@testset "solve_multirow_vectoraffine_nonpos" begin
    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [0.5, 0.0]),
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [0.25, 0.0]),
        ),
    )
    MOIT.solve_multirow_vectoraffine_nonpos(bridged_mock, config)

    MOI.set(
        bridged_mock,
        MOI.ConstraintName(),
        MOI.get(
            mock,
            MOI.ListOfConstraintIndices{
                MOI.VectorAffineFunction{Float64},
                MOI.NonpositiveCone,
            }(),
        ),
        ["c"],
    )

    @testset "Mock model" begin
        var_names = ["xpos", "xneg"]
        MOI.set(
            mock,
            MOI.VariableName(),
            MOI.get(mock, MOI.ListOfVariableIndices()),
            var_names,
        )
        MOI.set(
            bridged_mock,
            MOI.ConstraintName(),
            MOI.get(
                mock,
                MOI.ListOfConstraintIndices{
                    MOI.VectorOfVariables,
                    MOI.NonnegativeCone,
                }(),
            ),
            ["nonneg"],
        )
        s = """
        variables: xpos, xneg
        nonneg: [xpos, xneg] in MathOptInterface.NonnegativeCone(2)
        c: [4.0xpos + -4.0xneg + -1.0, 3.0xpos + -3.0xneg + -1.0] in MathOptInterface.NonpositiveCone(2)
        maxobjective: xpos + -1.0xneg
        """
        model = MOIU.Model{Float64}()
        MOIU.loadfromstring!(model, s)
        MOIU.test_models_equal(mock, model, var_names, ["nonneg", "c"])
    end
    @testset "Bridged model" begin
        s = """
        variables: x
        c: [4.0x + -1.0, 3.0x + -1.0] in MathOptInterface.NonpositiveCone(2)
        maxobjective: 1.0x
        """
        model = MOIU.Model{Float64}()
        MOIU.loadfromstring!(model, s)
        MOIU.test_models_equal(bridged_mock, model, ["x"], ["c"])
    end
end

@testset "Linear6" begin
    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [0, 0, 0, 0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [100, 0, 0, 0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [100, 0, 0, 100],
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) =>
                [1.0],
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) =>
                [-1.0],
        ),
    )
    MOIT.linear6test(bridged_mock, config)

    loc = MOI.get(bridged_mock, MOI.ListOfConstraintTypesPresent())
    @test length(loc) == 2
    @test !((MOI.VectorOfVariables, MOI.Reals) in loc)
    @test (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) in loc
    @test (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) in loc
    @test MOI.get(mock, MOI.NumberOfVariables()) == 4
    @test MOI.get(bridged_mock, MOI.NumberOfVariables()) == 2
    vis = MOI.get(bridged_mock, MOI.ListOfVariableIndices())
    @test vis == MOI.VariableIndex.([-1, -2])

    cx = MOI.ConstraintIndex{MOI.VectorOfVariables,MOI.Reals}(vis[1].value)
    @test MOI.get(bridged_mock, MOI.ConstraintPrimal(), cx) == [100.0]
    @test MOI.get(bridged_mock, MOI.ConstraintDual(), cx) == [0.0]
    cy = MOI.ConstraintIndex{MOI.VectorOfVariables,MOI.Reals}(vis[2].value)
    @test MOI.get(bridged_mock, MOI.ConstraintPrimal(), cy) == [-100.0]
    @test MOI.get(bridged_mock, MOI.ConstraintDual(), cy) == [0.0]

    _test_delete_bridged_variable(
        bridged_mock,
        vis[1],
        MOI.Reals,
        2,
        (
            (MOI.VectorOfVariables, MOI.NonnegativeCone, 0),
            (MOI.VectorOfVariables, MOI.NonpositiveCone, 0),
        ),
    )
    _test_delete_bridged_variable(
        bridged_mock,
        vis[2],
        MOI.Reals,
        1,
        (
            (MOI.VectorOfVariables, MOI.NonnegativeCone, 0),
            (MOI.VectorOfVariables, MOI.NonpositiveCone, 0),
        ),
    )
end

@testset "Linear7" begin
    function set_mock_optimize_linear7Test!(mock)
        return MOIU.set_mock_optimize!(
            mock,
            (mock::MOIU.MockOptimizer) ->
                MOIU.mock_optimize!(mock, [0, 0, 0, 0]),
            (mock::MOIU.MockOptimizer) ->
                MOIU.mock_optimize!(mock, [100, 0, 0, 0]),
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                [100, 0, 0, 100],
                (MOI.VectorAffineFunction{Float64}, MOI.NonnegativeCone) =>
                    [[1.0]],
                (MOI.VectorAffineFunction{Float64}, MOI.NonpositiveCone) =>
                    [[-1.0]],
            ),
        )
    end
    set_mock_optimize_linear7Test!(mock)
    MOIT.linear7test(bridged_mock, config)

    x, y = MOI.get(bridged_mock, MOI.ListOfVariableIndices())

    cx = MOI.ConstraintIndex{MOI.VectorOfVariables,MOI.Reals}(x.value)
    @test MOI.get(bridged_mock, MOI.ConstraintPrimal(), cx) == [100.0]
    @test MOI.get(bridged_mock, MOI.ConstraintDual(), cx) == [0.0]
    cy = MOI.ConstraintIndex{MOI.VectorOfVariables,MOI.Reals}(y.value)
    @test MOI.get(bridged_mock, MOI.ConstraintPrimal(), cy) == [-100.0]
    @test MOI.get(bridged_mock, MOI.ConstraintDual(), cy) == [0.0]

    @test MOI.supports(
        bridged_mock,
        MOI.VariablePrimalStart(),
        MOI.VariableIndex,
    )
    @test MOI.supports(
        bridged_mock,
        MOI.VariablePrimalStart(),
        typeof(MOIB.bridge(bridged_mock, x)),
    )
    MOI.set(bridged_mock, MOI.VariablePrimalStart(), [x, y], [1.0, -1.0])
    xa, xb, ya, yb = MOI.get(mock, MOI.ListOfVariableIndices())
    @test MOI.get(mock, MOI.VariablePrimalStart(), [xa, xb, ya, yb]) ==
          [1.0, 0.0, 0.0, 1.0]
    @test MOI.get(bridged_mock, MOI.VariablePrimalStart(), x) == 1
    @test MOI.get(bridged_mock, MOI.VariablePrimalStart(), y) == -1
end

@testset "Linear11" begin
    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) ->
            MOIU.mock_optimize!(mock, [1.0, 1.0, 0.0, 0.0]),
        (mock::MOIU.MockOptimizer) ->
            MOIU.mock_optimize!(mock, [0.5, 0.5, 0.0, 0.0]),
    )
    MOIT.linear11test(bridged_mock, config)

    vis = MOI.get(bridged_mock, MOI.ListOfVariableIndices())
    @test vis == MOI.VariableIndex.([-1, -2])

    MOI.set(
        bridged_mock,
        MOI.ConstraintName(),
        MOI.get(
            mock,
            MOI.ListOfConstraintIndices{
                MOI.ScalarAffineFunction{Float64},
                MOI.GreaterThan{Float64},
            }(),
        ),
        ["c1"],
    )
    MOI.set(
        bridged_mock,
        MOI.ConstraintName(),
        MOI.get(
            mock,
            MOI.ListOfConstraintIndices{
                MOI.ScalarAffineFunction{Float64},
                MOI.LessThan{Float64},
            }(),
        ),
        ["c2"],
    )

    @testset "Mock model" begin
        var_names = ["v1pos", "v2pos", "v1neg", "v2neg"]
        MOI.set(
            mock,
            MOI.VariableName(),
            MOI.get(mock, MOI.ListOfVariableIndices()),
            var_names,
        )
        MOI.set(
            bridged_mock,
            MOI.ConstraintName(),
            MOI.get(
                mock,
                MOI.ListOfConstraintIndices{
                    MOI.VectorOfVariables,
                    MOI.NonnegativeCone,
                }(),
            ),
            ["nonneg"],
        )
        s = """
        variables: v1pos, v2pos, v1neg, v2neg
        nonneg: [v1pos, v2pos, v1neg, v2neg] in MathOptInterface.NonnegativeCone(4)
        c1: v1pos + -1.0v1neg + v2pos + -1.0v2neg >= 1.0
        c2: v1pos + -1.0v1neg + v2pos + -1.0v2neg <= 2.0
        minobjective: v1pos + -1.0v1neg + v2pos + -1.0v2neg
        """
        model = MOIU.Model{Float64}()
        MOIU.loadfromstring!(model, s)
        MOIU.test_models_equal(mock, model, var_names, ["nonneg", "c1", "c2"])
    end
    @testset "Bridged model" begin
        var_names = ["v1", "v2"]
        MOI.set(bridged_mock, MOI.VariableName(), vis, var_names)
        s = """
        variables: v1, v2
        c1: v1 + v2 >= 1.0
        c2: v1 + v2 <= 2.0
        minobjective: v1 + v2
        """
        model = MOIU.Model{Float64}()
        MOIU.loadfromstring!(model, s)
        MOIU.test_models_equal(bridged_mock, model, var_names, ["c1", "c2"])
    end

    _test_delete_bridged_variable(
        bridged_mock,
        vis[1],
        MOI.Reals,
        2,
        (
            (MOI.VectorOfVariables, MOI.NonnegativeCone, 0),
            (MOI.VectorOfVariables, MOI.NonpositiveCone, 0),
        ),
        used_bridges = 0,
        used_constraints = 0,
    )
    _test_delete_bridged_variable(
        bridged_mock,
        vis[2],
        MOI.Reals,
        1,
        (
            (MOI.VectorOfVariables, MOI.NonnegativeCone, 0),
            (MOI.VectorOfVariables, MOI.NonpositiveCone, 0),
        ),
    )
end
