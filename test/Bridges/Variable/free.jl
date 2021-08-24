module TestVariableFree

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

function test_modification_multirow_vectoraffine_nonpos()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    bridged_mock = MOI.Bridges.Variable.Free{Float64}(mock)
    MOI.Utilities.set_mock_optimize!(
        mock,
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [0.5, 0.0]),
        ),
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, [0.25, 0.0]),
        ),
    )
    MOI.Test.test_modification_multirow_vectoraffine_nonpos(
        bridged_mock,
        MOI.Test.Config(),
    )
    MOI.set(
        bridged_mock,
        MOI.ConstraintName(),
        MOI.get(
            mock,
            MOI.ListOfConstraintIndices{
                MOI.VectorAffineFunction{Float64},
                MOI.Nonpositives,
            }(),
        ),
        ["c"],
    )
    var_names = ["xpos", "xneg"]
    MOI.set(
        mock,
        MOI.VariableName(),
        MOI.get(mock, MOI.ListOfVariableIndices()),
        var_names,
    )
    MOI.set(
        bridged_mock,
        MOI.VariableName(),
        MOI.get(bridged_mock, MOI.ListOfVariableIndices()),
        ["x"],
    )
    MOI.set(
        bridged_mock,
        MOI.ConstraintName(),
        MOI.get(
            mock,
            MOI.ListOfConstraintIndices{
                MOI.VectorOfVariables,
                MOI.Nonnegatives,
            }(),
        ),
        ["nonneg"],
    )
    s = """
    variables: xpos, xneg
    nonneg: [xpos, xneg] in MathOptInterface.Nonnegatives(2)
    c: [4.0xpos + -4.0xneg + -1.0, 3.0xpos + -3.0xneg + -1.0] in MathOptInterface.Nonpositives(2)
    maxobjective: xpos + -1.0xneg
    """
    model = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(model, s)
    MOI.Test.test_models_equal(mock, model, var_names, ["nonneg", "c"])
    s = """
    variables: x
    c: [4.0x + -1.0, 3.0x + -1.0] in MathOptInterface.Nonpositives(2)
    maxobjective: 1.0x
    """
    model = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(model, s)
    MOI.Test.test_models_equal(bridged_mock, model, ["x"], ["c"])
    return
end

function test_linear_LessThan_and_GreaterThan()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    bridged_mock = MOI.Bridges.Variable.Free{Float64}(mock)
    MOI.Utilities.set_mock_optimize!(
        mock,
        (mock::MOI.Utilities.MockOptimizer) ->
            MOI.Utilities.mock_optimize!(mock, [0, 0, 0, 0]),
        (mock::MOI.Utilities.MockOptimizer) ->
            MOI.Utilities.mock_optimize!(mock, [100, 0, 0, 0]),
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            [100, 0, 0, 100],
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) =>
                [1.0],
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) =>
                [-1.0],
        ),
    )
    MOI.Test.test_linear_LessThan_and_GreaterThan(
        bridged_mock,
        MOI.Test.Config(),
    )

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
            (MOI.VectorOfVariables, MOI.Nonnegatives, 0),
            (MOI.VectorOfVariables, MOI.Nonpositives, 0),
        ),
    )
    _test_delete_bridged_variable(
        bridged_mock,
        vis[2],
        MOI.Reals,
        1,
        (
            (MOI.VectorOfVariables, MOI.Nonnegatives, 0),
            (MOI.VectorOfVariables, MOI.Nonpositives, 0),
        ),
    )
    return
end

function test_linear_integration_modification()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    bridged_mock = MOI.Bridges.Variable.Free{Float64}(mock)
    MOI.Utilities.set_mock_optimize!(
        mock,
        (mock::MOI.Utilities.MockOptimizer) ->
            MOI.Utilities.mock_optimize!(mock, [0, 0, 0, 0]),
        (mock::MOI.Utilities.MockOptimizer) ->
            MOI.Utilities.mock_optimize!(mock, [100, 0, 0, 0]),
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            [100, 0, 0, 100],
            (MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives) =>
                [[1.0]],
            (MOI.VectorAffineFunction{Float64}, MOI.Nonpositives) =>
                [[-1.0]],
        ),
    )
    MOI.Test.test_linear_VectorAffineFunction(bridged_mock, MOI.Test.Config())

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
        typeof(MOI.Bridges.bridge(bridged_mock, x)),
    )
    MOI.set(bridged_mock, MOI.VariablePrimalStart(), [x, y], [1.0, -1.0])
    xa, xb, ya, yb = MOI.get(mock, MOI.ListOfVariableIndices())
    @test MOI.get(mock, MOI.VariablePrimalStart(), [xa, xb, ya, yb]) ==
          [1.0, 0.0, 0.0, 1.0]
    @test MOI.get(bridged_mock, MOI.VariablePrimalStart(), x) == 1
    @test MOI.get(bridged_mock, MOI.VariablePrimalStart(), y) == -1
    return
end

function test_linear_transform()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    bridged_mock = MOI.Bridges.Variable.Free{Float64}(mock)
    MOI.Utilities.set_mock_optimize!(
        mock,
        (mock::MOI.Utilities.MockOptimizer) ->
            MOI.Utilities.mock_optimize!(mock, [1.0, 1.0, 0.0, 0.0]),
        (mock::MOI.Utilities.MockOptimizer) ->
            MOI.Utilities.mock_optimize!(mock, [0.5, 0.5, 0.0, 0.0]),
    )
    MOI.Test.test_linear_transform(bridged_mock, MOI.Test.Config())

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
                MOI.Nonnegatives,
            }(),
        ),
        ["nonneg"],
    )
    s = """
    variables: v1pos, v2pos, v1neg, v2neg
    nonneg: [v1pos, v2pos, v1neg, v2neg] in MathOptInterface.Nonnegatives(4)
    c1: v1pos + -1.0v1neg + v2pos + -1.0v2neg >= 1.0
    c2: v1pos + -1.0v1neg + v2pos + -1.0v2neg <= 2.0
    minobjective: v1pos + -1.0v1neg + v2pos + -1.0v2neg
    """
    model = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(model, s)
    MOI.Test.test_models_equal(mock, model, var_names, ["nonneg", "c1", "c2"])
    var_names = ["v1", "v2"]
    MOI.set(bridged_mock, MOI.VariableName(), vis, var_names)
    s = """
    variables: v1, v2
    c1: v1 + v2 >= 1.0
    c2: v1 + v2 <= 2.0
    minobjective: v1 + v2
    """
    model = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(model, s)
    MOI.Test.test_models_equal(bridged_mock, model, var_names, ["c1", "c2"])
    _test_delete_bridged_variable(
        bridged_mock,
        vis[1],
        MOI.Reals,
        2,
        (
            (MOI.VectorOfVariables, MOI.Nonnegatives, 0),
            (MOI.VectorOfVariables, MOI.Nonpositives, 0),
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
            (MOI.VectorOfVariables, MOI.Nonnegatives, 0),
            (MOI.VectorOfVariables, MOI.Nonpositives, 0),
        ),
    )
    return
end

end  # module

TestVariableFree.runtests()
