using Test

using MathOptInterface
const MOI = MathOptInterface
const MOIT = MathOptInterface.Test
const MOIU = MathOptInterface.Utilities
const MOIB = MathOptInterface.Bridges

include("../utilities.jl")

mock = MOIU.MockOptimizer(MOIU.UniversalFallback(MOIU.Model{Float64}()))
config = MOIT.TestConfig()

bridged_mock = MOIB.Variable.Free{Float64}(mock)

@testset "solve_multirow_vectoraffine_nonpos" begin
    MOIU.set_mock_optimize!(mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock,
            MOI.OPTIMAL, (MOI.FEASIBLE_POINT, [0.5, 0.0])
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock,
            MOI.OPTIMAL, (MOI.FEASIBLE_POINT, [0.25, 0.0])
        )
    )
    MOIT.solve_multirow_vectoraffine_nonpos(bridged_mock, config)
end

@testset "Linear6" begin
    MOIU.set_mock_optimize!(mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [0, 0, 0, 0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [100, 0, 0, 0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [100, 0, 0, -100],
        (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) => [1.0],
        (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) => [-1.0]
    ))
    MOIT.linear6test(bridged_mock, config)

    loc = MOI.get(bridged_mock, MOI.ListOfConstraints())
    @test length(loc) == 2
    @test !((MOI.VectorOfVariables, MOI.Reals) in loc)
    @test (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) in loc
    @test (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) in loc
    @test MOI.get(mock, MOI.NumberOfVariables()) == 4
    @test MOI.get(bridged_mock, MOI.NumberOfVariables()) == 2
    vis = MOI.get(bridged_mock, MOI.ListOfVariableIndices())
    @test vis == MOI.VariableIndex.([-1, -2])

    cx = MOI.ConstraintIndex{MOI.VectorOfVariables, MOI.Reals}(vis[1].value)
    @test MOI.get(bridged_mock, MOI.ConstraintPrimal(), cx) == [100.0]
    @test MOI.get(bridged_mock, MOI.ConstraintDual(), cx) == [0.0]
    cy = MOI.ConstraintIndex{MOI.VectorOfVariables, MOI.Reals}(vis[2].value)
    @test MOI.get(bridged_mock, MOI.ConstraintPrimal(), cy) == [-100.0]
    @test MOI.get(bridged_mock, MOI.ConstraintDual(), cy) == [0.0]

    test_delete_bridged_variable(bridged_mock, vis[1], MOI.Reals, 2, (
        (MOI.VectorOfVariables, MOI.Nonnegatives, 0),
        (MOI.VectorOfVariables, MOI.Nonpositives, 0)
    ))
    test_delete_bridged_variable(bridged_mock, vis[2], MOI.Reals, 1, (
        (MOI.VectorOfVariables, MOI.Nonnegatives, 0),
        (MOI.VectorOfVariables, MOI.Nonpositives, 0)
    ))
end

@testset "Linear7" begin
    function set_mock_optimize_linear7Test!(mock)
        MOIU.set_mock_optimize!(mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [0, 0, 0, 0]),
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [100, 0, 0, 0]),
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [100, 0, 0, -100],
            (MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives) => [[1.0]],
            (MOI.VectorAffineFunction{Float64}, MOI.Nonpositives) => [[-1.0]]
    ))
    end
    set_mock_optimize_linear7Test!(mock)
    MOIT.linear7test(bridged_mock, config)

    x, y = MOI.get(bridged_mock, MOI.ListOfVariableIndices())

    cx = MOI.ConstraintIndex{MOI.VectorOfVariables, MOI.Reals}(x.value)
    @test MOI.get(bridged_mock, MOI.ConstraintPrimal(), cx) == [100.0]
    @test MOI.get(bridged_mock, MOI.ConstraintDual(), cx) == [0.0]
    cy = MOI.ConstraintIndex{MOI.VectorOfVariables, MOI.Reals}(y.value)
    @test MOI.get(bridged_mock, MOI.ConstraintPrimal(), cy) == [-100.0]
    @test MOI.get(bridged_mock, MOI.ConstraintDual(), cy) == [0.0]

    @test MOI.supports(bridged_mock, MOI.VariablePrimalStart(), MOI.VariableIndex)
    MOI.set(bridged_mock, MOI.VariablePrimalStart(), [x, y], [1.0, -1.0])
    xa, xb, ya, yb = MOI.get(mock, MOI.ListOfVariableIndices())
    @test MOI.get(mock, MOI.VariablePrimalStart(), [xa, xb, ya, yb]) == [1.0, 0.0, 0.0, -1.0]
end

@testset "Linear11" begin
    MOIU.set_mock_optimize!(mock,
         (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1.0, 0.0, 1.0, 0.0]),
         (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [0.5, 0.0, 0.5, 0.0]))
    MOIT.linear11test(bridged_mock, config)

    vis = MOI.get(bridged_mock, MOI.ListOfVariableIndices())
    @test vis == MOI.VariableIndex.([-1, -2])

    test_delete_bridged_variable(bridged_mock, vis[1], MOI.Reals, 2, (
        (MOI.VectorOfVariables, MOI.Nonnegatives, 0),
        (MOI.VectorOfVariables, MOI.Nonpositives, 0)
    ), used_bridges = 0, used_constraints = 0)
    test_delete_bridged_variable(bridged_mock, vis[2], MOI.Reals, 1, (
        (MOI.VectorOfVariables, MOI.Nonnegatives, 0),
        (MOI.VectorOfVariables, MOI.Nonpositives, 0)
    ))
end
