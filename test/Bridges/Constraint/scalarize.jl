using Test

using MathOptInterface
const MOI = MathOptInterface
const MOIT = MathOptInterface.Test
const MOIU = MathOptInterface.Utilities
const MOIB = MathOptInterface.Bridges

include("../utilities.jl")

mock = MOIU.MockOptimizer(MOIU.Model{Float64}())
config = MOIT.TestConfig()

@testset "Scalarize" begin
    bridged_mock = MOIB.Constraint.Scalarize{Float64}(mock)

    MOIT.basic_constraint_tests(
        bridged_mock, config,
        include = [(F, S)
                   for F in [MOI.VectorOfVariables, MOI.VectorAffineFunction{Float64},
                             MOI.VectorQuadraticFunction{Float64}]
                   for S in [MOI.Nonnegatives, MOI.Nonpositives, MOI.Zeros]])

    # VectorOfVariables-in-Nonnegatives
    # VectorAffineFunction-in-Zeros
    mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1.0, 0.0, 2.0],
        (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) => [-3, -1])
    MOIT.lin1vtest(bridged_mock, config)
    ci = first(MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.Zeros}()))
    test_delete_bridge(bridged_mock, ci, 3,
        ((MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}, 0),
         (MOI.SingleVariable, MOI.GreaterThan{Float64}, 0)))
    ci = first(MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MOI.VectorOfVariables, MOI.Nonnegatives}()))
    func = MOI.get(bridged_mock, MOI.ConstraintFunction(), ci)
    MOI.delete(bridged_mock, func.variables[2])
    new_func = MOI.VectorOfVariables(func.variables[[1, 3]])
    @test MOI.get(bridged_mock, MOI.ConstraintFunction(), ci) == new_func
    @test MOI.get(bridged_mock, MOI.ConstraintSet(), ci) == MOI.Nonnegatives(2)
    test_delete_bridge(bridged_mock, ci, 2,
        ((MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}, 0),
         (MOI.SingleVariable, MOI.GreaterThan{Float64}, 0)))

    # VectorAffineFunction-in-Nonnegatives
    # VectorAffineFunction-in-Zeros
    mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1.0, 0.0, 2.0],
        (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) => [0, 2, 0],
        (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64})     => [-3, -1])
    MOIT.lin1ftest(bridged_mock, config)
    ci = first(MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.Zeros}()))
    test_delete_bridge(bridged_mock, ci, 3,
        ((MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}, 0),
         (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}, 0)))
    ci = first(MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives}()))
    test_delete_bridge(bridged_mock, ci, 3,
        ((MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}, 0),
         (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}, 0)))

    # VectorOfVariables-in-Nonnegatives
    # VectorOfVariables-in-Nonpositives
    # VectorOfVariables-in-Zeros
    # VectorAffineFunction-in-Zeros
    mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [-4, -3, 16, 0],
        (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64})        => [7, 2, -4])
    MOIT.lin2vtest(bridged_mock, config)

    # VectorAffineFunction-in-Nonnegatives
    # VectorAffineFunction-in-Nonpositives
    # VectorAffineFunction-in-Zeros
    mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [-4, -3, 16, 0],
        (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) => [0],
        (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})    => [0],
        (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64})     => [7, 2, -4, 7])
    MOIT.lin2ftest(bridged_mock, config)
end
