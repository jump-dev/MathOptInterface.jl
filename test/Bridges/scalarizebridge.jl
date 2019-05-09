using Test

using MathOptInterface
const MOI = MathOptInterface
const MOIT = MathOptInterface.Test
const MOIU = MathOptInterface.Utilities
const MOIB = MathOptInterface.Bridges

include("utilities.jl")

include("simple_model.jl")

mock = MOIU.MockOptimizer(SimpleModel{Float64}())
config = MOIT.TestConfig()

@testset "Scalarize" begin
    bridged_mock = MOIB.Scalarize{Float64}(mock)
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
    test_delete_bridge(bridged_mock, ci, 3,
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
