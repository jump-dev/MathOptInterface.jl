using Test

using MathOptInterface
const MOI = MathOptInterface
const MOIT = MathOptInterface.Test
const MOIU = MathOptInterface.Utilities
const MOIB = MathOptInterface.Bridges

include("../utilities.jl")

mock = MOIU.MockOptimizer(MOIU.Model{Float64}())
config = MOIT.TestConfig()
config_with_basis = MOIT.TestConfig(basis = true)

@testset "Interval" begin
    bridged_mock = MOIB.Constraint.SplitInterval{Float64}(mock)
    MOIT.basic_constraint_tests(bridged_mock, config,
                                include=[(MOI.SingleVariable,
                                          MOI.Interval{Float64}),
                                         (MOI.ScalarAffineFunction{Float64},
                                          MOI.Interval{Float64}),
                                         (MOI.ScalarQuadraticFunction{Float64},
                                          MOI.Interval{Float64})])
    MOIU.set_mock_optimize!(mock,
         (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [5.0, 5.0], con_basis =
             [(MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) => [MOI.BASIC],
              (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})    => [MOI.NONBASIC],
              (MOI.SingleVariable, MOI.GreaterThan{Float64})                => [MOI.BASIC, MOI.BASIC]],
              (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) => [0],
              (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})    => [-1]),
         (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [2.5, 2.5], con_basis =
             [(MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) => [MOI.NONBASIC],
              (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})    => [MOI.BASIC],
              (MOI.SingleVariable, MOI.GreaterThan{Float64})                => [MOI.BASIC, MOI.BASIC]],
              (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) => [1],
              (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})    => [0]),
         (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1.0, 1.0],
                  (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) => [1],
                  (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})    => [0],
              con_basis =
             [(MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) => [MOI.NONBASIC],
              (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})    => [MOI.BASIC],
              (MOI.SingleVariable, MOI.GreaterThan{Float64})                => [MOI.BASIC, MOI.BASIC]]),
         (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [6.0, 6.0], con_basis =
             [(MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) => [MOI.BASIC],
              (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})    => [MOI.NONBASIC],
              (MOI.SingleVariable, MOI.GreaterThan{Float64})                => [MOI.BASIC, MOI.BASIC]]))
    MOIT.linear10test(bridged_mock, config_with_basis)
    MOIU.set_mock_optimize!(mock,
         (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [0.0, 0.0],
               (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) => [0],
               (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})    => [-1],
              con_basis =
             [(MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) => [MOI.BASIC],
              (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})    => [MOI.BASIC],
              (MOI.SingleVariable, MOI.GreaterThan{Float64})                => [MOI.NONBASIC, MOI.NONBASIC]],
              (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) => [0],
              (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})    => [0]))
    MOIT.linear10btest(bridged_mock, config_with_basis)

    ci = first(MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64}}()))
    vis = MOI.get(bridged_mock, MOI.ListOfVariableIndices())
    newf = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0, -1.0], vis), 0.0)
    MOI.set(bridged_mock, MOI.ConstraintFunction(), ci, newf)
    @test MOI.get(bridged_mock, MOI.ConstraintFunction(), ci) ≈ newf

    MOI.modify(bridged_mock, ci, MOI.ScalarCoefficientChange(vis[2], 1.0))
    modified_f = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(ones(2), vis), 0.0)
    @test MOI.get(bridged_mock, MOI.ConstraintFunction(), ci) ≈ modified_f

    test_delete_bridge(bridged_mock, ci, 2, ((MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}, 0),
                                            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64},    0)))
end
