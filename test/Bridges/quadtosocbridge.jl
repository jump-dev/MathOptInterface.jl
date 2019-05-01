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

@testset "QuadtoSOC" begin
    bridged_mock = MOIB.QuadtoSOC{Float64}(mock)
    @testset "Error for non-convex quadratic constraints" begin
        x = MOI.add_variable(bridged_mock)
        @test_throws ErrorException begin
            MOI.add_constraint(bridged_mock,
                               MOI.ScalarQuadraticFunction(MOI.ScalarAffineTerm{Float64}[],
                                                           [MOI.ScalarQuadraticTerm(1.0, x, x)],
                                                           0.0),
                               MOI.GreaterThan(0.0))
        end
        @test_throws ErrorException begin
            MOI.add_constraint(bridged_mock,
                               MOI.ScalarQuadraticFunction(MOI.ScalarAffineTerm{Float64}[],
                                                           [MOI.ScalarQuadraticTerm(-1.0, x, x)],
                                                           0.0),
                               MOI.LessThan(0.0))
        end
    end
    @testset "Quadratic constraints with 2 variables" begin
        MOIU.set_mock_optimize!(mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock,
                MOI.OPTIMAL,
                (MOI.FEASIBLE_POINT, [0.5, 0.5])
            ),
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock,
                MOI.OPTIMAL,
                (MOI.FEASIBLE_POINT, [0.5, (√13 - 1)/4])
            )
        )
        MOIT.solve_qcp_edge_cases(bridged_mock, config)
        ci = first(MOI.get(mock,
                           MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64},
                                                       MOI.RotatedSecondOrderCone}()))
        x, y = MOI.get(mock, MOI.ListOfVariableIndices())
        # The matrix is
        # 2 1
        # 1 2
        # for which the cholesky factorization is U' * U with U =
        # √2 √2/2
        #  . √3/√2
        expected = MOI.VectorAffineFunction{Float64}(MOI.VectorAffineTerm.([3, 3, 4],
                                                                           MOI.ScalarAffineTerm.([√2, √2/2, √3/√2],
                                                                                                 [x, y, y])),
                                                     [1.0, 1.0, 0.0, 0.0])
        @test MOI.get(mock, MOI.ConstraintFunction(), ci) ≈ expected
    end
    @testset "QCP tests" begin
        MOIU.set_mock_optimize!(mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1/2, 7/4], MOI.FEASIBLE_POINT))
        MOIT.qcp1test(bridged_mock, config)
        MOIU.set_mock_optimize!(mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [√2], MOI.FEASIBLE_POINT))
        MOIT.qcp2test(bridged_mock, config)
        MOIU.set_mock_optimize!(mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [√2], MOI.FEASIBLE_POINT))
        MOIT.qcp3test(bridged_mock, config)
        @testset "Bridge deletion" begin
            ci = first(MOI.get(bridged_mock,
                               MOI.ListOfConstraintIndices{MOI.ScalarQuadraticFunction{Float64},
                                                           MOI.LessThan{Float64}}()))
            test_delete_bridge(bridged_mock, ci, 1, ((MOI.VectorAffineFunction{Float64}, MOI.RotatedSecondOrderCone, 0),))
        end
        MOIU.set_mock_optimize!(mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1.0, 1.0], MOI.FEASIBLE_POINT))
        MOIT.qcp4test(bridged_mock, config)
        MOIT.qcp5test(bridged_mock, config)
    end
end
