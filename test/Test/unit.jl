using Test
import MathOptInterface
const MOI = MathOptInterface
const MOIT = MOI.Test
const MOIU = MOI.Utilities

@testset "Basic Constraint Tests" begin
    mock = MOIU.MockOptimizer(MOIU.Model{Float64}())
    config = MOIT.Config()
    MOIT.basic_constraint_tests(mock, config)
end

@testset "Unit Tests" begin
    # `UniversalFallback` needed for `MOI.Silent`
    mock = MOIU.MockOptimizer(MOIU.UniversalFallback(MOIU.Model{Float64}()))
    config = MOIT.Config()
    for model in [
        mock,
        MOIU.CachingOptimizer(
            MOIU.UniversalFallback(MOIU.Model{Float64}()),
            mock,
        ),
    ]
        MOIT.unittest(
            model,
            config,
            [
                "test_ObjectiveFunction_blank",
                "test_ObjectiveFunction_SingleVariable",
                "solve_affine_lessthan",
                "solve_affine_greaterthan",
                "solve_affine_equalto",
                "solve_affine_interval",
                "solve_duplicate_terms_scalar_affine",
                "solve_duplicate_terms_vector_affine",
                "test_qp_ObjectiveFunction_edge_cases",
                "test_qp_ObjectiveFunction_zero_ofdiag",
                "solve_qcp_edge_cases",
                "solve_affine_deletion_edge_cases",
                "test_ObjectiveFunction_duplicate_terms",
                "test_ObjectiveBound_edge_cases",
                "solve_zero_one_with_bounds_1",
                "solve_zero_one_with_bounds_2",
                "solve_zero_one_with_bounds_3",
                "solve_start_soc",
                "test_TerminationStatus_DUAL_INFEASIBLE",
                "test_SingleVariable_ConstraintDual_MIN_SENSE",
                "test_SingleVariable_ConstraintDual_MAX_SENSE",
                "test_result_index",
                "test_DualStatus_INFEASIBILITY_CERTIFICATE_EqualTo_lower",
                "test_DualStatus_INFEASIBILITY_CERTIFICATE_EqualTo_upper",
                "test_DualStatus_INFEASIBILITY_CERTIFICATE_LessThan",
                "test_DualStatus_INFEASIBILITY_CERTIFICATE_GreaterThan",
                "test_DualStatus_INFEASIBILITY_CERTIFICATE_Interval_lower",
                "test_DualStatus_INFEASIBILITY_CERTIFICATE_Interval_upper",
                "test_DualStatus_INFEASIBILITY_CERTIFICATE_SingleVariable_LessThan",
                "test_DualStatus_INFEASIBILITY_CERTIFICATE_SingleVariable_LessThan_max",
                "test_optimize_twice",
            ],
        )
        MOI.empty!(model)
    end
    @testset "solve_affine_lessthan" begin
        MOIU.set_mock_optimize!(
            mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.OPTIMAL,
                (MOI.FEASIBLE_POINT, [0.5]),
                MOI.FEASIBLE_POINT,
                (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) => [-0.5],
            ),
        )
        MOIT.solve_affine_lessthan(mock, config)
    end
    @testset "solve_affine_greaterthan" begin
        MOIU.set_mock_optimize!(
            mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.OPTIMAL,
                (MOI.FEASIBLE_POINT, [0.5]),
                MOI.FEASIBLE_POINT,
                (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) => [0.5],
            ),
        )
        MOIT.solve_affine_greaterthan(mock, config)
    end
    @testset "solve_affine_equalto" begin
        MOIU.set_mock_optimize!(
            mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.OPTIMAL,
                (MOI.FEASIBLE_POINT, [0.5]),
                MOI.FEASIBLE_POINT,
                (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                    [0.5],
            ),
        )
        MOIT.solve_affine_equalto(mock, config)
    end
    @testset "solve_affine_interval" begin
        MOIU.set_mock_optimize!(
            mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.OPTIMAL,
                (MOI.FEASIBLE_POINT, [2.0]),
                MOI.FEASIBLE_POINT,
                (MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64}) => [-1.5],
            ),
        )
        MOIT.solve_affine_interval(mock, config)
    end
    @testset "solve_duplicate_terms_scalar_affine" begin
        MOIU.set_mock_optimize!(
            mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.OPTIMAL,
                (MOI.FEASIBLE_POINT, [0.5]),
                MOI.FEASIBLE_POINT,
                (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) => [-0.5],
            ),
        )
        MOIT.solve_duplicate_terms_scalar_affine(mock, config)
    end
    @testset "solve_duplicate_terms_vector_affine" begin
        MOIU.set_mock_optimize!(
            mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.OPTIMAL,
                (MOI.FEASIBLE_POINT, [0.5]),
                MOI.FEASIBLE_POINT,
                (MOI.VectorAffineFunction{Float64}, MOI.Nonpositives) =>
                    [[-0.5]],
            ),
        )
        MOIT.solve_duplicate_terms_vector_affine(mock, config)
    end

    @testset "solve_qcp_edge_cases" begin
        MOIU.set_mock_optimize!(
            mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.OPTIMAL,
                (MOI.FEASIBLE_POINT, [0.5, 0.5]),
            ),
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.OPTIMAL,
                (MOI.FEASIBLE_POINT, [0.5, (√13 - 1) / 4]),
            ),
        )
        MOIT.solve_qcp_edge_cases(mock, config)
    end
    @testset "solve_affine_deletion_edge_cases" begin
        MOIU.set_mock_optimize!(
            mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.OPTIMAL,
                (MOI.FEASIBLE_POINT, [0.0]),
            ),
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.OPTIMAL,
                (MOI.FEASIBLE_POINT, [0.0]),
            ),
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.OPTIMAL,
                (MOI.FEASIBLE_POINT, [1.0]),
            ),
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.OPTIMAL,
                (MOI.FEASIBLE_POINT, [1.0]),
            ),
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.OPTIMAL,
                (MOI.FEASIBLE_POINT, [2.0]),
            ),
        )
        MOIT.solve_affine_deletion_edge_cases(mock, config)
    end
    @testset "solve_zero_one_with_bounds" begin
        MOIU.set_mock_optimize!(
            mock,
            (mock::MOIU.MockOptimizer) -> begin
                MOIU.mock_optimize!(mock, MOI.OPTIMAL, (MOI.FEASIBLE_POINT, [1.0]))
            end,
            (mock::MOIU.MockOptimizer) -> begin
                MOIU.mock_optimize!(mock, MOI.OPTIMAL, (MOI.FEASIBLE_POINT, [0.0]))
            end,
            (mock::MOIU.MockOptimizer) -> begin
                MOIU.mock_optimize!(mock, MOI.INFEASIBLE)
            end,
        )
        MOIT.solve_zero_one_with_bounds_1(mock, config)
        MOIT.solve_zero_one_with_bounds_2(mock, config)
        MOIT.solve_zero_one_with_bounds_3(mock, config)
    end

    @testset "solve_start_soc" begin
        MOIU.set_mock_optimize!(
            mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.OPTIMAL,
                (MOI.FEASIBLE_POINT, [1.0]),
                MOI.FEASIBLE_POINT,
                (MOI.VectorAffineFunction{Float64}, MOI.SecondOrderCone) =>
                    [[1.0, -1.0]],
            ),
        )
        MOIT.solve_start_soc(mock, config)
    end
end
