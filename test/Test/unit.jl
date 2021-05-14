using Test
import MathOptInterface
const MOI = MathOptInterface
const MOIT = MOI.Test
const MOIU = MOI.Utilities

@testset "Basic Constraint Tests" begin
    mock = MOIU.MockOptimizer(MOIU.Model{Float64}())
    config = MOIT.TestConfig()
    MOIT.basic_constraint_tests(mock, config)
end

@testset "Unit Tests" begin
    # `UniversalFallback` needed for `MOI.Silent`
    mock = MOIU.MockOptimizer(MOIU.UniversalFallback(MOIU.Model{Float64}()))
    # Optimizers attributes have to be set to default value since the mock
    # optimizer doesn't handle this
    MOI.set(mock, MOI.Silent(), true)
    MOI.set(mock, MOI.TimeLimitSec(), nothing)
    MOI.set(mock, MOI.NumberOfThreads(), nothing)
    config = MOIT.TestConfig()
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
                "solve_blank_obj",
                "solve_constant_obj",
                "solve_singlevariable_obj",
                "solve_with_lowerbound",
                "solve_with_upperbound",
                "solve_affine_lessthan",
                "solve_affine_greaterthan",
                "solve_affine_equalto",
                "solve_affine_interval",
                "solve_duplicate_terms_scalar_affine",
                "solve_duplicate_terms_vector_affine",
                "solve_qp_edge_cases",
                "solve_qp_zero_offdiag",
                "solve_qcp_edge_cases",
                "solve_affine_deletion_edge_cases",
                "solve_duplicate_terms_obj",
                "solve_integer_edge_cases",
                "solve_objbound_edge_cases",
                "raw_status_string",
                "solve_time",
                "solve_zero_one_with_bounds_1",
                "solve_zero_one_with_bounds_2",
                "solve_zero_one_with_bounds_3",
                "solve_unbounded_model",
                "solve_single_variable_dual_min",
                "solve_single_variable_dual_max",
                "solve_result_index",
                "solve_farkas_equalto_lower",
                "solve_farkas_equalto_upper",
                "solve_farkas_lessthan",
                "solve_farkas_greaterthan",
                "solve_farkas_interval_lower",
                "solve_farkas_interval_upper",
                "solve_farkas_variable_lessthan",
                "solve_farkas_variable_lessthan_max",
                "solve_twice",
                "solve_basis_lower_bound",
                "solve_basis_superbasic",
            ],
        )
        MOI.empty!(model)
    end

    @testset "solve_blank_obj" begin
        MOIU.set_mock_optimize!(
            mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.OPTIMAL,
                (MOI.FEASIBLE_POINT, [1]),
                MOI.FEASIBLE_POINT,
            ),
        )
        MOIT.solve_blank_obj(mock, config)
        # The objective is blank so any primal value ≥ 1 is correct
        MOIU.set_mock_optimize!(
            mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.OPTIMAL,
                (MOI.FEASIBLE_POINT, [2]),
                MOI.FEASIBLE_POINT,
            ),
        )
        MOIT.solve_blank_obj(mock, config)
    end
    @testset "solve_constant_obj" begin
        MOIU.set_mock_optimize!(
            mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.OPTIMAL,
                (MOI.FEASIBLE_POINT, [1]),
                MOI.FEASIBLE_POINT,
            ),
        )
        MOIT.solve_constant_obj(mock, config)
    end
    @testset "solve_singlevariable_obj" begin
        MOIU.set_mock_optimize!(
            mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.OPTIMAL,
                (MOI.FEASIBLE_POINT, [1]),
                MOI.FEASIBLE_POINT,
            ),
        )
        MOIT.solve_singlevariable_obj(mock, config)
    end
    @testset "solve_with_lowerbound" begin
        MOIU.set_mock_optimize!(
            mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.OPTIMAL,
                (MOI.FEASIBLE_POINT, [1]),
                MOI.FEASIBLE_POINT,
                (MOI.SingleVariable, MOI.GreaterThan{Float64}) => [2.0],
                (MOI.SingleVariable, MOI.LessThan{Float64}) => [0.0],
            ),
        )
        # x has two variable constraints
        mock.eval_variable_constraint_dual = false
        MOIT.solve_with_lowerbound(mock, config)
        mock.eval_variable_constraint_dual = true
    end
    @testset "solve_with_upperbound" begin
        MOIU.set_mock_optimize!(
            mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.OPTIMAL,
                (MOI.FEASIBLE_POINT, [1]),
                MOI.FEASIBLE_POINT,
                (MOI.SingleVariable, MOI.LessThan{Float64}) => [-2.0],
                (MOI.SingleVariable, MOI.GreaterThan{Float64}) => [0.0],
            ),
        )
        # x has two variable constraints
        mock.eval_variable_constraint_dual = false
        MOIT.solve_with_upperbound(mock, config)
        mock.eval_variable_constraint_dual = true
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

    @testset "solve_qp_edge_cases" begin
        MOIU.set_mock_optimize!(
            mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.OPTIMAL,
                (MOI.FEASIBLE_POINT, [1.0, 2.0]),
            ),
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.OPTIMAL,
                (MOI.FEASIBLE_POINT, [1.0, 2.0]),
            ),
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.OPTIMAL,
                (MOI.FEASIBLE_POINT, [1.0, 2.0]),
            ),
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.OPTIMAL,
                (MOI.FEASIBLE_POINT, [1.0, 2.0]),
            ),
        )
        MOIT.solve_qp_edge_cases(mock, config)
    end
    @testset "solve_qp_zero_offdiag" begin
        MOIU.set_mock_optimize!(
            mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.OPTIMAL,
                (MOI.FEASIBLE_POINT, [1.0, 2.0]),
            ),
        )
        MOIT.solve_qp_zero_offdiag(mock, config)
    end
    @testset "solve_duplicate_terms_obj" begin
        MOIU.set_mock_optimize!(
            mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.OPTIMAL,
                (MOI.FEASIBLE_POINT, [1]),
                MOI.FEASIBLE_POINT,
            ),
        )
        MOIT.solve_duplicate_terms_obj(mock, config)
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
    @testset "solve_integer_edge_cases" begin
        MOIU.set_mock_optimize!(
            mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.OPTIMAL,
                (MOI.FEASIBLE_POINT, [2.0]),
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
                (MOI.FEASIBLE_POINT, [0.0]),
            ),
        )
        MOIT.solve_integer_edge_cases(mock, config)
    end
    @testset "solve_objbound_edge_cases" begin
        MOIU.set_mock_optimize!(
            mock,
            (mock::MOIU.MockOptimizer) -> begin
                MOI.set(mock, MOI.ObjectiveBound(), 3.0)
                MOIU.mock_optimize!(
                    mock,
                    MOI.OPTIMAL,
                    (MOI.FEASIBLE_POINT, [2.0]),
                )
            end,
            (mock::MOIU.MockOptimizer) -> begin
                MOI.set(mock, MOI.ObjectiveBound(), 3.0)
                MOIU.mock_optimize!(
                    mock,
                    MOI.OPTIMAL,
                    (MOI.FEASIBLE_POINT, [1.0]),
                )
            end,
            (mock::MOIU.MockOptimizer) -> begin
                MOI.set(mock, MOI.ObjectiveBound(), 2.0)
                MOIU.mock_optimize!(
                    mock,
                    MOI.OPTIMAL,
                    (MOI.FEASIBLE_POINT, [1.5]),
                )
            end,
            (mock::MOIU.MockOptimizer) -> begin
                MOI.set(mock, MOI.ObjectiveBound(), 4.0)
                MOIU.mock_optimize!(
                    mock,
                    MOI.OPTIMAL,
                    (MOI.FEASIBLE_POINT, [1.5]),
                )
            end,
        )
        MOIT.solve_objbound_edge_cases(mock, config)
    end

    @testset "raw_status_string" begin
        MOIU.set_mock_optimize!(
            mock,
            (mock::MOIU.MockOptimizer) -> begin
                MOI.set(
                    mock,
                    MOI.RawStatusString(),
                    "Mock solution set by `mock_optimize!`.",
                )
                MOIU.mock_optimize!(
                    mock,
                    MOI.OPTIMAL,
                    (MOI.FEASIBLE_POINT, [0.0]),
                )
            end,
        )
        MOIT.raw_status_string(mock, config)
    end

    @testset "solve_time" begin
        MOIU.set_mock_optimize!(
            mock,
            (mock::MOIU.MockOptimizer) -> begin
                MOI.set(mock, MOI.SolveTimeSec(), 0.0)
                MOIU.mock_optimize!(
                    mock,
                    MOI.OPTIMAL,
                    (MOI.FEASIBLE_POINT, [0.0]),
                )
            end,
        )
        MOIT.solve_time(mock, config)
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

    @testset "solve_unbounded_model" begin
        MOIU.set_mock_optimize!(
            mock,
            (mock::MOIU.MockOptimizer) -> begin
                MOIU.mock_optimize!(mock, MOI.DUAL_INFEASIBLE)
            end,
        )
        MOIT.solve_unbounded_model(mock, config)
    end

    @testset "solve_single_variable_dual_min" begin
        flag = mock.eval_variable_constraint_dual
        mock.eval_variable_constraint_dual = false
        MOIU.set_mock_optimize!(
            mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.OPTIMAL,
                (MOI.FEASIBLE_POINT, [1.0]),
                MOI.FEASIBLE_POINT,
                (MOI.SingleVariable, MOI.GreaterThan{Float64}) => [1.0],
                (MOI.SingleVariable, MOI.LessThan{Float64}) => [0.0],
            ),
        )
        MOIT.solve_single_variable_dual_min(mock, config)
        mock.eval_variable_constraint_dual = flag
    end

    @testset "solve_single_variable_dual_max" begin
        flag = mock.eval_variable_constraint_dual
        mock.eval_variable_constraint_dual = false
        MOIU.set_mock_optimize!(
            mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.OPTIMAL,
                (MOI.FEASIBLE_POINT, [1.0]),
                MOI.FEASIBLE_POINT,
                (MOI.SingleVariable, MOI.GreaterThan{Float64}) => [0.0],
                (MOI.SingleVariable, MOI.LessThan{Float64}) => [-1.0],
            ),
        )
        MOIT.solve_single_variable_dual_max(mock, config)
        mock.eval_variable_constraint_dual = flag
    end

    @testset "solve_result_index" begin
        MOIU.set_mock_optimize!(
            mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.OPTIMAL,
                (MOI.FEASIBLE_POINT, [1.0]),
                MOI.FEASIBLE_POINT,
                (MOI.SingleVariable, MOI.GreaterThan{Float64}) => [1.0],
            ),
        )
        MOIT.solve_result_index(mock, config)
    end

    @testset "solve_farkas_equalto_upper" begin
        MOIU.set_mock_optimize!(
            mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.INFEASIBLE,
                (MOI.NO_SOLUTION, [NaN, NaN]),
                MOI.INFEASIBILITY_CERTIFICATE,
                (MOI.SingleVariable, MOI.GreaterThan{Float64}) =>
                    [2.0, 1.0],
                (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                    [-1.0],
            ),
        )
        MOIT.solve_farkas_equalto_upper(mock, config)
    end

    @testset "solve_farkas_equalto_lower" begin
        MOIU.set_mock_optimize!(
            mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.INFEASIBLE,
                (MOI.NO_SOLUTION, [NaN, NaN]),
                MOI.INFEASIBILITY_CERTIFICATE,
                (MOI.SingleVariable, MOI.GreaterThan{Float64}) =>
                    [2.0, 1.0],
                (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                    [1.0],
            ),
        )
        MOIT.solve_farkas_equalto_lower(mock, config)
    end

    @testset "solve_farkas_lessthan" begin
        MOIU.set_mock_optimize!(
            mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.INFEASIBLE,
                (MOI.NO_SOLUTION, [NaN, NaN]),
                MOI.INFEASIBILITY_CERTIFICATE,
                (MOI.SingleVariable, MOI.GreaterThan{Float64}) =>
                    [2.0, 1.0],
                (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) => [-1.0],
            ),
        )
        MOIT.solve_farkas_lessthan(mock, config)
    end

    @testset "solve_farkas_greaterthan" begin
        MOIU.set_mock_optimize!(
            mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.INFEASIBLE,
                (MOI.NO_SOLUTION, [NaN, NaN]),
                MOI.INFEASIBILITY_CERTIFICATE,
                (MOI.SingleVariable, MOI.GreaterThan{Float64}) =>
                    [2.0, 1.0],
                (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) => [1.0],
            ),
        )
        MOIT.solve_farkas_greaterthan(mock, config)
    end

    @testset "solve_farkas_interval_upper" begin
        MOIU.set_mock_optimize!(
            mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.INFEASIBLE,
                (MOI.NO_SOLUTION, [NaN, NaN]),
                MOI.INFEASIBILITY_CERTIFICATE,
                (MOI.SingleVariable, MOI.GreaterThan{Float64}) =>
                    [2.0, 1.0],
                (MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64}) => [-1.0],
            ),
        )
        MOIT.solve_farkas_interval_upper(mock, config)
    end

    @testset "solve_farkas_interval_lower" begin
        MOIU.set_mock_optimize!(
            mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.INFEASIBLE,
                (MOI.NO_SOLUTION, [NaN, NaN]),
                MOI.INFEASIBILITY_CERTIFICATE,
                (MOI.SingleVariable, MOI.GreaterThan{Float64}) =>
                    [2.0, 1.0],
                (MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64}) => [1.0],
            ),
        )
        MOIT.solve_farkas_interval_lower(mock, config)
    end

    @testset "solve_farkas_variable_lessthan" begin
        MOIU.set_mock_optimize!(
            mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.INFEASIBLE,
                (MOI.NO_SOLUTION, [NaN, NaN]),
                MOI.INFEASIBILITY_CERTIFICATE,
                (MOI.SingleVariable, MOI.LessThan{Float64}) => [-2.0, -1.0],
                (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) => [1.0],
            ),
        )
        MOIT.solve_farkas_variable_lessthan(mock, config)
    end

    @testset "solve_farkas_variable_lessthan_max" begin
        MOIU.set_mock_optimize!(
            mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.INFEASIBLE,
                (MOI.NO_SOLUTION, [NaN, NaN]),
                MOI.INFEASIBILITY_CERTIFICATE,
                (MOI.SingleVariable, MOI.LessThan{Float64}) => [-2.0, -1.0],
                (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) => [1.0],
            ),
        )
        MOIT.solve_farkas_variable_lessthan_max(mock, config)
    end

    @testset "solve_twice" begin
        MOIU.set_mock_optimize!(
            mock,
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
        )
        MOIT.solve_twice(mock, config)
    end
    @testset "solve_basis_lower_bound" begin
        MOIU.set_mock_optimize!(
            mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.OPTIMAL,
                (MOI.FEASIBLE_POINT, [1.0, 0.0]);
                con_basis = [
                    (MOI.SingleVariable, MOI.GreaterThan{Float64}) =>
                        [MOI.NONBASIC],
                    (MOI.SingleVariable, MOI.Reals) => [MOI.SUPER_BASIC],
                ],
            ),
        )
        MOIT.solve_basis_lower_bound(mock, MOI.Test.TestConfig(basis = true))
    end
    @testset "solve_basis_superbasic" begin
        MOIU.set_mock_optimize!(
            mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.OPTIMAL,
                (MOI.FEASIBLE_POINT, [4.0, 0.0]);
                con_basis = [
                    (
                        MOI.ScalarAffineFunction{Float64},
                        MOI.LessThan{Float64},
                    ) => [MOI.NONBASIC],
                    (MOI.SingleVariable, MOI.Reals) =>
                        [MOI.BASIC, MOI.SUPER_BASIC],
                ],
            ),
        )
        MOIT.solve_basis_superbasic(mock, MOI.Test.TestConfig(basis = true))
    end
end

@testset "modifications" begin
    # `UniversalFallback` needed for `MOI.Silent`
    mock = MOIU.MockOptimizer(MOIU.UniversalFallback(MOIU.Model{Float64}()))
    config = MOIT.TestConfig()
    @testset "delete_variables_in_a_batch" begin
        MOIU.set_mock_optimize!(
            mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.OPTIMAL,
                (MOI.FEASIBLE_POINT, [1.0, 1.0, 1.0]),
                MOI.FEASIBLE_POINT,
            ),
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.OPTIMAL,
                (MOI.FEASIBLE_POINT, [1.0]),
                MOI.FEASIBLE_POINT,
            ),
        )
        MOIT.delete_variables_in_a_batch(mock, config)
    end
    @testset "solve_set_singlevariable_lessthan" begin
        MOIU.set_mock_optimize!(
            mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.OPTIMAL,
                (MOI.FEASIBLE_POINT, [1.0]),
                MOI.FEASIBLE_POINT,
            ),
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.OPTIMAL,
                (MOI.FEASIBLE_POINT, [2.0]),
                MOI.FEASIBLE_POINT,
            ),
        )
        MOIT.solve_set_singlevariable_lessthan(mock, config)
    end
    @testset "solve_transform_singlevariable_lessthan" begin
        MOIU.set_mock_optimize!(
            mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.OPTIMAL,
                (MOI.FEASIBLE_POINT, [1.0]),
                MOI.FEASIBLE_POINT,
            ),
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.OPTIMAL,
                (MOI.FEASIBLE_POINT, [2.0]),
                MOI.FEASIBLE_POINT,
            ),
        )
        MOIT.solve_transform_singlevariable_lessthan(mock, config)
    end
    @testset "solve_set_scalaraffine_lessthan" begin
        MOIU.set_mock_optimize!(
            mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.OPTIMAL,
                (MOI.FEASIBLE_POINT, [1.0]),
                MOI.FEASIBLE_POINT,
                (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) => [-1.0],
            ),
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.OPTIMAL,
                (MOI.FEASIBLE_POINT, [2.0]),
                MOI.FEASIBLE_POINT,
                (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) => [-1.0],
            ),
        )
        MOIT.solve_set_scalaraffine_lessthan(mock, config)
    end
    @testset "solve_coef_scalaraffine_lessthan" begin
        MOIU.set_mock_optimize!(
            mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.OPTIMAL,
                (MOI.FEASIBLE_POINT, [1.0]),
                MOI.FEASIBLE_POINT,
                (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) => [-1.0],
            ),
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.OPTIMAL,
                (MOI.FEASIBLE_POINT, [0.5]),
                MOI.FEASIBLE_POINT,
                (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) => [-0.5],
            ),
        )
        MOIT.solve_coef_scalaraffine_lessthan(mock, config)
    end
    @testset "solve_func_scalaraffine_lessthan" begin
        MOIU.set_mock_optimize!(
            mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.OPTIMAL,
                (MOI.FEASIBLE_POINT, [1.0]),
                MOI.FEASIBLE_POINT,
                (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) => [-1.0],
            ),
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.OPTIMAL,
                (MOI.FEASIBLE_POINT, [0.5]),
                MOI.FEASIBLE_POINT,
                (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) => [-0.5],
            ),
        )
        MOIT.solve_func_scalaraffine_lessthan(mock, config)
    end
    @testset "solve_const_vectoraffine_nonpos" begin
        MOIU.set_mock_optimize!(
            mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.OPTIMAL,
                (MOI.FEASIBLE_POINT, [0.0, 0.0]),
            ),
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.OPTIMAL,
                (MOI.FEASIBLE_POINT, [1.0, 0.75]),
            ),
        )
        MOIT.solve_const_vectoraffine_nonpos(mock, config)
    end
    @testset "solve_multirow_vectoraffine_nonpos" begin
        MOIU.set_mock_optimize!(
            mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.OPTIMAL,
                (MOI.FEASIBLE_POINT, [0.5]),
            ),
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.OPTIMAL,
                (MOI.FEASIBLE_POINT, [0.25]),
            ),
        )
        MOIT.solve_multirow_vectoraffine_nonpos(mock, config)
    end
    @testset "solve_const_scalar_objective" begin
        MOIU.set_mock_optimize!(
            mock,
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
        )
        MOIT.solve_const_scalar_objective(mock, config)
    end
    @testset "solve_coef_scalar_objective" begin
        MOIU.set_mock_optimize!(
            mock,
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
        )
        MOIT.solve_coef_scalar_objective(mock, config)
    end
    @testset "delete_variable_with_single_variable_obj" begin
        MOIU.set_mock_optimize!(
            mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.OPTIMAL,
                (MOI.FEASIBLE_POINT, [1.0]),
                MOI.FEASIBLE_POINT,
                (MOI.SingleVariable, MOI.GreaterThan{Float64}) => [1.0],
            ),
        )
        MOIT.delete_variable_with_single_variable_obj(mock, config)
    end
end
