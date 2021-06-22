using Test
import MathOptInterface

const MOI = MathOptInterface
const MOIU = MOI.Utilities

function MOI.Test.setup_test(
    ::typeof(MOI.Test.test_linear_integration),
    mock::MOIU.MockOptimizer,
    ::MOI.Test.Config,
)
    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [1, 0],
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) =>
                [-1],
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [1, 0],
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) =>
                [-1],
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [0, 0, 1],
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) =>
                [-2],
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [-1, 0, 2]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1, 0, 0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [2, 0, 0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [0, 2, 0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [1, 1, 0],
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                [-1.5],
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) =>
                [0.5],
        ),
    )
    return
end

function MOI.Test.setup_test(
    ::typeof(MOI.Test.test_linear_integration_2),
    mock::MOIU.MockOptimizer,
    ::MOI.Test.Config,
)
    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [1, 0],
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) =>
                [-1],
            con_basis = [
                (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) => [MOI.NONBASIC],
            ],
            var_basis = [MOI.BASIC, MOI.NONBASIC_AT_LOWER],
        ),
    )
    return
end

function MOI.Test.setup_test(
    ::typeof(MOI.Test.test_linear_inactive_bounds),
    mock::MOIU.MockOptimizer,
    ::MOI.Test.Config,
)
    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [3],
            con_basis = [
                (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) => [MOI.NONBASIC],
            ],
            var_basis = [MOI.BASIC],
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [0],
            con_basis = [
                (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) => [MOI.BASIC],
            ],
            var_basis = [MOI.NONBASIC_AT_UPPER],
        ),
    )
    return
end

function MOI.Test.setup_test(
    ::typeof(MOI.Test.test_linear_LessThan_and_GreaterThan),
    mock::MOIU.MockOptimizer,
    ::MOI.Test.Config,
)
    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [0, 0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [100, 0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [100, -100]),
    )
    return
end

function MOI.Test.setup_test(
    ::typeof(MOI.Test.test_linear_integration_modification),
    mock::MOIU.MockOptimizer,
    ::MOI.Test.Config,
)
    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [4 / 3, 4 / 3]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [2, 0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [4, 0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [2]),
    )
    return
end

function MOI.Test.setup_test(
    ::typeof(MOI.Test.test_linear_modify_GreaterThan_and_LessThan_constraints),
    mock::MOIU.MockOptimizer,
    ::MOI.Test.Config,
)
    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [0, 0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [100, 0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [100, -100]),
    )
    return
end

function MOI.Test.setup_test(
    ::typeof(MOI.Test.test_linear_VectorAffineFunction),
    mock::MOIU.MockOptimizer,
    ::MOI.Test.Config,
)
    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [0, 0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [100, 0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [100, -100]),
    )
    return
end

function MOI.Test.setup_test(
    ::typeof(MOI.Test.test_linear_INFEASIBLE),
    mock::MOIU.MockOptimizer,
    config::MOI.Test.Config,
)
    if config.infeas_certificates
        MOIU.set_mock_optimize!(
            mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.INFEASIBLE,
                tuple(),
                (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) => [-1],
            ),
        )
    else
        MOIU.set_mock_optimize!(
            mock,
            (mock::MOIU.MockOptimizer) ->
                MOIU.mock_optimize!(mock, MOI.INFEASIBLE),
        )
    end
    return
end

function MOI.Test.setup_test(
    ::typeof(MOI.Test.test_linear_DUAL_INFEASIBLE),
    mock::MOIU.MockOptimizer,
    config::MOI.Test.Config,
)
    primal_status = if config.infeas_certificates
        MOI.INFEASIBILITY_CERTIFICATE
    else
        MOI.NO_SOLUTION
    end
    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) ->
            MOIU.mock_optimize!(mock, MOI.DUAL_INFEASIBLE, primal_status),
    )
    return
end

function MOI.Test.setup_test(
    ::typeof(MOI.Test.test_linear_DUAL_INFEASIBLE_2),
    mock::MOIU.MockOptimizer,
    config::MOI.Test.Config,
)
    if config.infeas_certificates
        MOIU.set_mock_optimize!(
            mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.DUAL_INFEASIBLE,
                (MOI.INFEASIBILITY_CERTIFICATE, [1, 1]),
            ),
        )
    else
        MOIU.set_mock_optimize!(
            mock,
            (mock::MOIU.MockOptimizer) ->
                MOIU.mock_optimize!(mock, MOI.DUAL_INFEASIBLE),
        )
    end
    return
end

function MOI.Test.setup_test(
    ::typeof(MOI.Test.test_linear_add_constraints),
    mock::MOIU.MockOptimizer,
    ::MOI.Test.Config,
)
    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [650 / 11, 400 / 11],
            con_basis = [
                (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) => [MOI.NONBASIC, MOI.NONBASIC],
                (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) => [MOI.BASIC],
            ],
            var_basis = [MOI.BASIC, MOI.BASIC],
        ),
    )
    return
end

function MOI.Test.setup_test(
    ::typeof(MOI.Test.test_linear_integration_Interval),
    mock::MOIU.MockOptimizer,
    ::MOI.Test.Config,
)
    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [5.0, 5.0],
            con_basis = [
                (MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64}) => [MOI.NONBASIC_AT_UPPER],
            ],
            var_basis = [MOI.BASIC, MOI.BASIC],
            (MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64}) =>
                [-1],
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [2.5, 2.5],
            con_basis = [
                (MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64}) => [MOI.NONBASIC_AT_LOWER],
            ],
            var_basis = [MOI.BASIC, MOI.BASIC],
            (MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64}) =>
                [1],
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [1.0, 1.0],
            (MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64}) =>
                [1],
            con_basis = [
                (MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64}) => [MOI.NONBASIC_AT_LOWER],
            ],
            var_basis = [MOI.BASIC, MOI.BASIC],
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [6.0, 6.0],
            (MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64}) =>
                [-1],
            con_basis = [
                (MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64}) => [MOI.NONBASIC_AT_UPPER],
            ],
            var_basis = [MOI.BASIC, MOI.BASIC],
        ),
    )
    return
end

function MOI.Test.setup_test(
    ::typeof(MOI.Test.test_linear_Interval_inactive),
    mock::MOIU.MockOptimizer,
    ::MOI.Test.Config,
)
    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [0.0, 0.0],
            con_basis = [
                (MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64}) => [MOI.BASIC],
            ],
            var_basis = [MOI.NONBASIC_AT_LOWER, MOI.NONBASIC_AT_LOWER],
            (MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64}) =>
                [0],
        ),
    )
    return
end

function MOI.Test.setup_test(
    ::typeof(MOI.Test.test_linear_transform),
    mock::MOIU.MockOptimizer,
    ::MOI.Test.Config,
)
    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1.0, 1.0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [0.5, 0.5]),
    )
    return
end

function MOI.Test.setup_test(
    ::typeof(MOI.Test.test_linear_INFEASIBLE_2),
    mock::MOIU.MockOptimizer,
    config::MOI.Test.Config,
)
    if config.infeas_certificates
        MOIU.set_mock_optimize!(
            mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock,
                MOI.INFEASIBLE,
                tuple(),
                (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) => [-1, -1],
            ),
        )
    else
        MOIU.set_mock_optimize!(
            mock,
            (mock::MOIU.MockOptimizer) ->
                MOIU.mock_optimize!(mock, MOI.INFEASIBLE),
        )
    end
    return
end

function MOI.Test.setup_test(
    ::typeof(MOI.Test.test_linear_FEASIBILITY_SENSE),
    mock::MOIU.MockOptimizer,
    ::MOI.Test.Config,
)
    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [1 / 5, 1 / 5],
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) =>
                [0],
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                [0],
        ),
    )
    return
end

function MOI.Test.setup_test(
    ::typeof(MOI.Test.test_linear_integration_delete_variables),
    mock::MOIU.MockOptimizer,
    ::MOI.Test.Config,
)
    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [0, 1 / 2, 1],
            con_basis = [
                (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) => [MOI.NONBASIC],
            ],
            var_basis = [
                MOI.NONBASIC_AT_LOWER,
                MOI.BASIC,
                MOI.NONBASIC_AT_UPPER,
            ],
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) =>
                [-1],
            (MOI.SingleVariable, MOI.GreaterThan{Float64}) => [2, 0, 0],
            (MOI.SingleVariable, MOI.LessThan{Float64}) => [-2],
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [1],
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) =>
                [-1],
            (MOI.SingleVariable, MOI.GreaterThan{Float64}) => [0],
        ),
    )
    # test_linear_integration_delete_variables has double variable bounds for
    # the z variable
    mock.eval_variable_constraint_dual = false
    function reset_function()
        mock.eval_variable_constraint_dual = true
        return
    end
    return reset_function
end

function MOI.Test.setup_test(
    ::typeof(MOI.Test.test_linear_VectorAffineFunction_empty_row),
    mock::MOIU.MockOptimizer,
    ::MOI.Test.Config,
)
    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [0.0],
            (MOI.VectorAffineFunction{Float64}, MOI.Zeros) => [[0.0, 0.0]],
        ),
    )
    return
end

function MOI.Test.setup_test(
    ::typeof(MOI.Test.test_linear_VariablePrimalStart_partial),
    mock::MOIU.MockOptimizer,
    ::MOI.Test.Config,
)
    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1.0, 0.0]),
    )
    return
end

MOI.Test.runtests(
    MOIU.MockOptimizer(MOIU.UniversalFallback(MOIU.Model{Float64}())),
    MOI.Test.Config(basis = true),
    include = ["test_linear_"],
    # Oops! Name clash.
    exclude = ["test_linear_mixed_complementarity"],
)

MOI.Test.runtests(
    MOIU.MockOptimizer(MOIU.UniversalFallback(MOIU.Model{Float64}())),
    MOI.Test.Config(modify_lhs = false),
    include = ["test_linear_"],
    # Oops! Name clash.
    exclude = ["test_linear_mixed_complementarity"],
)
