using Test
import MathOptInterface
const MOI = MathOptInterface
const MOIT = MOI.DeprecatedTest
const MOIU = MOI.Utilities

mock = MOIU.MockOptimizer(MOIU.UniversalFallback(MOIU.Model{Float64}()))
config = MOIT.Config(basis = true)
config_no_lhs_modif = MOIT.Config(modify_lhs = false)

function set_mock_optimize_linear1Test!(mock)
    return MOIU.set_mock_optimize!(
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
end
set_mock_optimize_linear1Test!(mock)
MOIT.linear1test(mock, config)
set_mock_optimize_linear1Test!(mock)
MOIT.linear1test(mock, config_no_lhs_modif)
MOIU.set_mock_optimize!(
    mock,
    (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
        mock,
        [1, 0],
        (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) => [-1],
        constraint_basis_status = [
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) =>
                [MOI.NONBASIC],
        ],
        variable_basis_status = [MOI.BASIC, MOI.NONBASIC_AT_LOWER],
    ),
)
MOIT.linear2test(mock, config)
MOIU.set_mock_optimize!(
    mock,
    (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
        mock,
        [3],
        constraint_basis_status = [
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) =>
                [MOI.NONBASIC],
        ],
        variable_basis_status = [MOI.BASIC],
    ),
    (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
        mock,
        [0],
        constraint_basis_status = [
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) =>
                [MOI.BASIC],
        ],
        variable_basis_status = [MOI.NONBASIC_AT_UPPER],
    ),
)
MOIT.linear3test(mock, config)
MOIU.set_mock_optimize!(
    mock,
    (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [0, 0]),
    (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [100, 0]),
    (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [100, -100]),
)
MOIT.linear4test(mock, config)
function set_mock_optimize_linear5Test!(mock)
    return MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [4 / 3, 4 / 3]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [2, 0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [4, 0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [2]),
    )
end
set_mock_optimize_linear5Test!(mock)
MOIT.linear5test(mock, config)
set_mock_optimize_linear5Test!(mock)
MOIT.linear5test(mock, config_no_lhs_modif)
MOIU.set_mock_optimize!(
    mock,
    (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [0, 0]),
    (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [100, 0]),
    (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [100, -100]),
)
MOIT.linear6test(mock, config)
function set_mock_optimize_linear7Test!(mock)
    return MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [0, 0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [100, 0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [100, -100]),
    )
end
set_mock_optimize_linear7Test!(mock)
MOIT.linear7test(mock, config)
set_mock_optimize_linear7Test!(mock)
MOIT.linear7test(mock, config_no_lhs_modif)
MOIU.set_mock_optimize!(
    mock,
    (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
        mock,
        MOI.INFEASIBLE,
        MOI.NO_SOLUTION,
        MOI.INFEASIBILITY_CERTIFICATE,
        (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) => [-1],
    ),
)
MOIT.linear8atest(mock, config)
MOIU.set_mock_optimize!(
    mock,
    (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, MOI.INFEASIBLE),
)
MOIT.linear8atest(mock, MOIT.Config(infeas_certificates = false))
MOIU.set_mock_optimize!(
    mock,
    (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
        mock,
        MOI.DUAL_INFEASIBLE,
        MOI.INFEASIBILITY_CERTIFICATE,
    ),
)
MOIT.linear8btest(mock, config)
MOIU.set_mock_optimize!(
    mock,
    (mock::MOIU.MockOptimizer) ->
        MOIU.mock_optimize!(mock, MOI.DUAL_INFEASIBLE),
)
MOIT.linear8btest(mock, MOIT.Config(infeas_certificates = false))
MOIU.set_mock_optimize!(
    mock,
    (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
        mock,
        MOI.DUAL_INFEASIBLE,
        (MOI.INFEASIBILITY_CERTIFICATE, [1, 1]),
    ),
)
MOIT.linear8ctest(mock, config)
MOIU.set_mock_optimize!(
    mock,
    (mock::MOIU.MockOptimizer) ->
        MOIU.mock_optimize!(mock, MOI.DUAL_INFEASIBLE),
)
MOIT.linear8ctest(mock, MOIT.Config(infeas_certificates = false))
MOIU.set_mock_optimize!(
    mock,
    (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
        mock,
        [650 / 11, 400 / 11],
        constraint_basis_status = [
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) =>
                [MOI.NONBASIC, MOI.NONBASIC],
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) =>
                [MOI.BASIC],
        ],
        variable_basis_status = [MOI.BASIC, MOI.BASIC],
    ),
)
MOIT.linear9test(mock, config)
MOIU.set_mock_optimize!(
    mock,
    (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
        mock,
        [5.0, 5.0],
        constraint_basis_status = [
            (MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64}) =>
                [MOI.NONBASIC_AT_UPPER],
        ],
        variable_basis_status = [MOI.BASIC, MOI.BASIC],
        (MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64}) => [-1],
    ),
    (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
        mock,
        [2.5, 2.5],
        constraint_basis_status = [
            (MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64}) =>
                [MOI.NONBASIC_AT_LOWER],
        ],
        variable_basis_status = [MOI.BASIC, MOI.BASIC],
        (MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64}) => [1],
    ),
    (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
        mock,
        [1.0, 1.0],
        (MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64}) => [1],
        constraint_basis_status = [
            (MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64}) =>
                [MOI.NONBASIC_AT_LOWER],
        ],
        variable_basis_status = [MOI.BASIC, MOI.BASIC],
    ),
    (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
        mock,
        [6.0, 6.0],
        (MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64}) => [-1],
        constraint_basis_status = [
            (MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64}) =>
                [MOI.NONBASIC_AT_UPPER],
        ],
        variable_basis_status = [MOI.BASIC, MOI.BASIC],
    ),
)
MOIT.linear10test(mock, config)
MOIU.set_mock_optimize!(
    mock,
    (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
        mock,
        [0.0, 0.0],
        constraint_basis_status = [
            (MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64}) =>
                [MOI.BASIC],
        ],
        variable_basis_status = [MOI.NONBASIC_AT_LOWER, MOI.NONBASIC_AT_LOWER],
        (MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64}) => [0],
    ),
)
MOIT.linear10btest(mock, config)

MOIU.set_mock_optimize!(
    mock,
    (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1.0, 1.0]),
    (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [0.5, 0.5]),
)
MOIT.linear11test(mock, config)
MOIU.set_mock_optimize!(
    mock,
    (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
        mock,
        MOI.INFEASIBLE,
        MOI.NO_SOLUTION,
        MOI.INFEASIBILITY_CERTIFICATE,
        (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) =>
            [-1, -1],
    ),
)
MOIT.linear12test(mock, config)
MOIU.set_mock_optimize!(
    mock,
    (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, MOI.INFEASIBLE),
)
MOIT.linear12test(mock, MOIT.Config(infeas_certificates = false))
MOIU.set_mock_optimize!(
    mock,
    (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
        mock,
        [1 / 5, 1 / 5],
        (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) =>
            [0],
        (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) => [0],
    ),
)
MOIT.linear13test(mock, config)
MOIU.set_mock_optimize!(
    mock,
    (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
        mock,
        [0, 1 / 2, 1],
        constraint_basis_status = [
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) =>
                [MOI.NONBASIC],
        ],
        variable_basis_status = [
            MOI.NONBASIC_AT_LOWER,
            MOI.BASIC,
            MOI.NONBASIC_AT_UPPER,
        ],
        (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) => [-1],
        (MOI.SingleVariable, MOI.GreaterThan{Float64}) => [2, 0, 0],
        (MOI.SingleVariable, MOI.LessThan{Float64}) => [-2],
    ),
    (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
        mock,
        [1],
        (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) => [-1],
        (MOI.SingleVariable, MOI.GreaterThan{Float64}) => [0],
    ),
)
# linear14 has double variable bounds for the z variable
mock.eval_variable_constraint_dual = false
MOIT.linear14test(mock, config)
mock.eval_variable_constraint_dual = true
MOIU.set_mock_optimize!(
    mock,
    (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
        mock,
        [0.0],
        (MOI.VectorAffineFunction{Float64}, MOI.Zeros) => [[0.0, 0.0]],
    ),
)
MOIT.linear15test(mock, config)

MOIU.set_mock_optimize!(
    mock,
    (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1.0, 0.0]),
)
MOIT.partial_start_test(mock, config)
