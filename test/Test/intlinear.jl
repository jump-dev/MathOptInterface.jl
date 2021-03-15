using Test
import MathOptInterface
const MOI = MathOptInterface
const MOIT = MOI.Test
const MOIU = MOI.Utilities

mock = MOIU.MockOptimizer(MOIU.Model{Float64}())
config = MOIT.TestConfig()

MOIU.set_mock_optimize!(
    mock,
    (mock::MOIU.MockOptimizer) -> begin
        MOI.set(mock, MOI.ObjectiveBound(), 20.0)
        MOIU.mock_optimize!(mock, [4, 5, 1])
    end,
)
MOIT.int1test(mock, config)
MOIU.set_mock_optimize!(
    mock,
    (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [0, 1, 2]),
    (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1, 1, 2]),
    (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
        mock,
        [0.0, 0.0, 1.0, 1.0, 0.0, 1.0, 0.0, 0.0, 3.0, 12.0],
    ),
    (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
        mock,
        [0.0, 0.0, 2.0, 2.0, 0.0, 2.0, 0.0, 0.0, 6.0, 24.0],
    ),
)
MOIT.int2test(mock, config)
# FIXME [1, 0...] is not the correct optimal solution but it passes the test
MOIU.set_mock_optimize!(
    mock,
    (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1.0; zeros(10)]),
)
MOIT.int3test(mock, config)
MOIU.set_mock_optimize!(
    mock,
    (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1, 0, 0, 1, 1]),
)
MOIT.knapsacktest(mock, config)
MOIU.set_mock_optimize!(
    mock,
    (mock::MOIU.MockOptimizer) ->
        MOIU.mock_optimize!(mock, [1.25, 8.75, 0.0, 1.0]),
)
MOIT.indicator1_test(mock, config)
MOIU.set_mock_optimize!(
    mock,
    (mock::MOIU.MockOptimizer) ->
        MOIU.mock_optimize!(mock, [2.0, 8.0, 1.0, 0.0]),
)
MOIT.indicator2_test(mock, config)
MOIU.set_mock_optimize!(
    mock,
    (mock::MOIU.MockOptimizer) ->
        MOIU.mock_optimize!(mock, [1.25, 8.75, 1.0, 1.0]),
)
MOIT.indicator3_test(mock, config)
MOIU.set_mock_optimize!(
    mock,
    (mock::MOIU.MockOptimizer) ->
        MOIU.mock_optimize!(mock, [1.25, 8.75, 0.0, 1.0]),
)
MOIT.indicator4_test(mock, config)
MOIU.set_mock_optimize!(
    mock,
    (mock::MOIU.MockOptimizer) -> begin
        MOI.set(mock, MOI.ObjectiveBound(), 0.0)
        MOIU.mock_optimize!(mock, [0.0, 0.0])
    end,
    (mock::MOIU.MockOptimizer) -> begin
        MOI.set(mock, MOI.ObjectiveBound(), 2.0)
        MOIU.mock_optimize!(mock, [2.0, 1.0])
    end,
    (mock::MOIU.MockOptimizer) -> begin
        MOI.set(mock, MOI.ObjectiveBound(), 2.0)
        MOIU.mock_optimize!(mock, [2.0, 2.0])
    end,
    (mock::MOIU.MockOptimizer) -> begin
        MOI.set(mock, MOI.ObjectiveBound(), 2.5)
        MOIU.mock_optimize!(mock, [2.5, 2.5])
    end,
    (mock::MOIU.MockOptimizer) -> begin
        MOI.set(mock, MOI.ObjectiveBound(), 3.0)
        MOIU.mock_optimize!(mock, [3.0, 3.0])
    end,
    (mock::MOIU.MockOptimizer) ->
        MOI.set(mock, MOI.TerminationStatus(), MOI.INFEASIBLE),
)
MOIT.semiconttest(mock, config)
MOIU.set_mock_optimize!(
    mock,
    (mock::MOIU.MockOptimizer) -> begin
        MOI.set(mock, MOI.ObjectiveBound(), 0.0)
        MOIU.mock_optimize!(mock, [0.0, 0.0])
    end,
    (mock::MOIU.MockOptimizer) -> begin
        MOI.set(mock, MOI.ObjectiveBound(), 2.0)
        MOIU.mock_optimize!(mock, [2.0, 1.0])
    end,
    (mock::MOIU.MockOptimizer) -> begin
        MOI.set(mock, MOI.ObjectiveBound(), 2.0)
        MOIU.mock_optimize!(mock, [2.0, 2.0])
    end,
    (mock::MOIU.MockOptimizer) -> begin
        MOI.set(mock, MOI.ObjectiveBound(), 3.0)
        MOIU.mock_optimize!(mock, [3.0, 2.5])
    end,
    (mock::MOIU.MockOptimizer) -> begin
        MOI.set(mock, MOI.ObjectiveBound(), 3.0)
        MOIU.mock_optimize!(mock, [3.0, 3.0])
    end,
    (mock::MOIU.MockOptimizer) ->
        MOI.set(mock, MOI.TerminationStatus(), MOI.INFEASIBLE),
)
MOIT.semiinttest(mock, config)
