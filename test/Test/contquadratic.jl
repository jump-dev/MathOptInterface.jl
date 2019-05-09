using Test
import MathOptInterface
const MOI = MathOptInterface
const MOIT = MOI.Test
const MOIU = MOI.Utilities

include("../model.jl")

mock = MOIU.MockOptimizer(Model{Float64}())
config = MOIT.TestConfig()

@testset "QP" begin
    MOIU.set_mock_optimize!(mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [4/7, 3/7, 6/7]))
    MOIT.qp1test(mock, config)
    MOIT.qp2test(mock, config)
    MOIU.set_mock_optimize!(mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1/4, 3/4]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1.0, 0.0]))
    MOIT.qp3test(mock, config)
end
@testset "QCP" begin
    MOIU.set_mock_optimize!(mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1/2, 7/4], MOI.FEASIBLE_POINT))
    MOIT.qcp1test(mock, config)
    MOIU.set_mock_optimize!(mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [√2], MOI.FEASIBLE_POINT))
    MOIT.qcp2test(mock, config)
    MOIU.set_mock_optimize!(mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [√2], MOI.FEASIBLE_POINT))
    MOIT.qcp3test(mock, config)
    MOIU.set_mock_optimize!(mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1.0, 1.0], MOI.FEASIBLE_POINT))
    MOIT.qcp4test(mock, config)
end
@testset "Non-convex QCP" begin
    MOIU.set_mock_optimize!(mock,
                            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [4.0, 1.0], MOI.FEASIBLE_POINT))
    MOIT.ncqcp1test(mock, config)
    MOIU.set_mock_optimize!(mock,
                            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [2.0, 2.0], MOI.FEASIBLE_POINT))
    MOIT.ncqcp2test(mock, config)
end
@testset "SOCP" begin
    MOIU.set_mock_optimize!(mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1/2, 1/2, 1/√2]))
    MOIT.socp1test(mock, config)
end
