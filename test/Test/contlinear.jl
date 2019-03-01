@testset "Continuous Linear" begin
    mock = MOIU.MockOptimizer(MOIU.UniversalFallback(ModelForMock{Float64}()))
    config = MOIT.TestConfig()
    config_no_lhs_modif = MOIT.TestConfig(modify_lhs = false)
    config_basis = MOIT.TestConfig(basis = true)

    function set_mock_optimize_linear1Test!(mock)
         MOIU.set_mock_optimize!(mock,
             (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1, 0],
                  (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})    => [-1]),
             (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1, 0],
                  (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})    => [-1]),
             (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [0, 0, 1],
                  (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})    => [-2]),
             (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [-1, 0, 2]),
             (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1, 0, 0]),
             (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [2, 0, 0]),
             (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [0, 2, 0]),
             (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1, 1, 0],
                  (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64})     => [-1.5],
                  (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) => [0.5]))
    end
    set_mock_optimize_linear1Test!(mock)
    MOIT.linear1test(mock, config)
    set_mock_optimize_linear1Test!(mock)
    MOIT.linear1test(mock, config_no_lhs_modif)
    MOIU.set_mock_optimize!(mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1, 0],
             (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})    => [-1],
             (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})    => [MOI.NONBASIC],
             (MOI.SingleVariable, MOI.GreaterThan{Float64}) => [MOI.BASIC, MOI.NONBASIC]))
    MOIT.linear2test(mock, config_basis)
    MOIU.set_mock_optimize!(mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [3],
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64})    => [MOI.NONBASIC],
            (MOI.SingleVariable, MOI.GreaterThan{Float64}) => [MOI.BASIC]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [0],
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})    => [MOI.BASIC],
            (MOI.SingleVariable, MOI.LessThan{Float64}) => [MOI.NONBASIC]))
    MOIT.linear3test(mock, config_basis)
    MOIU.set_mock_optimize!(mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [0, 0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [100, 0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [100, -100]))
    MOIT.linear4test(mock, config)
    function set_mock_optimize_linear5Test!(mock)
         MOIU.set_mock_optimize!(mock,
             (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [4/3, 4/3]),
             (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [2, 0]),
             (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [4, 0]),
             (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [2]))
    end
    set_mock_optimize_linear5Test!(mock)
    MOIT.linear5test(mock, config)
    set_mock_optimize_linear5Test!(mock)
    MOIT.linear5test(mock, config_no_lhs_modif)
    MOIU.set_mock_optimize!(mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [0, 0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [100, 0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [100, -100]))
    MOIT.linear6test(mock, config)
    function set_mock_optimize_linear7Test!(mock)
        MOIU.set_mock_optimize!(mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [0, 0]),
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [100, 0]),
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [100, -100]))
    end
    set_mock_optimize_linear7Test!(mock)
    MOIT.linear7test(mock, config)
    set_mock_optimize_linear7Test!(mock)
    MOIT.linear7test(mock, config_no_lhs_modif)
    MOIU.set_mock_optimize!(mock,
         (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, MOI.INFEASIBLE,
                                                           tuple(),
             (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) => [-1]))
    MOIT.linear8atest(mock, config)
    MOIU.set_mock_optimize!(mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, MOI.INFEASIBLE))
    MOIT.linear8atest(mock, MOIT.TestConfig(infeas_certificates=false))
    MOIU.set_mock_optimize!(mock,
         (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, MOI.DUAL_INFEASIBLE,
                                                           MOI.INFEASIBILITY_CERTIFICATE))
    MOIT.linear8btest(mock, config)
    MOIU.set_mock_optimize!(mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, MOI.DUAL_INFEASIBLE))
    MOIT.linear8btest(mock, MOIT.TestConfig(infeas_certificates=false))
    MOIU.set_mock_optimize!(mock,
         (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, MOI.DUAL_INFEASIBLE, (MOI.INFEASIBILITY_CERTIFICATE, [1, 1])))
    MOIT.linear8ctest(mock, config)
    MOIU.set_mock_optimize!(mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, MOI.DUAL_INFEASIBLE))
    MOIT.linear8ctest(mock, MOIT.TestConfig(infeas_certificates=false))
    MOIU.set_mock_optimize!(mock,
         (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [650/11, 400/11],
              (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})    => [MOI.NONBASIC, MOI.NONBASIC],
              (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) => [MOI.BASIC],
              (MOI.SingleVariable, MOI.GreaterThan{Float64})                => [MOI.BASIC, MOI.BASIC]))
    MOIT.linear9test(mock, config_basis)
    MOIU.set_mock_optimize!(mock,
         (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [5.0, 5.0],
              (MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64})    => [-1],
              (MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64})    => [MOI.NONBASIC_AT_UPPER],
              (MOI.SingleVariable, MOI.GreaterThan{Float64})                => [MOI.BASIC, MOI.BASIC]),
         (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [2.5, 2.5],
              (MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64})    => [1],
              (MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64})    => [MOI.NONBASIC_AT_LOWER],
              (MOI.SingleVariable, MOI.GreaterThan{Float64})                => [MOI.BASIC, MOI.BASIC]),
         (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1.0, 1.0],
              (MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64})    => [MOI.NONBASIC_AT_LOWER],
              (MOI.SingleVariable, MOI.GreaterThan{Float64})                => [MOI.BASIC, MOI.BASIC]),
         (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [6.0, 6.0],
              (MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64})    => [MOI.NONBASIC_AT_UPPER],
              (MOI.SingleVariable, MOI.GreaterThan{Float64})                => [MOI.BASIC, MOI.BASIC]))
    MOIT.linear10test(mock, config_basis)
    MOIU.set_mock_optimize!(mock,
         (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1.0, 1.0]),
         (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [0.5, 0.5]))
    MOIT.linear11test(mock, config)
    MOIU.set_mock_optimize!(mock,
         (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, MOI.INFEASIBLE,
                                                           tuple(),
              (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})    => [-1, -1]))
    MOIT.linear12test(mock, config)
    MOIU.set_mock_optimize!(mock,
         (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, MOI.INFEASIBLE))
    MOIT.linear12test(mock, MOIT.TestConfig(infeas_certificates=false))
    MOIU.set_mock_optimize!(mock,
         (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1/5, 1/5],
             (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) => [0],
             (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64})     => [0]))
    MOIT.linear13test(mock, config)
    MOIU.set_mock_optimize!(mock,
         (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [0, 1/2, 1],
             (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})    => [-1],
             (MOI.SingleVariable, MOI.GreaterThan{Float64})                => [2, 0, 0],
             (MOI.SingleVariable, MOI.LessThan{Float64})                   => [-2],
             (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})    => [MOI.NONBASIC],
             (MOI.SingleVariable, MOI.GreaterThan{Float64})                => [MOI.NONBASIC, MOI.BASIC, MOI.BASIC],
             (MOI.SingleVariable, MOI.LessThan{Float64})                   => [MOI.NONBASIC]),
         (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1],
             (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})    => [-1],
             (MOI.SingleVariable, MOI.GreaterThan{Float64})                => [0]))
    # linear14 has double variable bounds for the z variable
    mock.eval_variable_constraint_dual = false
    MOIT.linear14test(mock, config_basis)
    mock.eval_variable_constraint_dual = true
    MOIU.set_mock_optimize!(mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [0.0],
             (MOI.VectorAffineFunction{Float64}, MOI.Zeros) => [0.0, 0.0]))
    MOIT.linear15test(mock, config)

    MOIU.set_mock_optimize!(mock,
         (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1.0, 0.0]))
    MOIT.partial_start_test(mock, config)
end
