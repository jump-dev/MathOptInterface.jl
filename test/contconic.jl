@testset "Continuous Conic" begin
    mock = MOIU.MockOptimizer(Model{Float64}())
    config = MOIT.TestConfig()
    mock.evalobjective = true

    @testset "Linear" begin
        mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1.0, 0.0, 2.0],
                              (MOI.VectorOfVariables,             MOI.Nonnegatives) => [[0, 2, 0]],
                              (MOI.VectorAffineFunction{Float64}, MOI.Zeros)        => [[-3, -1]])
        MOIT.lin1vtest(mock, config)
        mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1.0, 0.0, 2.0],
                              (MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives) => [[0, 2, 0]],
                              (MOI.VectorAffineFunction{Float64}, MOI.Zeros)        => [[-3, -1]])
        MOIT.lin1ftest(mock, config)
        mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [-4, -3, 16, 0],
                              (MOI.VectorOfVariables,             MOI.Nonnegatives) => [[0]],
                              (MOI.VectorOfVariables,             MOI.Nonpositives) => [[0]],
                              (MOI.VectorOfVariables,             MOI.Zeros)        => [[7]],
                              (MOI.VectorAffineFunction{Float64}, MOI.Zeros)        => [[7, 2, -4]])
        MOIT.lin2vtest(mock, config)
        mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [-4, -3, 16, 0],
                              (MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives) => [[0]],
                              (MOI.VectorAffineFunction{Float64}, MOI.Nonpositives) => [[0]],
                              (MOI.VectorAffineFunction{Float64}, MOI.Zeros)        => [[7, 2, -4], [7]])
        MOIT.lin2ftest(mock, config)
        mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, MOI.InfeasiblePoint)
        MOIT.lin3test(mock, config)
        mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, MOI.InfeasibleNoResult)
        MOIT.lin3test(mock, MOIT.TestConfig(infeas_certificates=false))
        mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, MOI.InfeasibleOrUnbounded)
        MOIT.lin3test(mock, MOIT.TestConfig(infeas_certificates=false))
        mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, MOI.InfeasiblePoint)
        MOIT.lin4test(mock, config)
        mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, MOI.InfeasibleNoResult)
        MOIT.lin4test(mock, MOIT.TestConfig(infeas_certificates=false))
        mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, MOI.InfeasibleOrUnbounded)
        MOIT.lin4test(mock, MOIT.TestConfig(infeas_certificates=false))
    end
    @testset "SOC" begin
        mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1.0, 1/√2, 1/√2],
                              (MOI.VectorOfVariables,             MOI.SecondOrderCone) => [[√2, -1, -1]],
                              (MOI.VectorAffineFunction{Float64}, MOI.Zeros)           => [[-√2]])
        MOIT.soc1vtest(mock, config)
        mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1.0, 1/√2, 1/√2],
                              (MOI.VectorAffineFunction{Float64}, MOI.SecondOrderCone) => [[√2, -1, -1]],
                              (MOI.VectorAffineFunction{Float64}, MOI.Zeros)           => [[-√2]])
        MOIT.soc1ftest(mock, config)
        mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [-1/√2, 1/√2, 1.],
                              (MOI.VectorAffineFunction{Float64}, MOI.SecondOrderCone) => [[√2, 1, -1]],
                              (MOI.VectorAffineFunction{Float64}, MOI.Zeros)           => [[√2]],
                              (MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives)    => [[1.0]])
        MOIT.soc2ntest(mock, config)
        mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [-1/√2, 1/√2, 1.],
                              (MOI.VectorAffineFunction{Float64}, MOI.SecondOrderCone) => [[√2, 1, -1]],
                              (MOI.VectorAffineFunction{Float64}, MOI.Zeros)           => [[√2]],
                              (MOI.VectorAffineFunction{Float64}, MOI.Nonpositives)    => [[-1.0]])
        MOIT.soc2ptest(mock, config)
        mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, MOI.InfeasiblePoint)
        MOIT.soc3test(mock, config)
        mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1.0, 2/√5, 1/√5, 2/√5, 1/√5],
                              (MOI.VectorAffineFunction{Float64}, MOI.Zeros)           => [[-√5, -2.0, -1.0]],
                              (MOI.VectorOfVariables,             MOI.SecondOrderCone) => [[√5, -2.0, -1.0]])
        MOIT.soc4test(mock, config)
    end
    @testset "RSOC" begin
        mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1/√2, 1/√2, 0.5, 1.0],
                              (MOI.SingleVariable,                MOI.EqualTo{Float64})       => [-√2, -1/√2],
                              (MOI.VectorOfVariables,             MOI.RotatedSecondOrderCone) => [[√2, 1/√2, -1.0, -1.0]])
        MOIT.rotatedsoc1vtest(mock, config)
        mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1/√2, 1/√2],
                              (MOI.VectorAffineFunction{Float64}, MOI.RotatedSecondOrderCone) => [[√2, 1/√2, -1.0, -1.0]])
        MOIT.rotatedsoc1ftest(mock, config)
        mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock,
                              (MOI.SingleVariable,                MOI.LessThan{Float64})      => [-1],
                              (MOI.SingleVariable,                MOI.EqualTo{Float64})       => [-1],
                              (MOI.SingleVariable,                MOI.GreaterThan{Float64})   => [1],
                              (MOI.VectorOfVariables            , MOI.RotatedSecondOrderCone) => [[1, 1, -1]])
        MOIT.rotatedsoc2test(mock, config)
        n = 2
        ub = 3.0
        mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1.0; zeros(n-1); ub; √ub; ones(2)],
                              (MOI.SingleVariable,                MOI.EqualTo{Float64})       => [-√ub/4, -√ub/4],
                              (MOI.VectorOfVariables,             MOI.Nonnegatives)           => [zeros(n)],
                              (MOI.SingleVariable,                MOI.GreaterThan{Float64})   => [0.0],
                              (MOI.SingleVariable,                MOI.LessThan{Float64})      => [-1/(2*√ub)],
                              (MOI.VectorAffineFunction{Float64}, MOI.RotatedSecondOrderCone) => [[√ub/(2*√2); √ub/(2*√2); -√ub/2; zeros(n-1)], [√ub/√2, 1/√(2*ub), -1.0]])
        MOIT.rotatedsoc3test(mock, config)
    end
    @testset "GeoMean" begin
        mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, ones(4))
        MOIT.geomeantest(mock, config)
    end
    @testset "Exponential" begin
        mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1., 2., 2exp(1/2)],
                              (MOI.VectorOfVariables,             MOI.ExponentialCone)   => [[-exp(1/2), -exp(1/2)/2, 1.]],
                              (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64})  => [1 + exp(1/2), 1 + exp(1/2)/2])
        MOIT.exp1vtest(mock, config)
        mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1., 2., 2exp(1/2)],
                              (MOI.VectorAffineFunction{Float64}, MOI.ExponentialCone)   => [[-exp(1/2), -exp(1/2)/2, 1.]],
                              (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64})  => [1 + exp(1/2), 1 + exp(1/2)/2])
        MOIT.exp1ftest(mock, config)
        mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [0., -0.3, 0., exp(-0.3), exp(-0.3), exp(-0.3), 0., 1.0, 0.],
                              (MOI.VectorAffineFunction{Float64}, MOI.ExponentialCone)   => [[-exp(-0.3)/2, -1.3exp(-0.3)/2, 0.5], [-exp(-0.3)/2, -1.3exp(-0.3)/2, 0.5]],
                              (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64})  => [-1.0, exp(-0.3)*0.3],
                              (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) => [-exp(-0.3)*0.3],
                              (MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives)      => [[0.0, exp(-0.3), exp(-0.3)/2], [0.0, 0.0, exp(-0.3)/2]])
        MOIT.exp2test(mock, config)
        mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [log(5), 5.],
                              (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) => [0.],
                              (MOI.SingleVariable,                MOI.LessThan{Float64}) => [-1/5],
                              (MOI.VectorAffineFunction{Float64}, MOI.ExponentialCone)   => [[-1., log(5)-1, 1/5]])
        MOIT.exp3test(mock, config)
    end
    @testset "PSD" begin
        mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, ones(3),
                              (MOI.VectorOfVariables,             MOI.PositiveSemidefiniteConeTriangle) => [[1, -1, 1]],
                              (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64})                 => [2])
        MOIT.sdp0tvtest(mock, config)
        mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, ones(3),
                              (MOI.VectorAffineFunction{Float64}, MOI.PositiveSemidefiniteConeTriangle) => [[1, -1, 1]],
                              (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64})                 => [2])
        MOIT.sdp0tftest(mock, config)
        δ = √(1 + (3*√2+2)*√(-116*√2+166) / 14) / 2
        ε = √((1 - 2*(√2-1)*δ^2) / (2-√2))
        y2 = 1 - ε*δ
        y1 = 1 - √2*y2
        obj = y1 + y2/2
        k = -2*δ/ε
        x2 = ((3-2obj)*(2+k^2)-4) / (4*(2+k^2)-4*√2)
        α = √(3-2obj-4x2)/2
        β = k*α
        Xv = [α^2, α*β, β^2, α^2, α*β, α^2]
        xv = [√2*x2, x2, x2]
        mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [Xv; xv],
                              (MOI.VectorOfVariables,             MOI.PositiveSemidefiniteConeTriangle) => [[1+(√2-1)*y2, 1-y2, 1+(√2-1)*y2, -y2, 1-y2, 1+(√2-1)*y2]],
                              (MOI.VectorOfVariables,             MOI.SecondOrderCone                 ) => [[1-y1, -y2, -y2]],
                              (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}                ) => [y1, y2])
        MOIT.sdp1tvtest(mock, config)
        mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [Xv; xv],
                              (MOI.VectorAffineFunction{Float64}, MOI.PositiveSemidefiniteConeTriangle) => [[1+(√2-1)*y2, 1-y2, 1+(√2-1)*y2, -y2, 1-y2, 1+(√2-1)*y2]],
                              (MOI.VectorOfVariables,             MOI.SecondOrderCone                 ) => [[1-y1, -y2, -y2]],
                              (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}                ) => [y1, y2])
        MOIT.sdp1tftest(mock, config)
        η = 10.0
        α = 0.8
        δ = 0.9
        mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [2η/3, 0, η/3, 0, 0, 0, η*δ*(1 - 1/√3)/2],
                              (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64})             => [δ*(1-1/√3)/2],
                              (MOI.VectorAffineFunction{Float64}, MOI.Nonpositives)                     => [[0, -α/√3+δ/(2*√6)*(2*√2-1), 0, -3δ*(1-1/√3)/8, -3δ*(1-1/√3)/8, -δ*(3 - 2*√3 + 1/√3)/8]],
                              (MOI.VectorAffineFunction{Float64}, MOI.PositiveSemidefiniteConeTriangle) => [[(1-1/√3)/2, 1/√6, (1+1/√3)/2]],
                              (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64})                 => [0])
        MOIT.sdp2test(mock, config)
    end
    @testset "LogDet and RootDet" begin
        mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [0, 1, 0, 1])
        MOIT.logdettest(mock, config)
        mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1, 1, 0, 1])
        MOIT.rootdettest(mock, config)
    end
end
