using Test

import MathOptInterface
const MOI = MathOptInterface

@testset "hs071" begin
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
        eval_objective_value=false
    )
    config = MOI.Test.TestConfig(optimal_status = MOI.LOCALLY_SOLVED)
    MOI.Utilities.set_mock_optimize!(
        mock,
        (mock) -> begin
            MOI.Utilities.mock_optimize!(
                mock, config.optimal_status,
                [1.0, 4.7429996418092970, 3.8211499817883077, 1.379408289755698]
            )
            MOI.set(mock, MOI.ObjectiveValue(), 17.014017145179164)
        end
    )
    MOI.Test.hs071_test(mock, config)
    MOI.Test.hs071_no_hessian_test(mock, config)
    MOI.Test.hs071_hessian_vector_product_test(mock, config)

    d = MOI.Test.HS071(false)
    VI = MOI.VariableIndex
    @test MOI.objective_expr(d) == :(x[$(VI(1))] * x[$(VI(4))] * (x[$(VI(1))] +
                                     x[$(VI(2))] + x[$(VI(3))]) + x[$(VI(3))])
    @test MOI.constraint_expr(d, 1) ==
        :(x[$(VI(1))] * x[$(VI(2))] * x[$(VI(3))] * x[$(VI(4))] >= 25.0)
    @test MOI.constraint_expr(d, 2) ==
        :(x[$(VI(1))]^2 + x[$(VI(2))]^2 + x[$(VI(3))]^2 + x[$(VI(4))]^2 == 40.0)
    @test_throws ErrorException MOI.constraint_expr(d, 3)
end

@testset "mixed_complementarity" begin
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    )
    config = MOI.Test.TestConfig(optimal_status = MOI.LOCALLY_SOLVED)
    MOI.Utilities.set_mock_optimize!(
        mock,
        (mock) -> MOI.Utilities.mock_optimize!(
            mock, config.optimal_status, [2.8, 0.0, 0.8, 1.2]
        )
    )
    MOI.Test.mixed_complementaritytest(mock, config)
end

@testset "math_program_complementarity_constraints" begin
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    )
    config = MOI.Test.TestConfig(optimal_status = MOI.LOCALLY_SOLVED)
    MOI.Utilities.set_mock_optimize!(
        mock,
        (mock) -> MOI.Utilities.mock_optimize!(
            mock, config.optimal_status, [1.0, 0.0, 3.5, 0.0, 0.0, 0.0, 3.0, 6.0]
        )
    )
    MOI.Test.math_program_complementarity_constraintstest(mock, config)
end
