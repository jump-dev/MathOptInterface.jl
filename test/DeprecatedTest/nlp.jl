using Test

import MathOptInterface
const MOI = MathOptInterface

@testset "hs071-manual" begin
    d = MOI.DeprecatedTest.HS071(true, true)
    MOI.initialize(d, [:Grad, :Jac, :ExprGraph, :Hess, :HessVec])
    @test :HessVec in MOI.features_available(d)
    x = ones(4)
    # f(x)
    @test MOI.eval_objective(d, x) == 4.0
    # g(x)
    g = zeros(2)
    MOI.eval_constraint(d, g, x)
    @test g == [1.0, 4.0]
    # f'(x)
    ∇f = fill(NaN, length(x))
    MOI.eval_objective_gradient(d, ∇f, x)
    @test ∇f == [4.0, 1.0, 2.0, 3.0]
    # Jacobian
    Js = MOI.jacobian_structure(d)
    J = fill(NaN, length(Js))
    MOI.eval_constraint_jacobian(d, J, x)
    @test J == [1, 1, 1, 1, 2, 2, 2, 2]
    # Hessian-lagrangian
    Hs = MOI.hessian_lagrangian_structure(d)
    H = fill(NaN, length(Hs))
    MOI.eval_hessian_lagrangian(d, H, x, 1.0, [1.0, 1.0])
    @test H == [4, 2, 2, 2, 1, 2, 5, 2, 2, 2]
    # Hessian-lagrangian-product
    Hv = fill(NaN, length(x))
    v = [1.0, 1.1, 1.2, 1.3]
    MOI.eval_hessian_lagrangian_product(d, Hv, x, v, 1.0, [1.0, 1.0])
    @test Hv == [15.1, 8.0, 8.1, 12.2]
end

@testset "hs071" begin
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
        eval_objective_value = false,
    )
    config = MOI.DeprecatedTest.Config(optimal_status = MOI.LOCALLY_SOLVED)
    MOI.Utilities.set_mock_optimize!(
        mock,
        (mock) -> begin
            MOI.Utilities.mock_optimize!(
                mock,
                config.optimal_status,
                [
                    1.0,
                    4.7429996418092970,
                    3.8211499817883077,
                    1.379408289755698,
                ],
            )
            MOI.set(mock, MOI.ObjectiveValue(), 17.014017145179164)
        end,
    )
    MOI.DeprecatedTest.hs071_test(mock, config)
    MOI.DeprecatedTest.hs071_no_hessian_test(mock, config)
    MOI.DeprecatedTest.hs071_hessian_vector_product_test(mock, config)

    d = MOI.DeprecatedTest.HS071(false)
    VI = MOI.VariableIndex
    @test MOI.objective_expr(d) == :(
        x[$(VI(1))] * x[$(VI(4))] * (x[$(VI(1))] + x[$(VI(2))] + x[$(VI(3))]) +
        x[$(VI(3))]
    )
    @test MOI.constraint_expr(d, 1) ==
          :(x[$(VI(1))] * x[$(VI(2))] * x[$(VI(3))] * x[$(VI(4))] >= 25.0)
    @test MOI.constraint_expr(d, 2) == :(
        x[$(VI(1))]^2 + x[$(VI(2))]^2 + x[$(VI(3))]^2 + x[$(VI(4))]^2 == 40.0
    )
    @test_throws ErrorException MOI.constraint_expr(d, 3)
end

@testset "mixed_complementarity" begin
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    config = MOI.DeprecatedTest.Config(optimal_status = MOI.LOCALLY_SOLVED)
    MOI.Utilities.set_mock_optimize!(
        mock,
        (mock) -> MOI.Utilities.mock_optimize!(
            mock,
            config.optimal_status,
            [2.8, 0.0, 0.8, 1.2],
        ),
    )
    MOI.DeprecatedTest.mixed_complementaritytest(mock, config)
end

@testset "math_program_complementarity_constraints" begin
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    config = MOI.DeprecatedTest.Config(optimal_status = MOI.LOCALLY_SOLVED)
    MOI.Utilities.set_mock_optimize!(
        mock,
        (mock) -> MOI.Utilities.mock_optimize!(
            mock,
            config.optimal_status,
            [1.0, 0.0, 3.5, 0.0, 0.0, 0.0, 3.0, 6.0],
        ),
    )
    MOI.DeprecatedTest.math_program_complementarity_constraintstest(
        mock,
        config,
    )
end
