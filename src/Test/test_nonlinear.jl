"""
    HS071(
        enable_hessian::Bool,
        enable_hessian_vector_product::Bool = false,
    )

An AbstractNLPEvaluator for the problem:
```
min x1 * x4 * (x1 + x2 + x3) + x3
st  x1 * x2 * x3 * x4 >= 25
    x1^2 + x2^2 + x3^2 + x4^2 = 40
    1 <= x1, x2, x3, x4 <= 5
```
Start at (1,5,5,1)
End at (1.000..., 4.743..., 3.821..., 1.379...)
"""
struct HS071 <: MOI.AbstractNLPEvaluator
    enable_hessian::Bool
    enable_hessian_vector_product::Bool
    function HS071(
        enable_hessian::Bool,
        enable_hessian_vector_product::Bool = false,
    )
        return new(enable_hessian, enable_hessian_vector_product)
    end
end

function MOI.initialize(d::HS071, requested_features::Vector{Symbol})
    for feat in requested_features
        if !(feat in MOI.features_available(d))
            error("Unsupported feature $feat")
            # TODO: implement Jac-vec products for solvers that need them
        end
    end
end

function MOI.features_available(d::HS071)
    features = [:Grad, :Jac, :ExprGraph]
    if d.enable_hessian
        push!(features, :Hess)
    end
    if d.enable_hessian_vector_product
        return push!(features, :HessVec)
    end
    return features
end

function MOI.objective_expr(::HS071)
    return :(
        x[$(MOI.VariableIndex(1))] *
        x[$(MOI.VariableIndex(4))] *
        (
            x[$(MOI.VariableIndex(1))] +
            x[$(MOI.VariableIndex(2))] +
            x[$(MOI.VariableIndex(3))]
        ) + x[$(MOI.VariableIndex(3))]
    )
end

function MOI.constraint_expr(::HS071, i::Int)
    if i == 1
        return :(
            x[$(MOI.VariableIndex(1))] *
            x[$(MOI.VariableIndex(2))] *
            x[$(MOI.VariableIndex(3))] *
            x[$(MOI.VariableIndex(4))] >= 25.0
        )
    elseif i == 2
        return :(
            x[$(MOI.VariableIndex(1))]^2 +
            x[$(MOI.VariableIndex(2))]^2 +
            x[$(MOI.VariableIndex(3))]^2 +
            x[$(MOI.VariableIndex(4))]^2 == 40.0
        )
    else
        error("Out of bounds constraint.")
    end
end

MOI.eval_objective(::HS071, x) = x[1] * x[4] * (x[1] + x[2] + x[3]) + x[3]

function MOI.eval_constraint(::HS071, g, x)
    g[1] = x[1] * x[2] * x[3] * x[4]
    g[2] = x[1]^2 + x[2]^2 + x[3]^2 + x[4]^2
    return
end

function MOI.eval_objective_gradient(::HS071, grad_f, x)
    grad_f[1] = x[1] * x[4] + x[4] * (x[1] + x[2] + x[3])
    grad_f[2] = x[1] * x[4]
    grad_f[3] = x[1] * x[4] + 1
    grad_f[4] = x[1] * (x[1] + x[2] + x[3])
    return
end

function MOI.jacobian_structure(::HS071)
    return Tuple{Int64,Int64}[
        (1, 1),
        (1, 2),
        (1, 3),
        (1, 4),
        (2, 1),
        (2, 2),
        (2, 3),
        (2, 4),
    ]
end

function MOI.hessian_lagrangian_structure(d::HS071)
    @assert d.enable_hessian
    return Tuple{Int64,Int64}[
        (1, 1),
        (2, 1),
        (2, 2),
        (3, 1),
        (3, 2),
        (3, 3),
        (4, 1),
        (4, 2),
        (4, 3),
        (4, 4),
    ]
end

function MOI.eval_constraint_jacobian(::HS071, J, x)
    # Constraint (row) 1
    J[1] = x[2] * x[3] * x[4]  # 1,1
    J[2] = x[1] * x[3] * x[4]  # 1,2
    J[3] = x[1] * x[2] * x[4]  # 1,3
    J[4] = x[1] * x[2] * x[3]  # 1,4
    # Constraint (row) 2
    J[5] = 2 * x[1]  # 2,1
    J[6] = 2 * x[2]  # 2,2
    J[7] = 2 * x[3]  # 2,3
    J[8] = 2 * x[4]  # 2,4
    return
end

function MOI.eval_hessian_lagrangian(d::HS071, H, x, σ, μ)
    @assert d.enable_hessian
    # Again, only lower left triangle
    # Objective
    H[1] = σ * (2 * x[4])               # 1,1
    H[2] = σ * (x[4])               # 2,1
    H[3] = 0                          # 2,2
    H[4] = σ * (x[4])               # 3,1
    H[5] = 0                          # 3,2
    H[6] = 0                          # 3,3
    H[7] = σ * (2 * x[1] + x[2] + x[3])  # 4,1
    H[8] = σ * (x[1])               # 4,2
    H[9] = σ * (x[1])               # 4,3
    H[10] = 0                         # 4,4
    # First constraint
    H[2] += μ[1] * (x[3] * x[4])  # 2,1
    H[4] += μ[1] * (x[2] * x[4])  # 3,1
    H[5] += μ[1] * (x[1] * x[4])  # 3,2
    H[7] += μ[1] * (x[2] * x[3])  # 4,1
    H[8] += μ[1] * (x[1] * x[3])  # 4,2
    H[9] += μ[1] * (x[1] * x[2])  # 4,3
    # Second constraint
    H[1] += μ[2] * 2  # 1,1
    H[3] += μ[2] * 2  # 2,2
    H[6] += μ[2] * 2  # 3,3
    H[10] += μ[2] * 2
    return
end

function MOI.eval_hessian_lagrangian_product(d::HS071, h, x, v, σ, μ)
    @assert d.enable_hessian_vector_product
    # Objective
    h[1] =
        2.0 * x[4] * v[1] +
        x[4] * v[2] +
        x[4] * v[3] +
        (2.0 * x[1] + x[2] + x[3]) * v[4]
    h[2] = x[4] * v[1] + x[1] * v[4]
    h[3] = x[4] * v[1] + x[1] * v[4]
    h[4] = (2.0 * x[1] + x[2] + x[3]) * v[1] + x[1] * v[2] + x[1] * v[3]
    h .*= σ
    # First constraint
    h[1] +=
        μ[1] * (x[3] * x[4] * v[2] + x[2] * x[4] * v[3] + x[2] * x[3] * v[4])
    h[2] +=
        μ[1] * (x[3] * x[4] * v[1] + x[1] * x[4] * v[3] + x[1] * x[3] * v[4])
    h[3] +=
        μ[1] * (x[2] * x[4] * v[1] + x[1] * x[4] * v[2] + x[1] * x[2] * v[4])
    h[4] +=
        μ[1] * (x[2] * x[3] * v[1] + x[1] * x[3] * v[2] + x[1] * x[2] * v[3])
    # Second constraint
    h[1] += μ[2] * 2.0 * v[1]
    h[2] += μ[2] * 2.0 * v[2]
    h[3] += μ[2] * 2.0 * v[3]
    h[4] += μ[2] * 2.0 * v[4]
    return
end

"""
    FeasibilitySenseEvaluator(enable_hessian::Bool)

An AbstractNLPEvaluator for the problem:
```
Test for FEASIBILITY_SENSE.
Find x satisfying x^2 == 1.
```
"""
struct FeasibilitySenseEvaluator <: MOI.AbstractNLPEvaluator
    enable_hessian::Bool
end

function MOI.initialize(
    d::FeasibilitySenseEvaluator,
    requested_features::Vector{Symbol},
)
    for feat in requested_features
        if !(feat in MOI.features_available(d))
            error("Unsupported feature $feat")
            # TODO: implement Jac-vec and Hess-vec products
            # for solvers that need them
        end
    end
    return
end

function MOI.features_available(d::FeasibilitySenseEvaluator)
    if d.enable_hessian
        return [:Grad, :Jac, :Hess, :ExprGraph]
    else
        return [:Grad, :Jac, :ExprGraph]
    end
end

MOI.objective_expr(::FeasibilitySenseEvaluator) = :()

function MOI.constraint_expr(::FeasibilitySenseEvaluator, i::Int)
    @assert i == 1
    return :(x[$(MOI.VariableIndex(1))]^2 == 1)
end

MOI.eval_objective(d::FeasibilitySenseEvaluator, x) = 0.0

function MOI.eval_constraint(::FeasibilitySenseEvaluator, g, x)
    g[1] = x[1]^2
    return
end

function MOI.eval_objective_gradient(::FeasibilitySenseEvaluator, grad_f, x)
    grad_f[1] = 0.0
    return
end

function MOI.jacobian_structure(::FeasibilitySenseEvaluator)
    return Tuple{Int64,Int64}[(1, 1)]
end

function MOI.hessian_lagrangian_structure(d::FeasibilitySenseEvaluator)
    @assert d.enable_hessian
    return Tuple{Int64,Int64}[(1, 1)]
end

function MOI.eval_constraint_jacobian(::FeasibilitySenseEvaluator, J, x)
    J[1] = 2 * x[1]
    return
end

function MOI.eval_hessian_lagrangian(d::FeasibilitySenseEvaluator, H, x, σ, μ)
    @assert d.enable_hessian
    H[1] = 2 * μ[1] # 1,1
    return
end

function _test_HS071(model::MOI.ModelLike, config::Config, evaluator::HS071)
    @requires MOI.supports(model, MOI.NLPBlock())
    @requires _supports(config, MOI.optimize!)
    @requires MOI.supports_constraint(
        model,
        MOI.SingleVariable,
        MOI.LessThan{Float64},
    )
    @requires MOI.supports_constraint(
        model,
        MOI.SingleVariable,
        MOI.GreaterThan{Float64},
    )
    @requires MOI.supports(model, MOI.VariablePrimalStart(), MOI.VariableIndex)
    lb = [25.0, 40.0]
    ub = [Inf, 40.0]
    block_data = MOI.NLPBlockData(MOI.NLPBoundsPair.(lb, ub), evaluator, true)
    v = MOI.add_variables(model, 4)
    @test MOI.get(model, MOI.NumberOfVariables()) == 4
    l = [1.0, 1.0, 1.0, 1.0]
    u = [5.0, 5.0, 5.0, 5.0]
    start = [1, 5, 5, 1]
    for i in 1:4
        cub = MOI.add_constraint(
            model,
            MOI.SingleVariable(v[i]),
            MOI.LessThan(u[i]),
        )
        # We test this after the creation of every `SingleVariable` constraint
        # to ensure a good coverage of corner cases.
        @test cub.value == v[i].value
        clb = MOI.add_constraint(
            model,
            MOI.SingleVariable(v[i]),
            MOI.GreaterThan(l[i]),
        )
        @test clb.value == v[i].value
        MOI.set(model, MOI.VariablePrimalStart(), v[i], start[i])
    end
    MOI.set(model, MOI.NLPBlock(), block_data)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
    @test MOI.get(model, MOI.ResultCount()) >= 1
    @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
    @test isapprox(
        MOI.get(model, MOI.ObjectiveValue()),
        17.014017145179164,
        config,
    )
    @test isapprox(
        MOI.get(model, MOI.VariablePrimal(), v),
        [1.0, 4.7429996418092970, 3.8211499817883077, 1.3794082897556983],
        config,
    )
    # TODO: Duals? Maybe better to test on a convex instance.
    return
end

"""
    test_nonlinear_hs071(model::MOI.ModelLike, config::Config)

Test the nonlinear HS071 problem.
"""
function test_nonlinear_hs071(model::MOI.ModelLike, config::Config)
    _test_HS071(model, config, HS071(true))
    return
end

function setup_test(
    ::typeof(test_nonlinear_hs071),
    model::MOIU.MockOptimizer,
    config::Config,
)
    config.optimal_status = MOI.LOCALLY_SOLVED
    MOI.Utilities.set_mock_optimize!(
        model,
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
    flag = model.eval_objective_value
    model.eval_objective_value = false
    return () -> model.eval_objective_value = flag
end

"""
    test_nonlinear_hs071_no_hessian(model::MOI.ModelLike, config::Config)

Test the nonlinear HS071 problem without hessians.
"""
function test_nonlinear_hs071_no_hessian(model, config)
    _test_HS071(model, config, HS071(false))
    return
end

function setup_test(
    ::typeof(test_nonlinear_hs071_no_hessian),
    model::MOIU.MockOptimizer,
    config::Config,
)
    config.optimal_status = MOI.LOCALLY_SOLVED
    MOI.Utilities.set_mock_optimize!(
        model,
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
    flag = model.eval_objective_value
    model.eval_objective_value = false
    return () -> model.eval_objective_value = flag
end

"""
    test_nonlinear_hs071_hessian_vector_product(
        model::MOI.ModelLike,
        config::Config,
    )

Test the nonlinear HS071 problem with a hessian vector product.
"""
function test_nonlinear_hs071_hessian_vector_product(model, config)
    _test_HS071(model, config, HS071(false, true))
    return
end

function setup_test(
    ::typeof(test_nonlinear_hs071_hessian_vector_product),
    model::MOIU.MockOptimizer,
    config::Config,
)
    config.optimal_status = MOI.LOCALLY_SOLVED
    MOI.Utilities.set_mock_optimize!(
        model,
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
    flag = model.eval_objective_value
    model.eval_objective_value = false
    return () -> model.eval_objective_value = flag
end

"""
    test_nonlinear_hs071_NLPBlockDual(model::MOI.ModelLike, config::Config)

Test NLPBlockDual.
"""
function test_nonlinear_hs071_NLPBlockDual(model::MOI.ModelLike, config::Config)
    @requires MOI.supports(model, MOI.NLPBlock())
    @requires _supports(config, MOI.optimize!)
    @requires MOI.supports(model, MOI.VariablePrimalStart(), MOI.VariableIndex)
    v = MOI.add_variables(model, 4)
    l = [1.1, 1.2, 1.3, 1.4]
    u = [5.1, 5.2, 5.3, 5.4]
    start = [2.1, 2.2, 2.3, 2.4]
    MOI.add_constraint.(model, MOI.SingleVariable.(v), MOI.GreaterThan.(l))
    MOI.add_constraint.(model, MOI.SingleVariable.(v), MOI.LessThan.(u))
    MOI.set.(model, MOI.VariablePrimalStart(), v, start)
    lb, ub = [25.0, 40.0], [Inf, 40.0]
    evaluator = MOI.Test.HS071(true)
    block_data = MOI.NLPBlockData(MOI.NLPBoundsPair.(lb, ub), evaluator, true)
    MOI.set(model, MOI.NLPBlock(), block_data)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.optimize!(model)
    dual = MOI.get(model, MOI.NLPBlockDual())
    @test isapprox(dual, [0.178761800, 0.985000823], config)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.optimize!(model)
    dual = MOI.get(model, MOI.NLPBlockDual())
    @test isapprox(dual, [0.0, -5.008488315], config)
    return
end

function setup_test(
    ::typeof(test_nonlinear_hs071_NLPBlockDual),
    model::MOIU.MockOptimizer,
    config::Config,
)
    config.optimal_status = MOI.LOCALLY_SOLVED
    MOI.Utilities.set_mock_optimize!(
        model,
        (mock) -> begin
            MOI.Utilities.mock_optimize!(
                mock,
                config.optimal_status,
                [1.1, 1.57355205213644, 2.6746837915674373, 5.4],
            )
            MOI.set(mock, MOI.NLPBlockDual(), [0.178761800, 0.985000823])
        end,
        (mock) -> begin
            MOI.Utilities.mock_optimize!(
                mock,
                config.optimal_status,
                [
                    4.567633003532132,
                    1.6613736769916652,
                    1.7612041983937636,
                    3.6434497419346723,
                ],
            )
            MOI.set(mock, MOI.NLPBlockDual(), [0.0, -5.008488315])
        end,
    )
    return
end

"""
    test_nonlinear_objective_and_moi_objective_test(
        model::MOI.ModelLike,
        config::Config,
    )

Test that nonlinear objectives take precedence over MOI.ObjectiveFunction.
"""
function test_nonlinear_objective_and_moi_objective_test(
    model::MOI.ModelLike,
    config::Config,
)
    @requires MOI.supports(model, MOI.NLPBlock())
    @requires _supports(config, MOI.optimize!)
    @requires MOI.supports(model, MOI.VariablePrimalStart(), MOI.VariableIndex)
    lb = [1.0]
    ub = [2.0]
    block_data = MOI.NLPBlockData(
        MOI.NLPBoundsPair.(lb, ub),
        FeasibilitySenseEvaluator(true),
        true,
    )
    x = MOI.add_variable(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 1
    # Avoid starting at zero because it's a critial point.
    MOI.set(model, MOI.VariablePrimalStart(), x, 1.5)
    MOI.set(model, MOI.NLPBlock(), block_data)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    f_x = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0)
    # This objective function should be ignored.
    MOI.set(model, MOI.ObjectiveFunction{typeof(f_x)}(), f_x)
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
    @test MOI.get(model, MOI.ResultCount()) >= 1
    @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
    @test isapprox(MOI.get(model, MOI.ObjectiveValue()), 0.0, config)
    xv = MOI.get(model, MOI.VariablePrimal(), x)
    @test isapprox(abs(xv), 1.0, config)
    return
end

function setup_test(
    ::typeof(test_nonlinear_objective_and_moi_objective_test),
    model::MOIU.MockOptimizer,
    config::Config,
)
    config.optimal_status = MOI.LOCALLY_SOLVED
    MOI.Utilities.set_mock_optimize!(
        model,
        (mock) -> begin
            MOI.Utilities.mock_optimize!(mock, config.optimal_status, [-1.0]),
            MOI.set(mock, MOI.ObjectiveValue(), 0.0)
        end,
    )
    flag = model.eval_objective_value
    model.eval_objective_value = false
    return () -> model.eval_objective_value = flag
end

"""
    test_nonlinear_without_objective(model::MOI.ModelLike, config::Config)

Test a nonlinear problem without an objective.
"""
function test_nonlinear_without_objective(model::MOI.ModelLike, config::Config)
    @requires MOI.supports(model, MOI.NLPBlock())
    @requires _supports(config, MOI.optimize!)
    @requires MOI.supports(model, MOI.VariablePrimalStart(), MOI.VariableIndex)
    lb = [1.0]
    ub = [2.0]
    block_data = MOI.NLPBlockData(
        MOI.NLPBoundsPair.(lb, ub),
        FeasibilitySenseEvaluator(true),
        false,
    )
    x = MOI.add_variable(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 1
    # Avoid starting at zero because it's a critial point.
    MOI.set(model, MOI.VariablePrimalStart(), x, 1.5)
    MOI.set(model, MOI.NLPBlock(), block_data)
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
    @test MOI.get(model, MOI.ResultCount()) >= 1
    @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
    @test isapprox(MOI.get(model, MOI.ObjectiveValue()), 0.0, config)
    xv = MOI.get(model, MOI.VariablePrimal(), x)
    @test isapprox(abs(xv), 1.0, config)
    return
end

function setup_test(
    ::typeof(test_nonlinear_without_objective),
    model::MOIU.MockOptimizer,
    config::Config,
)
    config.optimal_status = MOI.LOCALLY_SOLVED
    MOI.Utilities.set_mock_optimize!(
        model,
        (mock) -> begin
            MOI.Utilities.mock_optimize!(mock, config.optimal_status, [-1.0]),
            MOI.set(mock, MOI.ObjectiveValue(), 0.0)
        end,
    )
    flag = model.eval_objective_value
    model.eval_objective_value = false
    return () -> model.eval_objective_value = flag
end

"""
    test_nonlinear_mixed_complementarity(model::MOI.ModelLike, config::Config)

Test the solution of the linear mixed-complementarity problem:
`F(x) complements x`, where `F(x) = M * x .+ q` and `0 <= x <= 10`.
"""
function test_nonlinear_mixed_complementarity(
    model::MOI.ModelLike,
    config::Config,
)
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.Complements,
    )
    @requires _supports(config, MOI.optimize!)
    x = MOI.add_variables(model, 4)
    MOI.add_constraint.(model, MOI.SingleVariable.(x), MOI.Interval(0.0, 10.0))
    MOI.set.(model, MOI.VariablePrimalStart(), x, 0.0)
    M = Float64[0 0 -1 -1; 0 0 1 -2; 1 -1 2 -2; 1 2 -2 4]
    q = [2; 2; -2; -6]
    terms = MOI.VectorAffineTerm{Float64}[]
    for i in 1:4
        push!(
            terms,
            MOI.VectorAffineTerm(4 + i, MOI.ScalarAffineTerm(1.0, x[i])),
        )
        for j in 1:4
            iszero(M[i, j]) && continue
            push!(
                terms,
                MOI.VectorAffineTerm(i, MOI.ScalarAffineTerm(M[i, j], x[j])),
            )
        end
    end
    MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(terms, [q; 0.0; 0.0; 0.0; 0.0]),
        MOI.Complements(8),
    )
    @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
    x_val = MOI.get.(model, MOI.VariablePrimal(), x)
    @test isapprox(x_val, [2.8, 0.0, 0.8, 1.2], config)
    return
end

function setup_test(
    ::typeof(test_nonlinear_mixed_complementarity),
    model::MOIU.MockOptimizer,
    config::Config,
)
    config.optimal_status = MOI.LOCALLY_SOLVED
    MOI.Utilities.set_mock_optimize!(
        model,
        (mock) -> MOI.Utilities.mock_optimize!(
            mock,
            config.optimal_status,
            [2.8, 0.0, 0.8, 1.2],
        ),
    )
    return
end

"""
    test_nonlinear_qp_complementarity_constraint(
        model::MOI.ModelLike,
        config::Config,
    )

Test the solution of the quadratic program with complementarity constraints:

```
    min  (x0 - 5)^2 +(2 x1 + 1)^2
    s.t.  -1.5 x0 + 2 x1 + x2 - 0.5 x3 + x4 = 2
        x2 complements(3 x0 - x1 - 3)
        x3 complements(-x0 + 0.5 x1 + 4)
        x4 complements(-x0 - x1 + 7)
        x0, x1, x2, x3, x4 >= 0

```
which rewrites, with auxiliary variables

```
    min  (x0 - 5)^2 +(2 x1 + 1)^2
    s.t.  -1.5 x0 + 2 x1 + x2 - 0.5 x3 + x4 = 2  (cf1)
        3 x0 - x1 - 3 - x5 = 0                 (cf2)
        -x0 + 0.5 x1 + 4 - x6 = 0              (cf3)
        -x0 - x1 + 7 - x7 = 0                  (cf4)
        x2 complements x5
        x3 complements x6
        x4 complements x7
        x0, x1, x2, x3, x4, x5, x6, x7 >= 0

```
"""
function test_nonlinear_qp_complementarity_constraint(
    model::MOI.ModelLike,
    config::Config,
)
    @requires MOI.supports_constraint(
        model,
        MOI.VectorOfVariables,
        MOI.Complements,
    )
    @requires _supports(config, MOI.optimize!)
    x = MOI.add_variables(model, 8)
    MOI.set.(model, MOI.VariablePrimalStart(), x, 0.0)
    MOI.add_constraint.(model, MOI.SingleVariable.(x), MOI.GreaterThan(0.0))
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{Float64}}(),
        MOI.ScalarQuadraticFunction(
            MOI.ScalarQuadraticTerm.([2.0, 8.0], x[1:2], x[1:2]),
            MOI.ScalarAffineTerm.([-10.0, 4.0], x[[1, 2]]),
            26.0,
        ),
    )
    MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([-1.5, 2.0, 1.0, 0.5, 1.0], x[1:5]),
            0.0,
        ),
        MOI.EqualTo(2.0),
    )
    MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([3.0, -1.0, -1.0], x[[1, 2, 6]]),
            0.0,
        ),
        MOI.EqualTo(3.0),
    )
    MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([-1.0, 0.5, -1.0], x[[1, 2, 7]]),
            0.0,
        ),
        MOI.EqualTo(-4.0),
    )
    MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.(-1.0, x[[1, 2, 8]]),
            0.0,
        ),
        MOI.EqualTo(-7.0),
    )
    MOI.add_constraint(
        model,
        MOI.VectorOfVariables([x[3], x[4], x[5], x[6], x[7], x[8]]),
        MOI.Complements(6),
    )
    @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
    x_val = MOI.get.(model, MOI.VariablePrimal(), x)
    @test isapprox(x_val, [1.0, 0.0, 3.5, 0.0, 0.0, 0.0, 3.0, 6.0], config)
    @test isapprox(MOI.get(model, MOI.ObjectiveValue()), 17.0, config)
    return
end

function setup_test(
    ::typeof(test_nonlinear_qp_complementarity_constraint),
    model::MOIU.MockOptimizer,
    config::Config,
)
    config.optimal_status = MOI.LOCALLY_SOLVED
    MOI.Utilities.set_mock_optimize!(
        model,
        (mock) -> begin
            MOI.Utilities.mock_optimize!(
                mock,
                config.optimal_status,
                [1.0, 0.0, 3.5, 0.0, 0.0, 0.0, 3.0, 6.0],
            )
            MOI.set(mock, MOI.ObjectiveValue(), 17.0)
        end,
    )
    return
end

"""
    test_nonlinear_HS071_internal(::MOI.ModelLike, ::Config)

A test for the correctness of the HS071 evaluator.

This is mainly for the internal purpose of checking their correctness as
written. External solvers can exclude this test without consequence.
"""
function test_nonlinear_HS071_internal(::MOI.ModelLike, ::Config)
    d = MOI.Test.HS071(true, true)
    @test MOI.objective_expr(d) == :(
        x[$(MOI.VariableIndex(1))] *
        x[$(MOI.VariableIndex(4))] *
        (
            x[$(MOI.VariableIndex(1))] +
            x[$(MOI.VariableIndex(2))] +
            x[$(MOI.VariableIndex(3))]
        ) + x[$(MOI.VariableIndex(3))]
    )
    @test MOI.constraint_expr(d, 1) == :(
        x[$(MOI.VariableIndex(1))] *
        x[$(MOI.VariableIndex(2))] *
        x[$(MOI.VariableIndex(3))] *
        x[$(MOI.VariableIndex(4))] >= 25.0
    )
    @test MOI.constraint_expr(d, 2) == :(
        x[$(MOI.VariableIndex(1))]^2 +
        x[$(MOI.VariableIndex(2))]^2 +
        x[$(MOI.VariableIndex(3))]^2 +
        x[$(MOI.VariableIndex(4))]^2 == 40.0
    )
    @test_throws ErrorException MOI.constraint_expr(d, 3)
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
    return
end

"""
    test_nonlinear_Feasibility_internal(::MOI.ModelLike, ::Config)

A test for the correctness of the FeasibilitySenseEvaluator evaluator.

This is mainly for the internal purpose of checking their correctness as
written. External solvers can exclude this test without consequence.
"""
function test_nonlinear_Feasibility_internal(::MOI.ModelLike, ::Config)
    d = MOI.Test.FeasibilitySenseEvaluator(true)
    @test MOI.objective_expr(d) == :()
    @test MOI.constraint_expr(d, 1) == :(x[$(MOI.VariableIndex(1))]^2 == 1.0)
    @test_throws AssertionError MOI.constraint_expr(d, 2)
    MOI.initialize(d, [:Grad, :Jac, :ExprGraph, :Hess])
    @test :Hess in MOI.features_available(d)
    x = [1.5]
    # f(x)
    @test MOI.eval_objective(d, x) == 0.0
    # g(x)
    g = zeros(1)
    MOI.eval_constraint(d, g, x)
    @test g == [1.5^2]
    # f'(x)
    ∇f = fill(NaN, length(x))
    MOI.eval_objective_gradient(d, ∇f, x)
    @test ∇f == [0.0]
    # Jacobian
    Js = MOI.jacobian_structure(d)
    J = fill(NaN, length(Js))
    MOI.eval_constraint_jacobian(d, J, x)
    @test J == [3.0]
    # Hessian-lagrangian
    Hs = MOI.hessian_lagrangian_structure(d)
    H = fill(NaN, length(Hs))
    MOI.eval_hessian_lagrangian(d, H, x, 1.0, [1.1])
    @test H == [2.2]
    return
end

"""
    InvalidEvaluator()

An AbstractNLPEvaluator for the problem: `min NaN`, where `eval_objective`
returns a `NaN`.

Solvers should return a `TerminationStatus` of `MOI.INVALID_MODEL`.
"""
struct InvalidEvaluator <: MOI.AbstractNLPEvaluator end

function MOI.initialize(d::InvalidEvaluator, requested_features::Vector{Symbol})
    for feat in requested_features
        @assert feat in MOI.features_available(d)
    end
    return
end

function MOI.features_available(::InvalidEvaluator)
    return [:Grad, :ExprGraph]
end

MOI.objective_expr(::InvalidEvaluator) = :(NaN)

MOI.eval_objective(::InvalidEvaluator, x) = NaN

function MOI.eval_objective_gradient(::InvalidEvaluator, grad_f, x)
    grad_f[1] = NaN
    return
end

"""
    test_nonlinear_InvalidEvaluator_internal(::MOI.ModelLike, ::Config)

A test for the correctness of the InvalidEvaluator evaluator.

This is mainly for the internal purpose of checking their correctness as
written. External solvers can exclude this test without consequence.
"""
function test_nonlinear_InvalidEvaluator_internal(::MOI.ModelLike, ::Config)
    d = InvalidEvaluator()
    @test MOI.objective_expr(d) == :(NaN)
    MOI.initialize(d, [:Grad, :ExprGraph])
    x = [1.5]
    # f(x)
    @test isnan(MOI.eval_objective(d, x))
    # f'(x)
    ∇f = fill(NaN, length(x))
    MOI.eval_objective_gradient(d, ∇f, x)
    @test isnan(∇f[1])
    return
end

"""
    test_nonlinear_invalid(model::MOI.ModelLike, config::Config)

Test that a nonlinear model returns the TerminationStatus `INVALID_MODEL` if a
NaN is detected in a function evaluation.
"""
function test_nonlinear_invalid(model::MOI.ModelLike, config::Config)
    @requires MOI.supports(model, MOI.NLPBlock())
    @requires _supports(config, MOI.optimize!)
    MOI.add_variable(model)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.set(
        model,
        MOI.NLPBlock(),
        MOI.NLPBlockData(MOI.NLPBoundsPair[], InvalidEvaluator(), true),
    )
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == MOI.INVALID_MODEL
    return
end

function setup_test(
    ::typeof(test_nonlinear_invalid),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOI.Utilities.set_mock_optimize!(
        model,
        mock -> MOI.Utilities.mock_optimize!(mock, MOI.INVALID_MODEL, [NaN]),
    )
    return
end
