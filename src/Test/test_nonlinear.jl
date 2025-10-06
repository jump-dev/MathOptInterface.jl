# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    HS071(
        enable_hessian::Bool,
        enable_hessian_vector_product::Bool = false,
    )

An [`MOI.AbstractNLPEvaluator`](@ref) for the problem:

```math
\\begin{aligned}
\\text{min}       \\ & x_1 * x_4 * (x_1 + x_2 + x_3) + x_3 \\\\
\\text{subject to}\\ & x_1 * x_2 * x_3 * x_4 \\ge 25 \\\\
                     & x_1^2 + x_2^2 + x_3^2 + x_4^2 = 40 \\\\
                     & 1 \\le x_1, x_2, x_3, x_4 \\le 5
\\end{aligned}
```

The optimal solution is `[1.000, 4.743, 3.821, 1.379]`.
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
        end
    end
    return
end

function MOI.features_available(d::HS071)
    features = [:Grad, :Jac, :JacVec, :ExprGraph]
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

function MOI.constraint_gradient_structure(::HS071, ::Int)
    return [1, 2, 3, 4]
end

function MOI.eval_constraint_gradient(::HS071, ∇g, x, i)
    @assert 1 <= i <= 2
    if i == 1
        ∇g[1] = x[2] * x[3] * x[4]  # 1,1
        ∇g[2] = x[1] * x[3] * x[4]  # 1,2
        ∇g[3] = x[1] * x[2] * x[4]  # 1,3
        ∇g[4] = x[1] * x[2] * x[3]  # 1,4
    else
        ∇g[1] = 2 * x[1]  # 2,1
        ∇g[2] = 2 * x[2]  # 2,2
        ∇g[3] = 2 * x[3]  # 2,3
        ∇g[4] = 2 * x[4]  # 2,4
    end
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

function MOI.eval_constraint_jacobian_product(d::HS071, y, x, w)
    y .= zero(eltype(y))
    indices = MOI.jacobian_structure(d)
    J = zeros(length(indices))
    MOI.eval_constraint_jacobian(d, J, x)
    for ((i, j), val) in zip(indices, J)
        y[i] += val * w[j]
    end
    return
end

function MOI.eval_constraint_jacobian_transpose_product(::HS071, y, x, w)
    y[1] = (x[2] * x[3] * x[4]) * w[1] + (2 * x[1]) * w[2]
    y[2] = (x[1] * x[3] * x[4]) * w[1] + (2 * x[2]) * w[2]
    y[3] = (x[1] * x[2] * x[4]) * w[1] + (2 * x[3]) * w[2]
    y[4] = (x[1] * x[2] * x[3]) * w[1] + (2 * x[4]) * w[2]
    return
end

function MOI.hessian_objective_structure(d::HS071)
    return [(1, 1), (2, 1), (3, 1), (4, 1), (4, 2), (4, 3)]
end

function MOI.eval_hessian_objective(d::HS071, H, x)
    @assert d.enable_hessian
    H[1] = 2 * x[4]                 # 1,1
    H[2] = x[4]                     # 2,1
    H[3] = x[4]                     # 3,1
    H[4] = 2 * x[1] + x[2] + x[3]   # 4,1
    H[5] = x[1]                     # 4,2
    H[6] = x[1]                     # 4,3
    return
end

function MOI.hessian_constraint_structure(d::HS071, i::Int)
    @assert 1 <= i <= 2
    if i == 1
        return [(2, 1), (3, 1), (3, 2), (4, 1), (4, 2), (4, 3)]
    else
        return [(1, 1), (2, 2), (3, 3), (4, 4)]
    end
end

function MOI.eval_hessian_constraint(d::HS071, H, x, i)
    @assert d.enable_hessian
    if i == 1
        H[1] = x[3] * x[4]  # 2,1
        H[2] = x[2] * x[4]  # 3,1
        H[3] = x[1] * x[4]  # 3,2
        H[4] = x[2] * x[3]  # 4,1
        H[5] = x[1] * x[3]  # 4,2
        H[6] = x[1] * x[2]  # 4,3
    else
        H[1] = 2.0  # 1,1
        H[2] = 2.0  # 2,2
        H[3] = 2.0  # 3,3
        H[4] = 2.0  # 4,4
    end
    return
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
    H[10] += μ[2] * 2 # 4,4
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
    return :(x[$(MOI.VariableIndex(1))]^2.0 == 1.0)
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

function _test_HS071(
    model::MOI.ModelLike,
    config::Config{T},
    evaluator::HS071,
) where {T}
    @requires T == Float64
    @requires MOI.supports(model, MOI.NLPBlock())
    @requires _supports(config, MOI.optimize!)
    @requires MOI.supports_constraint(
        model,
        MOI.VariableIndex,
        MOI.LessThan{Float64},
    )
    @requires MOI.supports_constraint(
        model,
        MOI.VariableIndex,
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
        cub = MOI.add_constraint(model, v[i], MOI.LessThan(u[i]))
        # We test this after the creation of every `VariableIndex` constraint
        # to ensure a good coverage of corner cases.
        @test cub.value == v[i].value
        clb = MOI.add_constraint(model, v[i], MOI.GreaterThan(l[i]))
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
    config::Config{T},
) where {T}
    if T != Float64
        return  # Skip for non-Float64 solvers
    end
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
function test_nonlinear_hs071_no_hessian(model, config::Config)
    _test_HS071(model, config, HS071(false))
    return
end

function setup_test(
    ::typeof(test_nonlinear_hs071_no_hessian),
    model::MOIU.MockOptimizer,
    config::Config{T},
) where {T}
    if T != Float64
        return  # Skip for non-Float64 solvers
    end
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
    config::Config{T},
) where {T}
    if T != Float64
        return  # Skip for non-Float64 solvers
    end
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
function test_nonlinear_hs071_NLPBlockDual(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires T == Float64
    @requires MOI.supports(model, MOI.NLPBlock())
    @requires _supports(config, MOI.optimize!)
    @requires MOI.supports(model, MOI.VariablePrimalStart(), MOI.VariableIndex)
    @requires _supports(config, MOI.NLPBlockDual)
    n_variables = 4
    v = MOI.add_variables(model, n_variables)
    l = [1.1, 1.2, 1.3, 1.4]
    u = [5.1, 5.2, 5.3, 5.4]
    start = [2.1, 2.2, 2.3, 2.4]
    cl = MOI.add_constraint.(model, v, MOI.GreaterThan.(l))
    cu = MOI.add_constraint.(model, v, MOI.LessThan.(u))
    MOI.set.(model, MOI.VariablePrimalStart(), v, start)
    lb, ub = [25.0, 40.0], [Inf, 40.0]
    evaluator = HS071(true)
    block_data = MOI.NLPBlockData(MOI.NLPBoundsPair.(lb, ub), evaluator, true)
    MOI.set(model, MOI.NLPBlock(), block_data)

    for sense in [MOI.MIN_SENSE, MOI.MAX_SENSE]
        MOI.set(model, MOI.ObjectiveSense(), sense)
        MOI.optimize!(model)
        # Get primal solution
        x = MOI.get(model, MOI.VariablePrimal(), v)
        # Get dual solution
        con_dual = MOI.get(model, MOI.NLPBlockDual())
        var_dual_lb = MOI.get(model, MOI.ConstraintDual(), cl)
        var_dual_ub = MOI.get(model, MOI.ConstraintDual(), cu)

        # Evaluate ∇f(x)
        g = zeros(n_variables)
        MOI.eval_objective_gradient(evaluator, g, x)
        # Evaluate ∑ μᵢ * ∇cᵢ(x)
        jtv = zeros(n_variables)
        MOI.eval_constraint_jacobian_transpose_product(
            evaluator,
            jtv,
            x,
            con_dual,
        )

        # Test that (x, μ, νl, νᵤ) satisfies stationarity condition
        # σ ∇f(x) - ∑ μᵢ * ∇cᵢ(x) - νₗ - νᵤ = 0
        σ = (sense == MOI.MAX_SENSE) ? -1.0 : 1.0
        @test isapprox(σ .* g, jtv .+ var_dual_ub .+ var_dual_lb, config)
    end
    return
end

function setup_test(
    ::typeof(test_nonlinear_hs071_NLPBlockDual),
    model::MOIU.MockOptimizer,
    config::Config{T},
) where {T}
    if T != Float64
        return  # Skip for non-Float64 solvers
    end
    config.optimal_status = MOI.LOCALLY_SOLVED
    MOI.Utilities.set_mock_optimize!(
        model,
        (mock) -> begin
            MOI.Utilities.mock_optimize!(
                mock,
                config.optimal_status,
                [1.1, 1.5735520521364397, 2.6746837915674373, 5.4],
                (MOI.VariableIndex, MOI.GreaterThan{Float64}) =>
                    [28.590703804861093, 0.0, 0.0, 0.0],
                (MOI.VariableIndex, MOI.LessThan{Float64}) =>
                    [0.0, 0.0, 0.0, -5.582550550234756],
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
                (MOI.VariableIndex, MOI.GreaterThan{Float64}) =>
                    [0.0, 0.0, 0.0, 0.0],
                (MOI.VariableIndex, MOI.LessThan{Float64}) =>
                    [0.0, 0.0, 0.0, 0.0],
            )
            MOI.set(mock, MOI.NLPBlockDual(), [0.0, -5.008488315])
        end,
    )
    model.eval_variable_constraint_dual = false
    return () -> model.eval_variable_constraint_dual = true
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
    config::Config{T},
) where {T}
    @requires T == Float64
    @requires MOI.supports(model, MOI.NLPBlock())
    @requires _supports(config, MOI.optimize!)
    @requires MOI.supports(model, MOI.VariablePrimalStart(), MOI.VariableIndex)
    lb = [1.0]
    ub = [1.0]
    block_data = MOI.NLPBlockData(
        MOI.NLPBoundsPair.(lb, ub),
        FeasibilitySenseEvaluator(true),
        true,
    )
    x = MOI.add_variable(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 1
    # Avoid starting at zero because it's a critical point.
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
    config::Config{T},
) where {T}
    if T != Float64
        return  # Skip for non-Float64 solvers
    end
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
    test_nonlinear_objective(model::MOI.ModelLike, config::Config)

Test a nonlinear objective only.
"""
function test_nonlinear_objective(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires T == Float64
    @requires MOI.supports(model, MOI.NLPBlock())
    @requires _supports(config, MOI.optimize!)
    @requires MOI.supports(model, MOI.VariablePrimalStart(), MOI.VariableIndex)
    lb = [1.0]
    ub = [1.0]
    block_data = MOI.NLPBlockData(
        MOI.NLPBoundsPair.(lb, ub),
        FeasibilitySenseEvaluator(true),
        true,
    )
    x = MOI.add_variable(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 1
    # Avoid starting at zero because it's a critical point.
    MOI.set(model, MOI.VariablePrimalStart(), x, 1.5)
    MOI.set(model, MOI.NLPBlock(), block_data)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
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
    ::typeof(test_nonlinear_objective),
    model::MOIU.MockOptimizer,
    config::Config{T},
) where {T}
    if T != Float64
        return  # Skip for non-Float64 solvers
    end
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
function test_nonlinear_without_objective(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires T == Float64
    @requires MOI.supports(model, MOI.NLPBlock())
    @requires _supports(config, MOI.optimize!)
    @requires MOI.supports(model, MOI.VariablePrimalStart(), MOI.VariableIndex)
    lb = [1.0]
    ub = [1.0]
    block_data = MOI.NLPBlockData(
        MOI.NLPBoundsPair.(lb, ub),
        FeasibilitySenseEvaluator(true),
        false,
    )
    x = MOI.add_variable(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 1
    # Avoid starting at zero because it's a critical point.
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
    config::Config{T},
) where {T}
    if T != Float64
        return  # Skip for non-Float64 solvers
    end
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
    config::Config{T},
) where {T}
    @requires MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{T},
        MOI.Complements,
    )
    @requires _supports(config, MOI.optimize!)
    x = MOI.add_variables(model, 4)
    MOI.add_constraint.(model, x, MOI.Interval(T(0), T(10)))
    MOI.set.(model, MOI.VariablePrimalStart(), x, T(0))
    M = T[0 0 -1 -1; 0 0 1 -2; 1 -1 2 -2; 1 2 -2 4]
    q = T[2; 2; -2; -6]
    terms = MOI.VectorAffineTerm{T}[]
    for i in 1:4
        term = MOI.ScalarAffineTerm(T(1), x[i])
        push!(terms, MOI.VectorAffineTerm(4 + i, term))
        for j in 1:4
            if iszero(M[i, j])
                continue
            end
            term = MOI.ScalarAffineTerm(M[i, j], x[j])
            push!(terms, MOI.VectorAffineTerm(i, term))
        end
    end
    MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(terms, T[q; 0; 0; 0; 0]),
        MOI.Complements(8),
    )
    @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
    x_val = MOI.get.(model, MOI.VariablePrimal(), x)
    @test isapprox(x_val, T[14//5, 0, 4//5, 6//5], config)
    return
end

function setup_test(
    ::typeof(test_nonlinear_mixed_complementarity),
    model::MOIU.MockOptimizer,
    config::Config{T},
) where {T}
    config.optimal_status = MOI.LOCALLY_SOLVED
    MOI.Utilities.set_mock_optimize!(
        model,
        (mock) -> MOI.Utilities.mock_optimize!(
            mock,
            config.optimal_status,
            T[14//5, 0, 4//5, 6//5],
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
    config::Config{T},
) where {T}
    @requires MOI.supports_constraint(
        model,
        MOI.VectorOfVariables,
        MOI.Complements,
    )
    @requires _supports(config, MOI.optimize!)
    x = MOI.add_variables(model, 8)
    MOI.set.(model, MOI.VariablePrimalStart(), x, T(0))
    MOI.add_constraint.(model, x, MOI.GreaterThan(T(0)))
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{Float64}}(),
        MOI.ScalarQuadraticFunction(
            MOI.ScalarQuadraticTerm.(T[2, 8], x[1:2], x[1:2]),
            MOI.ScalarAffineTerm.(T[-10, 4], x[[1, 2]]),
            T(26),
        ),
    )
    MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.(T[-3//2, 2, 1, 1//2, 1], x[1:5]),
            T(0),
        ),
        MOI.EqualTo(T(2)),
    )
    MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.(T[3, -1, -1], x[[1, 2, 6]]),
            T(0),
        ),
        MOI.EqualTo(T(3)),
    )
    MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.(T[-1, 1//2, -1], x[[1, 2, 7]]),
            T(0),
        ),
        MOI.EqualTo(T(-4)),
    )
    MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.(T(-1), x[[1, 2, 8]]),
            T(0),
        ),
        MOI.EqualTo(T(-7)),
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
    @test isapprox(x_val, T[1, 0, 7//2, 0, 0, 0, 3, 6], config)
    @test isapprox(MOI.get(model, MOI.ObjectiveValue()), T(17), config)
    return
end

function setup_test(
    ::typeof(test_nonlinear_qp_complementarity_constraint),
    model::MOIU.MockOptimizer,
    config::Config{T},
) where {T}
    config.optimal_status = MOI.LOCALLY_SOLVED
    MOI.Utilities.set_mock_optimize!(
        model,
        (mock) -> begin
            MOI.Utilities.mock_optimize!(
                mock,
                config.optimal_status,
                T[1, 0, 7//2, 0, 0, 0, 3, 6],
            )
            MOI.set(mock, MOI.ObjectiveValue(), T(17))
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
    d = HS071(true, true)
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
    # Hessian-Lagrangian
    Hs = MOI.hessian_lagrangian_structure(d)
    H = fill(NaN, length(Hs))
    MOI.eval_hessian_lagrangian(d, H, x, 1.0, [1.0, 1.0])
    @test H == [4, 2, 2, 2, 1, 2, 5, 2, 2, 2]
    # Hessian-Lagrangian-product
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
    d = FeasibilitySenseEvaluator(true)
    @test MOI.objective_expr(d) == :()
    @test MOI.constraint_expr(d, 1) == :(x[$(MOI.VariableIndex(1))]^2.0 == 1.0)
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
    # Hessian-Lagrangian
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

MOI.objective_expr(::InvalidEvaluator) = :(+($NaN))

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
    @test MOI.objective_expr(d) == :(+($NaN))
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
function test_nonlinear_invalid(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires T == Float64
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
    ::Config{T},
) where {T}
    if T != Float64
        return  # Skip for non-Float64 solvers
    end
    MOI.Utilities.set_mock_optimize!(
        model,
        mock -> MOI.Utilities.mock_optimize!(mock, MOI.INVALID_MODEL, [NaN]),
    )
    return
end

function test_nonlinear_expression_hs071(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires T == Float64
    @requires _supports(config, MOI.optimize!)
    F = MOI.ScalarNonlinearFunction
    @requires MOI.supports(model, MOI.ObjectiveFunction{F}())
    @requires MOI.supports_constraint(model, F, MOI.GreaterThan{Float64})
    MOI.Utilities.loadfromstring!(
        model,
        """
variables: w, x, y, z
minobjective: ScalarNonlinearFunction(w * z * (w + x + y) + y)
c2: ScalarNonlinearFunction(w * x * y * z) >= 25.0
c3: ScalarNonlinearFunction(w^2 + x^2 + y^2 + z^2) == 40.0
w in Interval(1.0, 5.0)
x in Interval(1.0, 5.0)
y in Interval(1.0, 5.0)
z in Interval(1.0, 5.0)
""",
    )
    x = MOI.get(model, MOI.ListOfVariableIndices())
    MOI.set.(model, MOI.VariablePrimalStart(), x, [1.0, 5.0, 5.0, 1.0])
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
    @test ≈(
        MOI.get.(model, MOI.VariablePrimal(), x),
        [1.0, 4.742999, 3.821150, 1.379408],
        config,
    )
    return
end

function setup_test(
    ::typeof(test_nonlinear_expression_hs071),
    model::MOIU.MockOptimizer,
    config::Config{T},
) where {T}
    MOI.Utilities.set_mock_optimize!(
        model,
        (mock) -> begin
            MOI.Utilities.mock_optimize!(
                mock,
                config.optimal_status,
                [1.0, 4.742999, 3.821150, 1.379408],
            )
        end,
    )
    return
end

version_added(::typeof(test_nonlinear_expression_hs071)) = v"1.17.0"

function test_nonlinear_expression_hs071_epigraph(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires T == Float64
    @requires _supports(config, MOI.optimize!)
    F = MOI.ScalarNonlinearFunction
    @requires MOI.supports(model, MOI.ObjectiveFunction{F}())
    @requires MOI.supports_constraint(model, F, MOI.GreaterThan{Float64})
    MOI.Utilities.loadfromstring!(
        model,
        """
variables: t, w, x, y, z
minobjective: t
c1: ScalarNonlinearFunction(t - (w * z * (w + x + y) + y)) >= 0.0
c2: ScalarNonlinearFunction(w * x * y * z) >= 25.0
c3: ScalarNonlinearFunction(w^2 + x^2 + y^2 + z^2) == 40.0
w in Interval(1.0, 5.0)
x in Interval(1.0, 5.0)
y in Interval(1.0, 5.0)
z in Interval(1.0, 5.0)
""",
    )
    x = MOI.get(model, MOI.ListOfVariableIndices())
    MOI.set.(model, MOI.VariablePrimalStart(), x, [16.0, 1.0, 5.0, 5.0, 1.0])
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
    @test ≈(
        MOI.get.(model, MOI.VariablePrimal(), x),
        [17.014013643792, 1.0, 4.742999, 3.821150, 1.379408],
        config,
    )
    return
end

function setup_test(
    ::typeof(test_nonlinear_expression_hs071_epigraph),
    model::MOIU.MockOptimizer,
    config::Config{T},
) where {T}
    MOI.Utilities.set_mock_optimize!(
        model,
        (mock) -> begin
            MOI.Utilities.mock_optimize!(
                mock,
                config.optimal_status,
                [17.014013643792, 1.0, 4.742999, 3.821150, 1.379408],
            )
        end,
    )
    return
end

version_added(::typeof(test_nonlinear_expression_hs071_epigraph)) = v"1.17.0"

function test_nonlinear_expression_hs109(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires T == Float64
    @requires _supports(config, MOI.optimize!)
    F = MOI.ScalarNonlinearFunction
    @requires MOI.supports(model, MOI.ObjectiveFunction{F}())
    @requires MOI.supports_constraint(model, F, MOI.GreaterThan{Float64})
    MOI.Utilities.loadfromstring!(
        model,
        """
variables: x1, x2, x3, x4, x5, x6, x7, x8, x9
minobjective: ScalarNonlinearFunction(3 * x1 + 1e-6 * x1^3 + 2 * x2 + 0.522074e-6 * x2^3)
x1 >= 0.0
x2 >= 0.0
x3 in Interval(-0.55, 0.55)
x4 in Interval(-0.55, 0.55)
x5 in Interval(196.0, 252.0)
x6 in Interval(196.0, 252.0)
x7 in Interval(196.0, 252.0)
x8 in Interval(-400.0, 800.0)
x9 in Interval(-400.0, 800.0)
c1: ScalarNonlinearFunction(x4 - x3 + 0.55) >= 0.0
c2: ScalarNonlinearFunction(x3 - x4 + 0.55) >= 0.0
c3: ScalarNonlinearFunction(2250000 - x1^2 - x8^2) >= 0.0
c4: ScalarNonlinearFunction(2250000 - x2^2 - x9^2) >= 0.0
c5: ScalarNonlinearFunction(x5 * x6 * sin(-x3 - 0.25) + x5 * x7 * sin(-x4 - 0.25) + 2 * 0.24740395925452294 * x5^2 - 50.176 * x1 + 400 * 50.176) == 0.0
c6: ScalarNonlinearFunction(x5 * x6 * sin(x3 - 0.25) + x6 * x7 * sin(x3 - x4 - 0.25) + 2 * 0.24740395925452294 * x6^2 - 50.176 * x2 + 400 * 50.176) == 0.0
c7: ScalarNonlinearFunction(x5 * x7 * sin(x4 - 0.25) + x6 * x7 * sin(x4 - x3 - 0.25) + 2 * 0.24740395925452294 * x7^2 + 881.779 * 50.176) == 0.0
c8: ScalarNonlinearFunction(50.176 * x8 + x5 * x6 * cos(-x3 - 0.25) + x5 * x7 * cos(-x4 - 0.25) - 200 * 50.176 - 2 * 0.9689124217106447 * x5^2 + 0.7533e-3 * 50.176 * x5^2) == 0.0
c9: ScalarNonlinearFunction(50.176 * x9 + x5 * x6 * cos(x3 - 0.25) + x6 * x7 * cos(x3 - x4 - 0.25) - 2 * 0.9689124217106447 * x6^2 + 0.7533e-3 * 50.176 * x6^2 - 200 * 50.176) == 0.0
c10: ScalarNonlinearFunction(x5 * x7 * cos(x4 - 0.25) + x6 * x7 * cos(x4 - x3 - 0.25) - 2 * 0.9689124217106447 * x7^2 + 22.938 * 50.176 + 0.7533e-3 * 50.176 * x7^2) == 0.0
""",
    )
    x = MOI.get(model, MOI.ListOfVariableIndices())
    MOI.set.(model, MOI.VariablePrimalStart(), x, 0.0)
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
    @test ≈(MOI.get(model, MOI.ObjectiveValue()), 5326.851310161077, config)
    return
end

function setup_test(
    ::typeof(test_nonlinear_expression_hs109),
    model::MOIU.MockOptimizer,
    config::Config{T},
) where {T}
    flag = model.eval_objective_value
    model.eval_objective_value = false
    MOI.Utilities.set_mock_optimize!(
        model,
        (mock) -> begin
            MOI.Utilities.mock_optimize!(
                mock,
                config.optimal_status,
                MOI.FEASIBLE_POINT,
            )
            MOI.set(mock, MOI.ObjectiveValue(), 5326.851310161077)
        end,
    )
    return () -> model.eval_objective_value = flag
end

version_added(::typeof(test_nonlinear_expression_hs109)) = v"1.17.0"

function test_nonlinear_expression_hs110(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires T == Float64
    @requires _supports(config, MOI.optimize!)
    F = MOI.ScalarNonlinearFunction
    @requires MOI.supports(model, MOI.ObjectiveFunction{F}())
    f = "log(x1 - 2)^2 + log(10 - x1)^2"
    for i in 2:10
        f *= " + log(x$i - 2)^2 + log(10 - x$i)^2"
    end
    f *= " - (x1 * x2 * x3 * x4 * x5 * x6 * x7 * x8 * x9 * x10)^0.2"
    MOI.Utilities.loadfromstring!(
        model,
        """
        variables: x1, x2, x3, x4, x5, x6, x7, x8, x9, x10
        minobjective: ScalarNonlinearFunction($f)
        x1 in Interval(2.001, 9.999)
        x2 in Interval(2.001, 9.999)
        x3 in Interval(2.001, 9.999)
        x4 in Interval(2.001, 9.999)
        x5 in Interval(2.001, 9.999)
        x6 in Interval(2.001, 9.999)
        x7 in Interval(2.001, 9.999)
        x8 in Interval(2.001, 9.999)
        x9 in Interval(2.001, 9.999)
        x10 in Interval(2.001, 9.999)
        """,
    )
    x = MOI.get(model, MOI.ListOfVariableIndices())
    MOI.set.(model, MOI.VariablePrimalStart(), x, 9.0)
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
    @test ≈(MOI.get(model, MOI.ObjectiveValue()), -45.77846971, config)
    return
end

function setup_test(
    ::typeof(test_nonlinear_expression_hs110),
    model::MOIU.MockOptimizer,
    config::Config{T},
) where {T}
    flag = model.eval_objective_value
    model.eval_objective_value = false
    MOI.Utilities.set_mock_optimize!(
        model,
        (mock) -> begin
            MOI.Utilities.mock_optimize!(
                mock,
                config.optimal_status,
                MOI.FEASIBLE_POINT,
            )
            MOI.set(mock, MOI.ObjectiveValue(), -45.77846971)
        end,
    )
    return () -> model.eval_objective_value = flag
end

version_added(::typeof(test_nonlinear_expression_hs110)) = v"1.17.0"

function test_nonlinear_expression_quartic(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires T == Float64
    @requires _supports(config, MOI.optimize!)
    F = MOI.ScalarNonlinearFunction
    @requires MOI.supports(model, MOI.ObjectiveFunction{F}())
    MOI.Utilities.loadfromstring!(
        model,
        """
variables: x
minobjective: ScalarNonlinearFunction(x^4)
x >= -1.0
""",
    )
    x = MOI.get(model, MOI.ListOfVariableIndices())
    MOI.set.(model, MOI.VariablePrimalStart(), x, [-0.5])
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
    @test ≈(MOI.get(model, MOI.ObjectiveValue()), 0.0, config)
    return
end

function setup_test(
    ::typeof(test_nonlinear_expression_quartic),
    model::MOIU.MockOptimizer,
    config::Config{T},
) where {T}
    flag = model.eval_objective_value
    model.eval_objective_value = false
    MOI.Utilities.set_mock_optimize!(
        model,
        (mock) -> begin
            MOI.Utilities.mock_optimize!(
                mock,
                config.optimal_status,
                MOI.FEASIBLE_POINT,
            )
            MOI.set(mock, MOI.ObjectiveValue(), 0.0)
        end,
    )
    return () -> model.eval_objective_value = flag
end

version_added(::typeof(test_nonlinear_expression_quartic)) = v"1.17.0"

function test_nonlinear_expression_overrides_objective(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires T == Float64
    @requires _supports(config, MOI.optimize!)
    F = MOI.ScalarNonlinearFunction
    @requires MOI.supports(model, MOI.ObjectiveFunction{F}())
    x = MOI.add_variables(model, 2)
    MOI.add_constraint.(model, x, MOI.GreaterThan(0.0))
    MOI.add_constraint(model, 1.0 * x[1] + 2.0 * x[2], MOI.LessThan(1.0))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    f = MOI.ScalarNonlinearFunction(:+, Any[x[1], x[2]])
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
    @test ≈(MOI.get.(model, MOI.VariablePrimal(), x), [1.0, 0.0], config)
    @test ≈(MOI.get(model, MOI.ObjectiveValue()), 1.0, config)
    f = 2.0 * x[1] + x[2]
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
    @test ≈(MOI.get.(model, MOI.VariablePrimal(), x), [1.0, 0.0], config)
    @test ≈(MOI.get(model, MOI.ObjectiveValue()), 2.0, config)
    return
end

function setup_test(
    ::typeof(test_nonlinear_expression_overrides_objective),
    model::MOIU.MockOptimizer,
    config::Config{T},
) where {T}
    flag = model.eval_objective_value
    model.eval_objective_value = false
    MOI.Utilities.set_mock_optimize!(
        model,
        (mock) -> begin
            MOI.Utilities.mock_optimize!(mock, config.optimal_status, [1.0, 0.0])
            MOI.set(mock, MOI.ObjectiveValue(), 1.0)
        end,
        (mock) -> begin
            MOI.Utilities.mock_optimize!(mock, config.optimal_status, [1.0, 0.0])
            MOI.set(mock, MOI.ObjectiveValue(), 2.0)
        end,
    )
    return () -> model.eval_objective_value = flag
end

function version_added(::typeof(test_nonlinear_expression_overrides_objective))
    return v"1.17.0"
end

function test_nonlinear_expression_univariate_function(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires T == Float64
    @requires _supports(config, MOI.optimize!)
    F = MOI.ScalarNonlinearFunction
    @requires MOI.supports(model, MOI.ObjectiveFunction{F}())
    @requires MOI.supports(model, MOI.UserDefinedFunction(:my_square, 1))
    my_square(x) = (x - 1)^2
    MOI.set(model, MOI.UserDefinedFunction(:my_square, 1), (my_square,))
    x = MOI.add_variable(model)
    obj = MOI.ScalarNonlinearFunction(:my_square, Any[x])
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{typeof(obj)}(), obj)
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
    @test ≈(MOI.get(model, MOI.VariablePrimal(), x), T(1), config)
    return
end

function setup_test(
    ::typeof(test_nonlinear_expression_univariate_function),
    model::MOIU.MockOptimizer,
    config::Config{T},
) where {T}
    MOI.Utilities.set_mock_optimize!(
        model,
        mock -> MOI.Utilities.mock_optimize!(mock, config.optimal_status, [1]),
    )
    return
end

function version_added(::typeof(test_nonlinear_expression_univariate_function))
    return v"1.17.0"
end

function test_nonlinear_expression_multivariate_function(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires T == Float64
    @requires _supports(config, MOI.optimize!)
    F = MOI.ScalarNonlinearFunction
    @requires MOI.supports(model, MOI.ObjectiveFunction{F}())
    @requires MOI.supports(model, MOI.UserDefinedFunction(:my_square, 2))
    f(x, y) = (x - 1)^2 + (y - 2)^2
    function ∇f(g, x, y)
        g[1] = 2 * (x - 1)
        g[2] = 2 * (y - 2)
        return
    end
    g = zeros(2)
    ∇f(g, 2.0, 3.0)
    @test g == [2.0, 2.0]
    MOI.set(model, MOI.UserDefinedFunction(:my_square, 2), (f, ∇f))
    x = MOI.add_variable(model)
    y = MOI.add_variable(model)
    obj = MOI.ScalarNonlinearFunction(:my_square, Any[x, y])
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{typeof(obj)}(), obj)
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
    @test ≈(MOI.get(model, MOI.VariablePrimal(), x), T(1), config)
    @test ≈(MOI.get(model, MOI.VariablePrimal(), y), T(2), config)
    return
end

function setup_test(
    ::typeof(test_nonlinear_expression_multivariate_function),
    model::MOIU.MockOptimizer,
    config::Config{T},
) where {T}
    MOI.Utilities.set_mock_optimize!(
        model,
        mock -> MOI.Utilities.mock_optimize!(
            mock,
            config.optimal_status,
            [1.0, 2.0],
        ),
    )
    return
end

function version_added(
    ::typeof(test_nonlinear_expression_multivariate_function),
)
    return v"1.17.0"
end

"""
    test_nonlinear_duals(model::MOI.ModelLike, config::MOI.Test.Config)

Tests dual solutions with `ScalarNonlinearFunction`. We use a linear program
so that the duals are easy to compute.
"""
function test_nonlinear_duals(model::MOI.ModelLike, config::Config{T}) where {T}
    @requires T == Float64
    @requires _supports(config, MOI.optimize!)
    @requires _supports(config, MOI.ConstraintDual)
    F = MOI.ScalarNonlinearFunction
    @requires MOI.supports(model, MOI.ObjectiveFunction{F}())
    MOI.Utilities.loadfromstring!(
        model,
        """
variables: x, y, z, r3, r4, r5, r6
minobjective: ScalarNonlinearFunction(-((x + y) / 2.0 + 3.0) / 3.0 - z - r3)
x >= 0.0
y <= 5.0
z in Interval(2.0, 4.0)
r3 in Interval(0.0, 3.0)
r4 in Interval(0.0, 4.0)
r5 in Interval(0.0, 5.0)
r6 in Interval(0.0, 6.0)
cons1: ScalarNonlinearFunction(x + y) >= 2.0
cons2: ScalarNonlinearFunction(r3 + r4 + r5 + x / 2) <= 1.0
cons3: ScalarNonlinearFunction(7.0 * y + - (z + r6 / 1.9)) <= 0.0
""",
    )
    x = MOI.get(model, MOI.ListOfVariableIndices())
    MOI.optimize!(model)
    @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
    @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
    @test ≈(
        MOI.get(model, MOI.VariablePrimal(), x),
        [0.9774436, 1.0225564, 4.0, 0.5112782, 0.0, 0.0, 6.0],
        config,
    )
    cons1 = MOI.get(model, MOI.ConstraintIndex, "cons1")
    cons2 = MOI.get(model, MOI.ConstraintIndex, "cons2")
    cons3 = MOI.get(model, MOI.ConstraintIndex, "cons3")
    LB = MOI.ConstraintIndex{MOI.VariableIndex,MOI.GreaterThan{Float64}}
    UB = MOI.ConstraintIndex{MOI.VariableIndex,MOI.LessThan{Float64}}
    IV = MOI.ConstraintIndex{MOI.VariableIndex,MOI.Interval{Float64}}
    @test ≈(MOI.get(model, MOI.ConstraintDual(), cons1), 1 / 3, config)
    @test ≈(MOI.get(model, MOI.ConstraintDual(), cons2), -1, config)
    @test ≈(MOI.get(model, MOI.ConstraintDual(), cons3), -0.0714286, config)
    @test ≈(MOI.get(model, MOI.ConstraintDual(), LB(x[1].value)), 0.0, config)
    @test ≈(MOI.get(model, MOI.ConstraintDual(), UB(x[2].value)), 0.0, config)
    @test ≈(
        MOI.get(model, MOI.ConstraintDual(), [IV(xi.value) for xi in x[3:end]]),
        [-1.0714286, 0.0, 1.0, 1.0, -0.03759398],
        config,
    )
    @test ≈(MOI.get(model, MOI.ObjectiveValue()), -5.8446115, config)
    f = MOI.get(model, MOI.ObjectiveFunction{F}())
    MOI.set(
        model,
        MOI.ObjectiveFunction{F}(),
        MOI.ScalarNonlinearFunction(:-, Any[f]),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.optimize!(model)
    @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
    @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
    @test ≈(
        MOI.get(model, MOI.VariablePrimal(), x),
        [0.9774436, 1.0225564, 4.0, 0.5112782, 0.0, 0.0, 6.0],
        config,
    )
    @test ≈(MOI.get(model, MOI.ConstraintDual(), cons1), 1 / 3, config)
    @test ≈(MOI.get(model, MOI.ConstraintDual(), cons2), -1, config)
    @test ≈(MOI.get(model, MOI.ConstraintDual(), cons3), -0.0714286, config)
    @test ≈(MOI.get(model, MOI.ConstraintDual(), LB(x[1].value)), 0.0, config)
    @test ≈(MOI.get(model, MOI.ConstraintDual(), UB(x[2].value)), 0.0, config)
    @test ≈(
        MOI.get(model, MOI.ConstraintDual(), [IV(xi.value) for xi in x[3:end]]),
        [-1.0714286, 0.0, 1.0, 1.0, -0.03759398],
        config,
    )
    @test ≈(MOI.get(model, MOI.ObjectiveValue()), 5.8446115, config)
    return
end

function setup_test(
    ::typeof(test_nonlinear_duals),
    model::MOIU.MockOptimizer,
    config::Config{T},
) where {T}
    if T != Float64
        return  # Skip for non-Float64 solvers
    end
    MOI.Utilities.set_mock_optimize!(
        model,
        mock -> begin
            MOI.Utilities.mock_optimize!(
                mock,
                [0.9774436, 1.0225564, 4.0, 0.5112782, 0.0, 0.0, 6.0],
                (MOI.ScalarNonlinearFunction, MOI.GreaterThan{T}) => T[1/3],
                (MOI.ScalarNonlinearFunction, MOI.LessThan{T}) =>
                    T[-1, -0.0714286],
                (MOI.VariableIndex, MOI.GreaterThan{T}) => [0.0],
                (MOI.VariableIndex, MOI.LessThan{T}) => [0.0],
                (MOI.VariableIndex, MOI.Interval{T}) =>
                    [-1.0714286, 0.0, 1.0, 1.0, -0.03759398],
            )
            MOI.set(mock, MOI.ObjectiveValue(), -5.8446115)
        end,
        mock -> begin
            MOI.Utilities.mock_optimize!(
                mock,
                [0.9774436, 1.0225564, 4.0, 0.5112782, 0.0, 0.0, 6.0],
                (MOI.ScalarNonlinearFunction, MOI.GreaterThan{T}) => T[1/3],
                (MOI.ScalarNonlinearFunction, MOI.LessThan{T}) =>
                    T[-1, -0.0714286],
                (MOI.VariableIndex, MOI.GreaterThan{T}) => [0.0],
                (MOI.VariableIndex, MOI.LessThan{T}) => [0.0],
                (MOI.VariableIndex, MOI.Interval{T}) =>
                    [-1.0714286, 0.0, 1.0, 1.0, -0.03759398],
            )
            MOI.set(mock, MOI.ObjectiveValue(), 5.8446115)
        end,
    )
    flag = model.eval_variable_constraint_dual
    obj_flag = model.eval_objective_value
    model.eval_variable_constraint_dual = false
    model.eval_objective_value = false
    return () -> begin
        model.eval_variable_constraint_dual = flag
        model.eval_objective_value = obj_flag
    end
end

version_added(::typeof(test_nonlinear_duals)) = v"1.17.0"

function test_nonlinear_vector_complements(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires T == Float64
    @requires _supports(config, MOI.optimize!)
    F = MOI.ScalarNonlinearFunction
    @requires MOI.supports_constraint(model, F, MOI.Complements)
    x = MOI.add_variables(model, 4)
    MOI.add_constraint.(model, x, MOI.Interval(T(0), T(10)))
    MOI.set.(model, MOI.VariablePrimalStart(), x, T(1))
    # f = [
    #     -1 * x3^2 + -1 * x4 + 2.0
    #     x3^3 + -1.0 * 2x4^2 + 2.0
    #     x1^5 + -1.0 * x2 + 2.0 * x3 + -2.0 * x4 + -2.0
    #     x1 + 2.0 * x2^3 + -2.0 * x3 + 4.0 * x4 + -6.0
    #     x...
    # ]
    f = MOI.VectorNonlinearFunction([
        MOI.ScalarNonlinearFunction(
            :+,
            Any[
                MOI.ScalarNonlinearFunction(:*, Any[-T(1), x[3], x[3]]),
                MOI.ScalarNonlinearFunction(:*, Any[-T(1), x[4]]),
                T(2),
            ],
        ),
        MOI.ScalarNonlinearFunction(
            :+,
            Any[
                MOI.ScalarNonlinearFunction(:^, Any[x[3], 3]),
                MOI.ScalarNonlinearFunction(:*, Any[-T(2), x[4], x[4]]),
                T(2),
            ],
        ),
        MOI.ScalarNonlinearFunction(
            :+,
            Any[
                MOI.ScalarNonlinearFunction(:^, Any[x[1], 5]),
                MOI.ScalarAffineFunction{T}(
                    MOI.ScalarAffineTerm.(T[-1, 2, -2], x[2:4]),
                    -T(2),
                ),
            ],
        ),
        MOI.ScalarNonlinearFunction(
            :+,
            Any[
                MOI.ScalarNonlinearFunction(:*, Any[T(2), x[2], x[2], x[2]]),
                MOI.ScalarAffineFunction{T}(
                    MOI.ScalarAffineTerm.(T[1, -2, 4], [x[1], x[3], x[4]]),
                    -T(6),
                ),
            ],
        ),
        x[1],
        x[2],
        x[3],
        x[4],
    ])
    MOI.add_constraint(model, f, MOI.Complements(8))
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
    @test ≈(
        MOI.get.(model, MOI.VariablePrimal(), x),
        T[1.2847523, 0.9729165, 0.9093762, 1.1730350],
        config,
    )
    return
end

function setup_test(
    ::typeof(test_nonlinear_vector_complements),
    model::MOIU.MockOptimizer,
    config::Config{T},
) where {T}
    if T != Float64
        return  # Skip for non-Float64 solvers
    end
    MOI.Utilities.set_mock_optimize!(
        model,
        mock -> MOI.Utilities.mock_optimize!(
            mock,
            config.optimal_status,
            T[1.2847523, 0.9729165, 0.9093762, 1.1730350],
        ),
    )
    return
end

version_added(::typeof(test_nonlinear_vector_complements)) = v"1.19.0"

function test_nonlinear_with_scalar_quadratic_function_with_off_diag(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires _supports(config, MOI.optimize!)
    F = MOI.ScalarNonlinearFunction
    @requires MOI.supports_constraint(model, F, MOI.EqualTo{T})
    test_cases = [(1, 2, true), (1, 3, false), (2, 3, true), (2, 4, false)]
    for (a, b, status) in test_cases
        MOI.empty!(model)
        x, _ = MOI.add_constrained_variable(model, MOI.EqualTo(T(2)))
        MOI.set(model, MOI.VariablePrimalStart(), x, T(2))
        y, _ = MOI.add_constrained_variable(model, MOI.EqualTo(T(3)))
        MOI.set(model, MOI.VariablePrimalStart(), y, T(3))
        g = T(a) * x * y
        @test g isa MOI.ScalarQuadraticFunction{T}
        f = MOI.ScalarNonlinearFunction(:sqrt, Any[g])
        MOI.add_constraint(model, f, MOI.GreaterThan(T(b)))
        MOI.optimize!(model)
        term_status = MOI.get(model, MOI.TerminationStatus())
        @test (term_status == config.optimal_status) == status
    end
    return
end

function setup_test(
    ::typeof(test_nonlinear_with_scalar_quadratic_function_with_off_diag),
    model::MOIU.MockOptimizer,
    config::Config{T},
) where {T}
    MOI.Utilities.set_mock_optimize!(
        model,
        mock ->
            MOI.Utilities.mock_optimize!(mock, config.optimal_status, T[2, 3]),
        mock -> MOI.Utilities.mock_optimize!(mock, config.infeasible_status),
        mock ->
            MOI.Utilities.mock_optimize!(mock, config.optimal_status, T[2, 3]),
        mock -> MOI.Utilities.mock_optimize!(mock, config.infeasible_status),
    )
    return
end

function version_added(
    ::typeof(test_nonlinear_with_scalar_quadratic_function_with_off_diag),
)
    return v"1.35.0"
end

function test_nonlinear_constraint_log(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires _supports(config, MOI.optimize!)
    F, S = MOI.ScalarNonlinearFunction, MOI.GreaterThan{T}
    @requires MOI.supports_constraint(model, F, S)
    x = MOI.add_variable(model)
    t = MOI.add_variable(model)
    MOI.add_constraint(model, x, MOI.LessThan(T(2)))
    MOI.add_constraint(model, x, MOI.GreaterThan(T(1)))
    MOI.set(model, MOI.VariablePrimalStart(), x, T(1))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    f = 1.0 * t
    # max t
    #  x <= 2
    #  log(x) >= t
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    g = MOI.ScalarNonlinearFunction(
        :-,
        Any[MOI.ScalarNonlinearFunction(:log, Any[x]), t],
    )
    c = MOI.add_constraint(model, g, MOI.GreaterThan(T(0)))
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
    x_val = MOI.get(model, MOI.VariablePrimal(), x)
    t_val = MOI.get(model, MOI.VariablePrimal(), t)
    @test ≈(x_val, T(2), config)
    @test ≈(t_val, log(x_val), config)
    @test ≈(MOI.get(model, MOI.ObjectiveValue()), t_val, config)
    @test (F, S) in MOI.get(model, MOI.ListOfConstraintTypesPresent())
    @test c in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
    return
end

function setup_test(
    ::typeof(test_nonlinear_constraint_log),
    model::MOIU.MockOptimizer,
    config::Config{T},
) where {T}
    MOI.Utilities.set_mock_optimize!(
        model,
        mock -> MOI.Utilities.mock_optimize!(
            mock,
            config.optimal_status,
            T[2, log(T(2))],
        ),
    )
    return
end

version_added(::typeof(test_nonlinear_constraint_log)) = v"1.35.0"

function test_nonlinear_constraint_uminus(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires _supports(config, MOI.optimize!)
    F, S = MOI.ScalarNonlinearFunction, MOI.GreaterThan{T}
    @requires MOI.supports_constraint(model, F, S)
    x = MOI.add_variable(model)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    f = T(1) * x
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    g = MOI.ScalarNonlinearFunction(:-, Any[x])
    MOI.add_constraint(model, g, MOI.GreaterThan(T(-2)))
    MOI.optimize!(model)
    @test ≈(MOI.get(model, MOI.VariablePrimal(), x), T(2), config)
    @test ≈(MOI.get(model, MOI.ObjectiveValue()), T(2), config)
    return
end

function setup_test(
    ::typeof(test_nonlinear_constraint_uminus),
    model::MOIU.MockOptimizer,
    config::Config{T},
) where {T}
    MOI.Utilities.set_mock_optimize!(
        model,
        mock -> MOI.Utilities.mock_optimize!(mock, config.optimal_status, T[2]),
    )
    return
end

version_added(::typeof(test_nonlinear_constraint_uminus)) = v"1.35.0"

function test_nonlinear_constraint_scalar_affine_function(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires _supports(config, MOI.optimize!)
    F, S = MOI.ScalarNonlinearFunction, MOI.LessThan{T}
    @requires MOI.supports_constraint(model, F, S)
    x1, _ = MOI.add_constrained_variable(model, MOI.GreaterThan(zero(T)))
    x2, _ = MOI.add_constrained_variable(model, MOI.GreaterThan(zero(T)))
    x3, _ = MOI.add_constrained_variable(model, MOI.GreaterThan(zero(T)))
    x4, _ = MOI.add_constrained_variable(model, MOI.GreaterThan(zero(T)))
    f = T(1) * x1 + T(2) * x2 + T(3) * x3
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    h = T(1) * x1 + T(2) * x2 + T(3) * x3 + T(4) * x4
    g = MOI.ScalarNonlinearFunction(:+, Any[h])
    MOI.add_constraint(model, g, MOI.LessThan(T(6)))
    MOI.optimize!(model)
    @test ≈(MOI.get(model, MOI.ObjectiveValue()), T(6), config)
    return
end

function setup_test(
    ::typeof(test_nonlinear_constraint_scalar_affine_function),
    model::MOIU.MockOptimizer,
    config::Config{T},
) where {T}
    MOI.Utilities.set_mock_optimize!(
        model,
        mock -> MOI.Utilities.mock_optimize!(
            mock,
            config.optimal_status,
            T[1, 1, 1, 0],
        ),
    )
    return
end

function version_added(
    ::typeof(test_nonlinear_constraint_scalar_affine_function),
)
    return v"1.35.0"
end

function test_nonlinear_quadratic_1(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires _supports(config, MOI.optimize!)
    F, S = MOI.ScalarNonlinearFunction, MOI.LessThan{T}
    @requires MOI.supports_constraint(model, F, S)
    # max x + y
    # s.t sqrt(*(x, x) + *(y, y)) <= 1
    # x, y >= 0
    #
    # -> x = y = 1/sqrt(2)
    x = MOI.add_variable(model)
    MOI.set(model, MOI.VariablePrimalStart(), x, T(1))
    y = MOI.add_variable(model)
    MOI.set(model, MOI.VariablePrimalStart(), y, T(1))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    f = T(1) * x + T(1) * y
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    f1 = MOI.ScalarNonlinearFunction(:*, Any[x, x])
    f2 = MOI.ScalarNonlinearFunction(:*, Any[y, y])
    f3 = MOI.ScalarNonlinearFunction(:+, Any[f1, f2])
    g = MOI.ScalarNonlinearFunction(:sqrt, Any[f3])
    c = MOI.add_constraint(model, g, MOI.LessThan(T(1)))
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
    @test ≈(MOI.get(model, MOI.ObjectiveValue()), 2 / sqrt(2), config)
    @test ≈(MOI.get(model, MOI.VariablePrimal(), x), 1 / sqrt(2), config)
    @test ≈(MOI.get(model, MOI.VariablePrimal(), y), 1 / sqrt(2), config)
    return
end

function setup_test(
    ::typeof(test_nonlinear_quadratic_1),
    model::MOIU.MockOptimizer,
    config::Config{T},
) where {T}
    MOI.Utilities.set_mock_optimize!(
        model,
        mock -> MOI.Utilities.mock_optimize!(
            mock,
            config.optimal_status,
            T[1/sqrt(2), 1/sqrt(2)],
        ),
    )
    return
end

version_added(::typeof(test_nonlinear_quadratic_1)) = v"1.35.0"

function test_nonlinear_quadratic_2(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires _supports(config, MOI.optimize!)
    F, S = MOI.ScalarNonlinearFunction, MOI.LessThan{T}
    @requires MOI.supports_constraint(model, F, S)
    # Present products as ScalarAffineTerms nested in ScalarNonlinearFunctions
    # max x + y
    # s.t sqrt(*(x, x) + *(y, y)) <= 1
    # x, y >= 0
    #
    # -> x = y = 1/sqrt(2)
    x = MOI.add_variable(model)
    MOI.set(model, MOI.VariablePrimalStart(), x, T(1))
    y = MOI.add_variable(model)
    MOI.set(model, MOI.VariablePrimalStart(), y, T(1))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    f = T(1) * x + T(1) * y
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    f1 = MOI.ScalarNonlinearFunction(:*, Any[T(1)*x, x])
    f2 = MOI.ScalarNonlinearFunction(:*, Any[T(1)*y, y])
    f3 = MOI.ScalarNonlinearFunction(:+, Any[f1, f2])
    g = MOI.ScalarNonlinearFunction(:sqrt, Any[f3])
    c = MOI.add_constraint(model, g, MOI.LessThan(T(1)))
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
    @test ≈(MOI.get(model, MOI.ObjectiveValue()), 2 / sqrt(2), config)
    @test ≈(MOI.get(model, MOI.VariablePrimal(), x), 1 / sqrt(2), config)
    @test ≈(MOI.get(model, MOI.VariablePrimal(), y), 1 / sqrt(2), config)
    return
end

function setup_test(
    ::typeof(test_nonlinear_quadratic_2),
    model::MOIU.MockOptimizer,
    config::Config{T},
) where {T}
    MOI.Utilities.set_mock_optimize!(
        model,
        mock -> MOI.Utilities.mock_optimize!(
            mock,
            config.optimal_status,
            T[1/sqrt(2), 1/sqrt(2)],
        ),
    )
    return
end

version_added(::typeof(test_nonlinear_quadratic_2)) = v"1.35.0"

function test_nonlinear_quadratic_3(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires _supports(config, MOI.optimize!)
    F, S = MOI.ScalarNonlinearFunction, MOI.LessThan{T}
    @requires MOI.supports_constraint(model, F, S)
    # Present products as ScalarQuadraticFunctions (complete with 2x factor ...)
    # max x + y
    # s.t sqrt(*(x, x) + *(y, y)) <= 1
    # x, y >= 0
    #
    # -> x = y = 1/sqrt(2)
    x = MOI.add_variable(model)
    MOI.set(model, MOI.VariablePrimalStart(), x, T(1))
    y = MOI.add_variable(model)
    MOI.set(model, MOI.VariablePrimalStart(), y, T(1))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    f = T(1) * x + T(1) * y
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    f1 = T(1) * x * x
    f2 = T(1) * y * y
    f3 = MOI.ScalarNonlinearFunction(:+, Any[f1, f2])
    g = MOI.ScalarNonlinearFunction(:sqrt, Any[f3])
    c = MOI.add_constraint(model, g, MOI.LessThan(T(1)))
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
    @test ≈(MOI.get(model, MOI.ObjectiveValue()), 2 / sqrt(2), config)
    @test ≈(MOI.get(model, MOI.VariablePrimal(), x), 1 / sqrt(2), config)
    @test ≈(MOI.get(model, MOI.VariablePrimal(), y), 1 / sqrt(2), config)
    return
end

function setup_test(
    ::typeof(test_nonlinear_quadratic_3),
    model::MOIU.MockOptimizer,
    config::Config{T},
) where {T}
    MOI.Utilities.set_mock_optimize!(
        model,
        mock -> MOI.Utilities.mock_optimize!(
            mock,
            config.optimal_status,
            T[1/sqrt(2), 1/sqrt(2)],
        ),
    )
    return
end

version_added(::typeof(test_nonlinear_quadratic_3)) = v"1.35.0"

function test_nonlinear_quadratic_4(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires _supports(config, MOI.optimize!)
    F, S = MOI.ScalarNonlinearFunction, MOI.LessThan{T}
    @requires MOI.supports_constraint(model, F, S)
    # max x + y
    # s.t sqrt(^(x, 2) + ^(y, 2)) <= 1  # Use NL POW(2) operator
    # x, y >= 0
    #
    # -> x = y = 1/sqrt(2)
    x = MOI.add_variable(model)
    MOI.set(model, MOI.VariablePrimalStart(), x, T(1))
    y = MOI.add_variable(model)
    MOI.set(model, MOI.VariablePrimalStart(), y, T(1))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    f = T(1) * x + T(1) * y
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    f1 = MOI.ScalarNonlinearFunction(:^, Any[x, 2])
    f2 = MOI.ScalarNonlinearFunction(:^, Any[y, 2])
    f3 = MOI.ScalarNonlinearFunction(:+, Any[f1, f2])
    g = MOI.ScalarNonlinearFunction(:sqrt, Any[f3])
    c = MOI.add_constraint(model, g, MOI.LessThan(T(1)))
    MOI.optimize!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
    @test ≈(MOI.get(model, MOI.ObjectiveValue()), 2 / sqrt(2), config)
    @test ≈(MOI.get(model, MOI.VariablePrimal(), x), 1 / sqrt(2), config)
    @test ≈(MOI.get(model, MOI.VariablePrimal(), y), 1 / sqrt(2), config)
    return
end

function setup_test(
    ::typeof(test_nonlinear_quadratic_4),
    model::MOIU.MockOptimizer,
    config::Config{T},
) where {T}
    MOI.Utilities.set_mock_optimize!(
        model,
        mock -> MOI.Utilities.mock_optimize!(
            mock,
            config.optimal_status,
            T[1/sqrt(2), 1/sqrt(2)],
        ),
    )
    return
end

version_added(::typeof(test_nonlinear_quadratic_4)) = v"1.35.0"

function test_vector_nonlinear_oracle(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires _supports(config, MOI.optimize!)
    @requires MOI.supports_constraint(
        model,
        MOI.VectorOfVariables,
        MOI.VectorNonlinearOracle{T},
    )
    set = MOI.VectorNonlinearOracle(;
        dimension = 5,
        l = T[0, 0],
        u = T[0, 0],
        eval_f = (ret, x) -> begin
            @test length(ret) == 2
            @test length(x) == 5
            ret[1] = x[1]^2 - x[4]
            ret[2] = x[2]^2 + x[3]^3 - x[5]
            return
        end,
        jacobian_structure = [(1, 1), (2, 2), (2, 3), (1, 4), (2, 5)],
        eval_jacobian = (ret, x) -> begin
            @test length(ret) == 5
            @test length(x) == 5
            ret[1] = T(2) * x[1]
            ret[2] = T(2) * x[2]
            ret[3] = T(3) * x[3]^2
            ret[4] = -T(1)
            ret[5] = -T(1)
            return
        end,
        hessian_lagrangian_structure = [(1, 1), (2, 2), (3, 3)],
        eval_hessian_lagrangian = (ret, x, u) -> begin
            @test length(ret) == 3
            @test length(x) == 5
            @test length(u) == 2
            ret[1] = T(2) * u[1]
            ret[2] = T(2) * u[2]
            ret[3] = T(6) * x[3] * u[2]
            return
        end,
    )
    @test MOI.dimension(set) == 5
    x = T[1, 2, 3, 4, 5]
    ret = T[0, 0]
    set.eval_f(ret, x)
    @test ret == T[-3, 26]
    ret = T[0, 0, 0, 0, 0]
    set.eval_jacobian(ret, x)
    @test ret == T[2, 4, 27, -1, -1]
    ret = T[0, 0, 0]
    set.eval_hessian_lagrangian(ret, x, T[2, 3])
    @test ret == T[4, 6, 54]
    x, y = MOI.add_variables(model, 3), MOI.add_variables(model, 2)
    MOI.add_constraints.(model, x, MOI.EqualTo.(T(1):T(3)))
    c = MOI.add_constraint(model, MOI.VectorOfVariables([x; y]), set)
    MOI.optimize!(model)
    x_v = MOI.get.(model, MOI.VariablePrimal(), x)
    y_v = MOI.get.(model, MOI.VariablePrimal(), y)
    @test ≈(y_v, [x_v[1]^2, x_v[2]^2 + x_v[3]^3], config)
    @test ≈(MOI.get(model, MOI.ConstraintPrimal(), c), [x_v; y_v], config)
    @test ≈(MOI.get(model, MOI.ConstraintDual(), c), zeros(T, 5), config)
    return
end

function setup_test(
    ::typeof(test_vector_nonlinear_oracle),
    model::MOIU.MockOptimizer,
    config::Config{T},
) where {T}
    MOI.Utilities.set_mock_optimize!(
        model,
        mock -> MOI.Utilities.mock_optimize!(
            mock,
            config.optimal_status,
            T[1, 2, 3, 1, 31],
            (MOI.VectorOfVariables, MOI.VectorNonlinearOracle{T}) =>
                [zeros(T, 5)],
        ),
    )
    model.eval_variable_constraint_dual = false
    return () -> model.eval_variable_constraint_dual = true
end

version_added(::typeof(test_vector_nonlinear_oracle)) = v"1.46.0"

function test_vector_nonlinear_oracle_no_hessian(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires _supports(config, MOI.optimize!)
    @requires MOI.supports_constraint(
        model,
        MOI.VectorOfVariables,
        MOI.VectorNonlinearOracle{T},
    )
    set = MOI.VectorNonlinearOracle(;
        dimension = 5,
        l = T[0, 0],
        u = T[0, 0],
        eval_f = (ret, x) -> begin
            ret[1] = x[1]^2 - x[4]
            ret[2] = x[2]^2 + x[3]^3 - x[5]
            return
        end,
        jacobian_structure = [(1, 1), (2, 2), (2, 3), (1, 4), (2, 5)],
        eval_jacobian = (ret, x) -> begin
            ret[1] = T(2) * x[1]
            ret[2] = T(2) * x[2]
            ret[3] = T(3) * x[3]^2
            ret[4] = -T(1)
            ret[5] = -T(1)
            return
        end,
    )
    @test MOI.dimension(set) == 5
    x = T[1, 2, 3, 4, 5]
    ret = T[0, 0]
    set.eval_f(ret, x)
    @test ret == T[-3, 26]
    ret = T[0, 0, 0, 0, 0]
    set.eval_jacobian(ret, x)
    @test ret == T[2, 4, 27, -1, -1]
    @test isempty(set.hessian_lagrangian_structure)
    @test set.eval_hessian_lagrangian === nothing
    x, y = MOI.add_variables(model, 3), MOI.add_variables(model, 2)
    MOI.add_constraints.(model, x, MOI.EqualTo.(T(1):T(3)))
    c = MOI.add_constraint(model, MOI.VectorOfVariables([x; y]), set)
    MOI.optimize!(model)
    x_v = MOI.get.(model, MOI.VariablePrimal(), x)
    y_v = MOI.get.(model, MOI.VariablePrimal(), y)
    @test ≈(y_v, [x_v[1]^2, x_v[2]^2 + x_v[3]^3], config)
    @test ≈(MOI.get(model, MOI.ConstraintPrimal(), c), [x_v; y_v], config)
    @test ≈(MOI.get(model, MOI.ConstraintDual(), c), zeros(T, 5), config)
    return
end

function setup_test(
    ::typeof(test_vector_nonlinear_oracle_no_hessian),
    model::MOIU.MockOptimizer,
    config::Config{T},
) where {T}
    MOI.Utilities.set_mock_optimize!(
        model,
        mock -> MOI.Utilities.mock_optimize!(
            mock,
            config.optimal_status,
            T[1, 2, 3, 1, 31],
            (MOI.VectorOfVariables, MOI.VectorNonlinearOracle{T}) =>
                [zeros(T, 5)],
        ),
    )
    model.eval_variable_constraint_dual = false
    return () -> model.eval_variable_constraint_dual = true
end

version_added(::typeof(test_vector_nonlinear_oracle_no_hessian)) = v"1.46.0"
