const VI = MOI.VariableIndex

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

# hs071
# min x1 * x4 * (x1 + x2 + x3) + x3
# st  x1 * x2 * x3 * x4 >= 25
#     x1^2 + x2^2 + x3^2 + x4^2 = 40
#     1 <= x1, x2, x3, x4 <= 5
# Start at (1,5,5,1)
# End at (1.000..., 4.743..., 3.821..., 1.379...)

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

function MOI.objective_expr(d::HS071)
    return :(
        x[$(VI(1))] * x[$(VI(4))] * (x[$(VI(1))] + x[$(VI(2))] + x[$(VI(3))]) +
        x[$(VI(3))]
    )
end

function MOI.constraint_expr(d::HS071, i::Int)
    if i == 1
        return :(x[$(VI(1))] * x[$(VI(2))] * x[$(VI(3))] * x[$(VI(4))] >= 25.0)
    elseif i == 2
        return :(
            x[$(VI(1))]^2 + x[$(VI(2))]^2 + x[$(VI(3))]^2 + x[$(VI(4))]^2 ==
            40.0
        )
    else
        error("Out of bounds constraint.")
    end
end

MOI.eval_objective(d::HS071, x) = x[1] * x[4] * (x[1] + x[2] + x[3]) + x[3]

function MOI.eval_constraint(d::HS071, g, x)
    g[1] = x[1] * x[2] * x[3] * x[4]
    g[2] = x[1]^2 + x[2]^2 + x[3]^2 + x[4]^2
    return
end

function MOI.eval_objective_gradient(d::HS071, grad_f, x)
    grad_f[1] = x[1] * x[4] + x[4] * (x[1] + x[2] + x[3])
    grad_f[2] = x[1] * x[4]
    grad_f[3] = x[1] * x[4] + 1
    grad_f[4] = x[1] * (x[1] + x[2] + x[3])
    return
end

function MOI.jacobian_structure(d::HS071)
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
# lower triangle only
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

function MOI.eval_constraint_jacobian(d::HS071, J, x)
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

function hs071test_template(
    model::MOI.ModelLike,
    config::Config,
    evaluator::HS071,
)
    atol = config.atol
    rtol = config.rtol
    @test MOI.supports(model, MOI.NLPBlock())
    @test MOI.supports_constraint(
        model,
        MOI.VariableIndex,
        MOI.LessThan{Float64},
    )
    @test MOI.supports_constraint(
        model,
        MOI.VariableIndex,
        MOI.GreaterThan{Float64},
    )
    @test MOI.supports(model, MOI.VariablePrimalStart(), MOI.VariableIndex)
    MOI.empty!(model)
    @test MOI.is_empty(model)
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
    # TODO: config.query tests
    if config.solve
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.ResultCount()) >= 1
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 17.014017145179164 atol =
            atol rtol = rtol
        optimal_v =
            [1.0, 4.7429996418092970, 3.8211499817883077, 1.3794082897556983]
        @test MOI.get(model, MOI.VariablePrimal(), v) ≈ optimal_v atol = atol rtol =
            rtol
        # TODO: Duals? Maybe better to test on a convex instance.
    end
end

function hs071_test(model, config)
    return hs071test_template(model, config, HS071(true))
end

function hs071_no_hessian_test(model, config)
    return hs071test_template(model, config, HS071(false))
end

function hs071_hessian_vector_product_test(model, config)
    return hs071test_template(model, config, HS071(false, true))
end

# Test for FEASIBILITY_SENSE.
# Find x satisfying x^2 == 1.
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
end

function MOI.features_available(d::FeasibilitySenseEvaluator)
    if d.enable_hessian
        return [:Grad, :Jac, :Hess, :ExprGraph]
    else
        return [:Grad, :Jac, :ExprGraph]
    end
end

MOI.objective_expr(d::FeasibilitySenseEvaluator) = :()

function MOI.constraint_expr(d::FeasibilitySenseEvaluator, i::Int)
    if i == 1
        return :(x[$(VI(1))]^2 == 1)
    else
        error("Out of bounds constraint.")
    end
end

MOI.eval_objective(d::FeasibilitySenseEvaluator, x) = 0.0

function MOI.eval_constraint(d::FeasibilitySenseEvaluator, g, x)
    return g[1] = x[1]^2
end

function MOI.eval_objective_gradient(d::FeasibilitySenseEvaluator, grad_f, x)
    return grad_f[1] = 0.0
end

function MOI.jacobian_structure(d::FeasibilitySenseEvaluator)
    return Tuple{Int64,Int64}[(1, 1)]
end

function MOI.hessian_lagrangian_structure(d::FeasibilitySenseEvaluator)
    @assert d.enable_hessian
    return Tuple{Int64,Int64}[(1, 1)]
end

function MOI.eval_constraint_jacobian(d::FeasibilitySenseEvaluator, J, x)
    return J[1] = 2x[1]
end

function MOI.eval_hessian_lagrangian(d::FeasibilitySenseEvaluator, H, x, σ, μ)
    @assert d.enable_hessian
    return H[1] = 2μ[1] # 1,1
end

function feasibility_sense_test_template(
    model::MOI.ModelLike,
    config::Config,
    set_has_objective::Bool,
    evaluator::FeasibilitySenseEvaluator,
)
    atol = config.atol
    rtol = config.rtol
    @test MOI.supports(model, MOI.NLPBlock())
    @test MOI.supports(model, MOI.VariablePrimalStart(), MOI.VariableIndex)
    MOI.empty!(model)
    @test MOI.is_empty(model)
    lb = [1.0]
    ub = [1.0]
    block_data = MOI.NLPBlockData(
        MOI.NLPBoundsPair.(lb, ub),
        evaluator,
        set_has_objective,
    )
    x = MOI.add_variable(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 1
    # Avoid starting at zero because it's a critial point.
    MOI.set(model, MOI.VariablePrimalStart(), x, 1.5)
    MOI.set(model, MOI.NLPBlock(), block_data)
    MOI.set(model, MOI.ObjectiveSense(), MOI.FEASIBILITY_SENSE)
    # TODO: config.query tests
    if config.solve
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.ResultCount()) >= 1
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 0.0 atol = atol rtol = rtol
        @test abs(MOI.get(model, MOI.VariablePrimal(), x)) ≈ 1.0 atol = atol rtol =
            rtol
    end
end

function feasibility_sense_with_objective_and_hessian_test(model, config)
    return feasibility_sense_test_template(
        model,
        config,
        true,
        FeasibilitySenseEvaluator(true),
    )
end

function feasibility_sense_with_objective_and_no_hessian_test(model, config)
    return feasibility_sense_test_template(
        model,
        config,
        true,
        FeasibilitySenseEvaluator(false),
    )
end

function feasibility_sense_with_no_objective_and_with_hessian_test(
    model,
    config,
)
    return feasibility_sense_test_template(
        model,
        config,
        false,
        FeasibilitySenseEvaluator(true),
    )
end

function feasibility_sense_with_no_objective_and_no_hessian_test(model, config)
    return feasibility_sense_test_template(
        model,
        config,
        false,
        FeasibilitySenseEvaluator(false),
    )
end

function nlp_objective_and_moi_objective_test(
    model::MOI.ModelLike,
    config::Config,
)
    atol = config.atol
    rtol = config.rtol
    @test MOI.supports(model, MOI.NLPBlock())
    @test MOI.supports(model, MOI.VariablePrimalStart(), MOI.VariableIndex)
    MOI.empty!(model)
    @test MOI.is_empty(model)
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
    # TODO: config.query tests
    if config.solve
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.ResultCount()) >= 1
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 0.0 atol = atol rtol = rtol
        @test 1.0 ≤ MOI.get(model, MOI.VariablePrimal(), x) ≤ 2.0
    end
end

const nlptests = Dict(
    "hs071" => hs071_test,
    "hs071_no_hessian" => hs071_no_hessian_test,
    "hs071_hessian_vector_product_test" =>
        hs071_hessian_vector_product_test,
    "feasibility_sense_with_objective_and_hessian" =>
        feasibility_sense_with_objective_and_hessian_test,
    "feasibility_sense_with_objective_and_no_hessian" =>
        feasibility_sense_with_objective_and_no_hessian_test,
    "feasibility_sense_with_no_objective_and_with_hessian" =>
        feasibility_sense_with_no_objective_and_with_hessian_test,
    "feasibility_sense_with_no_objective_and_no_hessian" =>
        feasibility_sense_with_no_objective_and_no_hessian_test,
    "nlp_objective_and_moi_objective" =>
        nlp_objective_and_moi_objective_test,
)

@moitestset nlp

"""
    test_linear_mixed_complementarity(model::MOI.ModelLike, config::Config)

Test the solution of the linear mixed-complementarity problem:
`F(x) complements x`, where `F(x) = M * x .+ q` and `0 <= x <= 10`.
"""
function test_linear_mixed_complementarity(model::MOI.ModelLike, config::Config)
    MOI.empty!(model)
    x = MOI.add_variables(model, 4)
    MOI.add_constraint.(model, x, MOI.Interval(0.0, 10.0))
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
    if config.solve
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        x_val = MOI.get.(model, MOI.VariablePrimal(), x)
        @test isapprox(
            x_val,
            [2.8, 0.0, 0.8, 1.2],
            atol = config.atol,
            rtol = config.rtol,
        )
    end
end

const mixed_complementaritytests = Dict(
    "test_linear_mixed_complementarity" =>
        test_linear_mixed_complementarity,
)

@moitestset mixed_complementarity

"""
    test_qp_complementarity_constraint(model::MOI.ModelLike, config::Config)

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
function test_qp_complementarity_constraint(
    model::MOI.ModelLike,
    config::Config,
)
    MOI.empty!(model)
    x = MOI.add_variables(model, 8)
    MOI.set.(model, MOI.VariablePrimalStart(), x, 0.0)
    MOI.add_constraint.(model, x, MOI.GreaterThan(0.0))
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
    if config.solve
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        x_val = MOI.get.(model, MOI.VariablePrimal(), x)
        @test isapprox(
            x_val,
            [1.0, 0.0, 3.5, 0.0, 0.0, 0.0, 3.0, 6.0],
            atol = config.atol,
            rtol = config.rtol,
        )
        @test isapprox(
            MOI.get(model, MOI.ObjectiveValue()),
            17.0,
            atol = config.atol,
            rtol = config.rtol,
        )
    end
end

const math_program_complementarity_constraintstests = Dict(
    "test_qp_complementarity_constraint" =>
        test_qp_complementarity_constraint,
)

@moitestset math_program_complementarity_constraints
