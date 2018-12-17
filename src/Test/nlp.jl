

struct HS071 <: MOI.AbstractNLPEvaluator
    enable_hessian::Bool
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
            # TODO: implement Jac-vec and Hess-vec products
            # for solvers that need them
        end
    end
end

function MOI.features_available(d::HS071)
    if d.enable_hessian
        return [:Grad, :Jac, :Hess]
    else
        return [:Grad, :Jac]
    end
end

MOI.eval_objective(d::HS071, x) = x[1] * x[4] * (x[1] + x[2] + x[3]) + x[3]

function MOI.eval_constraint(d::HS071, g, x)
    g[1] = x[1]   * x[2]   * x[3]   * x[4]
    g[2] = x[1]^2 + x[2]^2 + x[3]^2 + x[4]^2
end

function MOI.eval_objective_gradient(d::HS071, grad_f, x)
    grad_f[1] = x[1] * x[4] + x[4] * (x[1] + x[2] + x[3])
    grad_f[2] = x[1] * x[4]
    grad_f[3] = x[1] * x[4] + 1
    grad_f[4] = x[1] * (x[1] + x[2] + x[3])
end

function MOI.jacobian_structure(d::HS071)
    return Tuple{Int64,Int64}[(1,1), (1,2), (1,3), (1,4), (2,1), (2,2),
                              (2,3), (2,4)]
end
# lower triangle only
function MOI.hessian_lagrangian_structure(d::HS071)
    @assert d.enable_hessian
    return Tuple{Int64,Int64}[(1,1), (2,1), (2,2), (3,1), (3,2), (3,3),
                              (4,1), (4,2), (4,3), (4,4)]
end

function MOI.eval_constraint_jacobian(d::HS071, J, x)
    # Constraint (row) 1
    J[1] = x[2]*x[3]*x[4]  # 1,1
    J[2] = x[1]*x[3]*x[4]  # 1,2
    J[3] = x[1]*x[2]*x[4]  # 1,3
    J[4] = x[1]*x[2]*x[3]  # 1,4
    # Constraint (row) 2
    J[5] = 2*x[1]  # 2,1
    J[6] = 2*x[2]  # 2,2
    J[7] = 2*x[3]  # 2,3
    J[8] = 2*x[4]  # 2,4
end

function MOI.eval_hessian_lagrangian(d::HS071, H, x, σ, μ)
    @assert d.enable_hessian
    # Again, only lower left triangle
    # Objective
    H[1] = σ * (2*x[4])               # 1,1
    H[2] = σ * (  x[4])               # 2,1
    H[3] = 0                          # 2,2
    H[4] = σ * (  x[4])               # 3,1
    H[5] = 0                          # 3,2
    H[6] = 0                          # 3,3
    H[7] = σ* (2*x[1] + x[2] + x[3])  # 4,1
    H[8] = σ * (  x[1])               # 4,2
    H[9] = σ * (  x[1])               # 4,3
    H[10] = 0                         # 4,4

    # First constraint
    H[2] += μ[1] * (x[3] * x[4])  # 2,1
    H[4] += μ[1] * (x[2] * x[4])  # 3,1
    H[5] += μ[1] * (x[1] * x[4])  # 3,2
    H[7] += μ[1] * (x[2] * x[3])  # 4,1
    H[8] += μ[1] * (x[1] * x[3])  # 4,2
    H[9] += μ[1] * (x[1] * x[2])  # 4,3

    # Second constraint
    H[1]  += μ[2] * 2  # 1,1
    H[3]  += μ[2] * 2  # 2,2
    H[6]  += μ[2] * 2  # 3,3
    H[10] += μ[2] * 2  # 4,4

end

function hs071test_template(model::MOI.ModelLike, config::TestConfig, evaluator::HS071)
    atol = config.atol
    rtol = config.rtol

    @test MOI.supports(model, MOI.NLPBlock())
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.LessThan{Float64})
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.GreaterThan{Float64})
    @test MOI.supports(model, MOI.VariablePrimalStart(), MOI.VariableIndex)

    MOI.empty!(model)
    @test MOI.is_empty(model)

    lb = [25.0, 40.0]
    ub = [Inf, 40.0]

    block_data = MOI.NLPBlockData(MOI.NLPBoundsPair.(lb, ub), evaluator, true)

    v = MOI.add_variables(model, 4)
    @test MOI.get(model, MOI.NumberOfVariables()) == 4

    l = [1.0,1.0,1.0,1.0]
    u = [5.0,5.0,5.0,5.0]
    start = [1,5,5,1]

    for i in 1:4
        MOI.add_constraint(model, MOI.SingleVariable(v[i]), MOI.LessThan(u[i]))
        MOI.add_constraint(model, MOI.SingleVariable(v[i]), MOI.GreaterThan(l[i]))
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

        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 17.014017145179164 atol=atol rtol=rtol

        optimal_v = [1.0, 4.7429996418092970, 3.8211499817883077, 1.3794082897556983]

        @test MOI.get(model, MOI.VariablePrimal(), v) ≈ optimal_v atol=atol rtol=rtol

        # TODO: Duals? Maybe better to test on a convex instance.
    end
end

hs071_test(model, config) = hs071test_template(model, config, HS071(true))
hs071_no_hessian_test(model, config) = hs071test_template(model, config, HS071(false))

# Test for FEASIBILITY_SENSE.
# Find x satisfying x^2 == 1.
struct FeasibilitySenseEvaluator <: MOI.AbstractNLPEvaluator
    enable_hessian::Bool
end

function MOI.initialize(d::FeasibilitySenseEvaluator,
                         requested_features::Vector{Symbol})
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
        return [:Grad, :Jac, :Hess]
    else
        return [:Grad, :Jac]
    end
end


MOI.eval_objective(d::FeasibilitySenseEvaluator, x) = 0.0

function MOI.eval_constraint(d::FeasibilitySenseEvaluator, g, x)
    g[1] = x[1]^2
end

function MOI.eval_objective_gradient(d::FeasibilitySenseEvaluator, grad_f, x)
    grad_f[1] = 0.0
end

function MOI.jacobian_structure(d::FeasibilitySenseEvaluator)
    return Tuple{Int64,Int64}[(1,1)]
end

function MOI.hessian_lagrangian_structure(d::FeasibilitySenseEvaluator)
    @assert d.enable_hessian
    return Tuple{Int64,Int64}[(1,1)]
end

function MOI.eval_constraint_jacobian(d::FeasibilitySenseEvaluator, J, x)
    J[1] = 2x[1]
end

function MOI.eval_hessian_lagrangian(d::FeasibilitySenseEvaluator, H, x, σ, μ)
    @assert d.enable_hessian
    H[1] = 2μ[1] # 1,1
end

function feasibility_sense_test_template(model::MOI.ModelLike,
                                         config::TestConfig,
                                         set_has_objective::Bool,
                                         evaluator::FeasibilitySenseEvaluator)
    atol = config.atol
    rtol = config.rtol

    @test MOI.supports(model, MOI.NLPBlock())
    @test MOI.supports(model, MOI.VariablePrimalStart(), MOI.VariableIndex)

    MOI.empty!(model)
    @test MOI.is_empty(model)

    lb = [1.0]
    ub = [1.0]

    block_data = MOI.NLPBlockData(MOI.NLPBoundsPair.(lb, ub), evaluator,
                                  set_has_objective)

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

        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 0.0 atol=atol rtol=rtol

        @test MOI.get(model, MOI.VariablePrimal(), x) ≈ 1.0 atol=atol rtol=rtol
    end
end

function feasibility_sense_with_objective_and_hessian_test(model, config)
    feasibility_sense_test_template(model, config, true,
                                    FeasibilitySenseEvaluator(true))
end

function feasibility_sense_with_objective_and_no_hessian_test(model, config)
    feasibility_sense_test_template(model, config, true,
                                    FeasibilitySenseEvaluator(false))
end

function feasibility_sense_with_no_objective_and_with_hessian_test(model, config)
    feasibility_sense_test_template(model, config, false,
                                    FeasibilitySenseEvaluator(true))
end

function feasibility_sense_with_no_objective_and_no_hessian_test(model, config)
    feasibility_sense_test_template(model, config, false,
                                    FeasibilitySenseEvaluator(false))
end

const nlptests = Dict("hs071" => hs071_test,
                      "hs071_no_hessian" => hs071_no_hessian_test,
                      "feasibility_sense_with_objective_and_hessian" =>
                      feasibility_sense_with_objective_and_hessian_test,
                      "feasibility_sense_with_objective_and_no_hessian" =>
                      feasibility_sense_with_objective_and_no_hessian_test,
                      "feasibility_sense_with_no_objective_and_with_hessian" =>
                      feasibility_sense_with_no_objective_and_with_hessian_test,
                      "feasibility_sense_with_no_objective_and_no_hessian" =>
                      feasibility_sense_with_no_objective_and_no_hessian_test)

@moitestset nlp
