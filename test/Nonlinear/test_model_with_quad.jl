# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestNonlinearModelWithQuad

using Test
import MathOptInterface as MOI

import MathOptInterface.Nonlinear

function runtests()
    for name in names(@__MODULE__; all = true)
        if startswith("$(name)", "test_")
            @testset "$(name)" begin
                getfield(@__MODULE__, name)()
            end
        end
    end
    return
end

# A model with, in row order:
#   row 1 (quad layer, linear):    2x + 3y <= 4
#   row 2 (quad layer, quadratic): x^2 + xy + y in [0, 1]
#   row 3 (inner nlp):             sin(x) <= 0.5
# and the objective x^2 in the quad layer.
function _test_model(x, y)
    model = Nonlinear.ModelWithQuad(Nonlinear.Model())
    Nonlinear.set_objective(
        model,
        MOI.ScalarQuadraticFunction(
            [MOI.ScalarQuadraticTerm(2.0, x, x)],
            MOI.ScalarAffineTerm{Float64}[],
            0.0,
        ),
    )
    c1 = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(
            [MOI.ScalarAffineTerm(2.0, x), MOI.ScalarAffineTerm(3.0, y)],
            0.0,
        ),
        MOI.LessThan(4.0),
    )
    @test c1 isa MOI.ConstraintIndex{
        MOI.ScalarAffineFunction{Float64},
        MOI.LessThan{Float64},
    }
    c2 = Nonlinear.add_constraint(
        model,
        MOI.ScalarQuadraticFunction(
            [
                MOI.ScalarQuadraticTerm(2.0, x, x),
                MOI.ScalarQuadraticTerm(1.0, x, y),
            ],
            [MOI.ScalarAffineTerm(1.0, y)],
            0.0,
        ),
        MOI.Interval(0.0, 1.0),
    )
    c3 = Nonlinear.add_constraint(model, :(sin($x)), MOI.LessThan(0.5))
    @test c3 isa Nonlinear.ConstraintIndex
    @test length(model) == 2
    return model
end

function test_evaluator_with_quad()
    # Use non-consecutive variable indices to test that the evaluator remaps
    # the variables to their consecutive index in `ordered_variables`.
    x, y = MOI.VariableIndex(5), MOI.VariableIndex(9)
    model = _test_model(x, y)
    d = Nonlinear.Evaluator(model, Nonlinear.SparseReverseMode(), [x, y])
    @test d isa Nonlinear.EvaluatorWithQuad
    @test d.inner isa Nonlinear.Evaluator
    @test MOI.features_available(d) == [:Grad, :Jac, :JacVec, :Hess, :HessVec]
    MOI.initialize(d, [:Grad, :Jac, :Hess])
    xv = [1.0, 2.0]  # x = 1, y = 2
    @test MOI.eval_objective(d, xv) == 1.0
    grad = fill(NaN, 2)
    MOI.eval_objective_gradient(d, grad, xv)
    @test grad == [2.0, 0.0]
    g = fill(NaN, 3)
    MOI.eval_constraint(d, g, xv)
    @test g ≈ [8.0, 5.0, sin(1.0)]
    # Jacobian: accumulate the sparse entries into a dense matrix.
    J_structure = MOI.jacobian_structure(d)
    J_values = fill(NaN, length(J_structure))
    MOI.eval_constraint_jacobian(d, J_values, xv)
    J = zeros(3, 2)
    for ((row, col), value) in zip(J_structure, J_values)
        J[row, col] += value
    end
    @test J ≈ [
        2.0 3.0
        4.0 2.0
        cos(1.0) 0.0
    ]
    # Hessian of the Lagrangian: accumulate into a dense matrix.
    H_structure = MOI.hessian_lagrangian_structure(d)
    σ, μ = 2.0, [10.0, 100.0, 1_000.0]
    H_values = fill(NaN, length(H_structure))
    MOI.eval_hessian_lagrangian(d, H_values, xv, σ, μ)
    H = zeros(2, 2)
    for ((row, col), value) in zip(H_structure, H_values)
        H[row, col] += value
    end
    # σ * ∇²(x^2) + μ₂ * ∇²(x^2 + xy) + μ₃ * ∇²(sin(x))
    @test H[1, 1] ≈ 2σ + 2 * μ[2] - sin(1.0) * μ[3]
    @test H[1, 2] + H[2, 1] ≈ μ[2]
    @test H[2, 2] ≈ 0.0
    block = MOI.NLPBlockData(d)
    @test block.has_objective
    @test block.constraint_bounds == [
        MOI.NLPBoundsPair(-Inf, 4.0),
        MOI.NLPBoundsPair(0.0, 1.0),
        MOI.NLPBoundsPair(-Inf, 0.5),
    ]
    return
end

function test_evaluator_products()
    x, y = MOI.VariableIndex(1), MOI.VariableIndex(2)
    model = _test_model(x, y)
    d = Nonlinear.Evaluator(model, Nonlinear.SparseReverseMode(), [x, y])
    MOI.initialize(d, [:Grad, :Jac, :JacVec, :Hess, :HessVec])
    xv = [1.0, 2.0]
    # Dense Jacobian from the sparse callback, as the reference.
    J_structure = MOI.jacobian_structure(d)
    J_values = fill(NaN, length(J_structure))
    MOI.eval_constraint_jacobian(d, J_values, xv)
    J = zeros(3, 2)
    for ((row, col), value) in zip(J_structure, J_values)
        J[row, col] += value
    end
    w = [1.0, -2.0]
    Jv = fill(NaN, 3)
    MOI.eval_constraint_jacobian_product(d, Jv, xv, w)
    @test Jv ≈ J * w
    u = [1.0, -1.0, 2.0]
    Jtv = fill(NaN, 2)
    MOI.eval_constraint_jacobian_transpose_product(d, Jtv, xv, u)
    @test Jtv ≈ J' * u
    # Dense Hessian of the Lagrangian, as the reference.
    H_structure = MOI.hessian_lagrangian_structure(d)
    σ, μ = 2.0, [10.0, 100.0, 1_000.0]
    H_values = fill(NaN, length(H_structure))
    MOI.eval_hessian_lagrangian(d, H_values, xv, σ, μ)
    H = zeros(2, 2)
    for ((row, col), value) in zip(H_structure, H_values)
        H[row, col] += value
        if row != col
            H[col, row] += value
        end
    end
    v = [1.0, -3.0]
    Hv = fill(NaN, 2)
    MOI.eval_hessian_lagrangian_product(d, Hv, xv, v, σ, μ)
    @test Hv ≈ H * v
    return
end

function test_objective_sink_switching()
    x = MOI.VariableIndex(1)
    model = Nonlinear.ModelWithQuad(Nonlinear.Model())
    @test model.objective_sink == :none
    f = MOI.ScalarQuadraticFunction(
        [MOI.ScalarQuadraticTerm(2.0, x, x)],
        MOI.ScalarAffineTerm{Float64}[],
        0.0,
    )
    Nonlinear.set_objective(model, f)
    @test model.objective_sink == :quad
    @test MOI.get(model, MOI.ObjectiveFunctionType()) ==
          MOI.ScalarQuadraticFunction{Float64}
    @test MOI.get(model, MOI.ObjectiveFunction{typeof(f)}()) ≈ f
    d = Nonlinear.Evaluator(model, Nonlinear.SparseReverseMode(), [x])
    MOI.initialize(d, [:Grad, :Jac, :Hess])
    @test MOI.eval_objective(d, [3.0]) == 9.0
    @test MOI.NLPBlockData(d).has_objective
    # Switch to a nonlinear objective: the quadratic objective must be
    # cleared, including its Hessian entries.
    Nonlinear.set_objective(model, :(sin($x)))
    @test model.objective_sink == :inner
    d = Nonlinear.Evaluator(model, Nonlinear.SparseReverseMode(), [x])
    MOI.initialize(d, [:Grad, :Jac, :Hess])
    @test MOI.eval_objective(d, [3.0]) == sin(3.0)
    @test MOI.NLPBlockData(d).has_objective
    H_structure = MOI.hessian_lagrangian_structure(d)
    H = fill(NaN, length(H_structure))
    MOI.eval_hessian_lagrangian(d, H, [3.0], 1.0, Float64[])
    @test sum(H) ≈ -sin(3.0)
    # Switch to a linear objective, and then remove it.
    g = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(2.0, x)], 1.0)
    Nonlinear.set_objective(model, g)
    @test model.objective_sink == :quad
    d = Nonlinear.Evaluator(model, Nonlinear.SparseReverseMode(), [x])
    MOI.initialize(d, [:Grad, :Jac])
    @test MOI.eval_objective(d, [3.0]) == 7.0
    Nonlinear.set_objective(model, nothing)
    @test model.objective_sink == :none
    d = Nonlinear.Evaluator(model, Nonlinear.SparseReverseMode(), [x])
    MOI.initialize(d, [:Grad, :Jac])
    @test MOI.eval_objective(d, [3.0]) == 0.0
    grad = fill(NaN, 1)
    MOI.eval_objective_gradient(d, grad, [3.0])
    @test grad == [0.0]
    @test !MOI.NLPBlockData(d).has_objective
    return
end

function test_quad_parameters()
    x, p = MOI.VariableIndex(1), MOI.VariableIndex(42)
    model = Nonlinear.ModelWithQuad(Nonlinear.Model())
    # Register the parameter before any structure query. Its value may be
    # updated between evaluations.
    model.qp.parameters[p.value] = 5.0
    Nonlinear.add_constraint(
        model,
        MOI.ScalarAffineFunction(
            [MOI.ScalarAffineTerm(2.0, x), MOI.ScalarAffineTerm(3.0, p)],
            0.0,
        ),
        MOI.LessThan(10.0),
    )
    Nonlinear.add_constraint(
        model,
        MOI.ScalarQuadraticFunction(
            [MOI.ScalarQuadraticTerm(1.0, p, x)],
            MOI.ScalarAffineTerm{Float64}[],
            0.0,
        ),
        MOI.LessThan(10.0),
    )
    d = Nonlinear.Evaluator(model, Nonlinear.SparseReverseMode(), [x])
    MOI.initialize(d, [:Grad, :Jac, :Hess])
    g = fill(NaN, 2)
    MOI.eval_constraint(d, g, [1.0])
    @test g == [2.0 * 1.0 + 3.0 * 5.0, 5.0 * 1.0]
    # Parameters never appear in the Jacobian or Hessian structure.
    @test MOI.jacobian_structure(d) == [(1, 1), (2, 1)]
    J = fill(NaN, 2)
    MOI.eval_constraint_jacobian(d, J, [1.0])
    @test J == [2.0, 5.0]
    @test isempty(MOI.hessian_lagrangian_structure(d))
    # Updating the parameter value must be visible without re-initializing.
    model.qp.parameters[p.value] = 7.0
    MOI.eval_constraint(d, g, [1.0])
    @test g == [2.0 * 1.0 + 3.0 * 7.0, 7.0 * 1.0]
    return
end

function test_attribute_forwarding()
    x = MOI.VariableIndex(1)
    model = Nonlinear.ModelWithQuad(Nonlinear.Model())
    F, S = MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}
    f = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0)
    ci = MOI.add_constraint(model, f, MOI.GreaterThan(1.0))
    @test MOI.is_valid(model, ci)
    @test !MOI.is_valid(model, typeof(ci)(ci.value + 1))
    @test MOI.get(model, MOI.NumberOfConstraints{F,S}()) == 1
    @test MOI.get(model, MOI.ListOfConstraintIndices{F,S}()) == [ci]
    @test (F, S) in MOI.get(model, MOI.ListOfConstraintTypesPresent())
    @test MOI.get(model, MOI.ConstraintFunction(), ci) ≈ f
    @test MOI.get(model, MOI.ConstraintSet(), ci) == MOI.GreaterThan(1.0)
    MOI.set(model, MOI.ConstraintSet(), ci, MOI.GreaterThan(2.0))
    @test MOI.get(model, MOI.ConstraintSet(), ci) == MOI.GreaterThan(2.0)
    @test MOI.get(model, MOI.ConstraintDualStart(), ci) === nothing
    MOI.set(model, MOI.ConstraintDualStart(), ci, 1.5)
    @test MOI.get(model, MOI.ConstraintDualStart(), ci) == 1.5
    # Nonlinear-model forwarding
    p = Nonlinear.add_parameter(model, 2.0)
    @test p isa Nonlinear.ParameterIndex
    ex = Nonlinear.add_expression(model, :($p * $x))
    @test model[ex] isa Nonlinear.Expression
    Nonlinear.register_operator(model, :my_square, 1, z -> z^2)
    c = Nonlinear.add_constraint(model, :(my_square($ex)), MOI.LessThan(1.0))
    @test MOI.is_valid(model, c)
    d = Nonlinear.Evaluator(model, Nonlinear.SparseReverseMode(), [x])
    MOI.initialize(d, [:Grad, :Jac])
    g = fill(NaN, 2)
    MOI.eval_constraint(d, g, [3.0])
    @test g == [3.0, 36.0]
    return
end

function test_quad_only_with_empty_inner()
    x = MOI.VariableIndex(1)
    model = Nonlinear.ModelWithQuad(Nonlinear.Model())
    Nonlinear.add_constraint(
        model,
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0),
        MOI.GreaterThan(1.0),
    )
    d = Nonlinear.Evaluator(model, Nonlinear.SparseReverseMode(), [x])
    MOI.initialize(d, [:Grad, :Jac, :Hess])
    g = fill(NaN, 1)
    MOI.eval_constraint(d, g, [1.5])
    @test g == [1.5]
    @test isempty(MOI.hessian_lagrangian_structure(d))
    @test MOI.NLPBlockData(d).constraint_bounds == [MOI.NLPBoundsPair(1.0, Inf)]
    return
end

end  # module

TestNonlinearModelWithQuad.runtests()
