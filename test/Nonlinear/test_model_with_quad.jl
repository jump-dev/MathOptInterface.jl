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
function _test_model()
    model = Nonlinear.ModelWithQuad(Nonlinear.Model())
    x = MOI.add_variable(model)
    y = MOI.add_variable(model)
    @test (x, y) == (MOI.VariableIndex(1), MOI.VariableIndex(2))
    @test MOI.is_valid(model, x) && !MOI.is_valid(model, MOI.VariableIndex(3))
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
    return model, x, y
end

function test_evaluator_with_quad()
    model, x, y = _test_model()
    d = Nonlinear.Evaluator(model, Nonlinear.SparseReverseMode())
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
    model, x, y = _test_model()
    d = Nonlinear.Evaluator(model, Nonlinear.SparseReverseMode())
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
    model = Nonlinear.ModelWithQuad(Nonlinear.Model())
    x = MOI.add_variable(model)
    @test model.objective_sink == Nonlinear._NONE
    f = MOI.ScalarQuadraticFunction(
        [MOI.ScalarQuadraticTerm(2.0, x, x)],
        MOI.ScalarAffineTerm{Float64}[],
        0.0,
    )
    Nonlinear.set_objective(model, f)
    @test model.objective_sink == Nonlinear._QUAD
    @test MOI.get(model, MOI.ObjectiveFunctionType()) ==
          MOI.ScalarQuadraticFunction{Float64}
    @test MOI.get(model, MOI.ObjectiveFunction{typeof(f)}()) ≈ f
    d = Nonlinear.Evaluator(model, Nonlinear.SparseReverseMode())
    MOI.initialize(d, [:Grad, :Jac, :Hess])
    @test MOI.eval_objective(d, [3.0]) == 9.0
    @test MOI.NLPBlockData(d).has_objective
    # Switch to a nonlinear objective: the quadratic objective must be
    # cleared, including its Hessian entries.
    Nonlinear.set_objective(model, :(sin($x)))
    @test model.objective_sink == Nonlinear._INNER
    d = Nonlinear.Evaluator(model, Nonlinear.SparseReverseMode())
    MOI.initialize(d, [:Grad, :Jac, :Hess])
    @test MOI.eval_objective(d, [3.0]) == sin(3.0)
    grad = fill(NaN, 1)
    MOI.eval_objective_gradient(d, grad, [3.0])
    @test grad ≈ [cos(3.0)]
    @test MOI.NLPBlockData(d).has_objective
    H_structure = MOI.hessian_lagrangian_structure(d)
    H = fill(NaN, length(H_structure))
    MOI.eval_hessian_lagrangian(d, H, [3.0], 1.0, Float64[])
    @test sum(H) ≈ -sin(3.0)
    # Switch to a linear objective, and then remove it.
    g = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(2.0, x)], 1.0)
    Nonlinear.set_objective(model, g)
    @test model.objective_sink == Nonlinear._QUAD
    d = Nonlinear.Evaluator(model, Nonlinear.SparseReverseMode())
    MOI.initialize(d, [:Grad, :Jac])
    @test MOI.eval_objective(d, [3.0]) == 7.0
    grad = fill(NaN, 1)
    MOI.eval_objective_gradient(d, grad, [3.0])
    @test grad == [2.0]
    Nonlinear.set_objective(model, nothing)
    @test model.objective_sink == Nonlinear._NONE
    d = Nonlinear.Evaluator(model, Nonlinear.SparseReverseMode())
    MOI.initialize(d, [:Grad, :Jac])
    @test MOI.eval_objective(d, [3.0]) == 0.0
    grad = fill(NaN, 1)
    MOI.eval_objective_gradient(d, grad, [3.0])
    @test grad == [0.0]
    @test !MOI.NLPBlockData(d).has_objective
    return
end

function test_quad_parameters()
    model = Nonlinear.ModelWithQuad(Nonlinear.Model())
    x = MOI.add_variable(model)
    p, cp = MOI.add_constrained_variable(model, MOI.Parameter(5.0))
    @test p.value == Nonlinear._PARAMETER_OFFSET + 1
    @test MOI.is_valid(model, p) && MOI.is_valid(model, cp)
    @test MOI.get(model, MOI.ConstraintFunction(), cp) == p
    @test MOI.get(model, MOI.ConstraintSet(), cp) == MOI.Parameter(5.0)
    F, S = MOI.VariableIndex, MOI.Parameter{Float64}
    @test MOI.get(model, MOI.NumberOfConstraints{F,S}()) == 1
    @test MOI.get(model, MOI.ListOfConstraintIndices{F,S}()) == [cp]
    # The value is stored in the inner model, aliased by the QP block.
    @test model.qp.parameters === model.inner.parameters
    # `ListOfVariableIndices` is in the order of creation, parameters
    # included.
    @test MOI.get(model, MOI.NumberOfVariables()) == 2
    @test MOI.get(model, MOI.ListOfVariableIndices()) == [x, p]
    let model = Nonlinear.ModelWithQuad(Nonlinear.Model())
        q, _ = MOI.add_constrained_variable(model, MOI.Parameter(1.0))
        z = MOI.add_variable(model)
        @test MOI.get(model, MOI.ListOfVariableIndices()) == [q, z]
    end
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
    d = Nonlinear.Evaluator(model, Nonlinear.SparseReverseMode())
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
    MOI.set(model, MOI.ConstraintSet(), cp, MOI.Parameter(7.0))
    MOI.eval_constraint(d, g, [1.0])
    @test g == [2.0 * 1.0 + 3.0 * 7.0, 7.0 * 1.0]
    # The Hessian and the products skip the terms with a parameter: the
    # second constraint, `p * x`, has no entry.
    @test isempty(MOI.hessian_lagrangian_structure(d))
    H = Float64[]
    MOI.eval_hessian_lagrangian(d, H, [1.0], 1.0, [1.0, 1.0])
    Jv = fill(NaN, 2)
    MOI.eval_constraint_jacobian_product(d, Jv, [1.0], [1.5])
    @test Jv == [2.0 * 1.5, 7.0 * 1.5]
    Jtv = fill(NaN, 1)
    MOI.eval_constraint_jacobian_transpose_product(d, Jtv, [1.0], [1.0, 1.0])
    @test Jtv == [2.0 + 7.0]
    Hv = fill(NaN, 1)
    MOI.eval_hessian_lagrangian_product(d, Hv, [1.0], [1.5], 1.0, [1.0, 1.0])
    @test Hv == [0.0]
    # A nonlinear constraint with the parameter in an embedded affine
    # subfunction: the layer substitutes the parameter before the inner model
    # parses the function.
    aff = MOI.ScalarAffineFunction(
        [MOI.ScalarAffineTerm(3.0, p), MOI.ScalarAffineTerm(1.0, x)],
        0.0,
    )
    snf = MOI.ScalarNonlinearFunction(:sqrt, Any[aff])
    Nonlinear.add_constraint(model, snf, MOI.LessThan(10.0))
    # A nonlinear constraint with a parameter-free affine subfunction, which
    # the substitution leaves as is.
    Nonlinear.add_constraint(
        model,
        MOI.ScalarNonlinearFunction(
            :sqrt,
            Any[MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0)],
        ),
        MOI.LessThan(10.0),
    )
    # A nonlinear constraint and objective mentioning the parameter and the
    # variable directly.
    Nonlinear.add_constraint(
        model,
        MOI.ScalarNonlinearFunction(:+, Any[x, p]),
        MOI.LessThan(20.0),
    )
    Nonlinear.set_objective(model, MOI.ScalarNonlinearFunction(:*, Any[p, x]))
    d = Nonlinear.Evaluator(model, Nonlinear.SparseReverseMode())
    MOI.initialize(d, [:Grad, :Jac])
    g = fill(NaN, 5)
    MOI.eval_constraint(d, g, [1.0])
    @test g ≈ [2.0 + 3.0 * 7.0, 7.0, sqrt(3.0 * 7.0 + 1.0), 1.0, 1.0 + 7.0]
    @test MOI.eval_objective(d, [1.5]) == 7.0 * 1.5
    return
end

function test_attribute_forwarding()
    model = Nonlinear.ModelWithQuad(Nonlinear.Model())
    x = MOI.add_variable(model)
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
    d = Nonlinear.Evaluator(model, Nonlinear.SparseReverseMode())
    MOI.initialize(d, [:Grad, :Jac])
    g = fill(NaN, 2)
    MOI.eval_constraint(d, g, [3.0])
    @test g == [3.0, 36.0]
    return
end

function test_qp_attribute_types()
    model = Nonlinear.ModelWithQuad(Nonlinear.Model())
    x = MOI.add_variable(model)
    y = MOI.add_variable(model)
    Nonlinear.set_objective(model, x)
    @test MOI.get(model, MOI.ObjectiveFunctionType()) == MOI.VariableIndex
    @test MOI.get(model, MOI.ObjectiveFunction{MOI.VariableIndex}()) == x
    F = MOI.ScalarAffineFunction{Float64}
    f = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0)
    q = MOI.ScalarQuadraticFunction(
        [MOI.ScalarQuadraticTerm(2.0, x, y)],
        MOI.ScalarAffineTerm{Float64}[],
        0.0,
    )
    c1 = MOI.add_constraint(model, f, MOI.LessThan(1.0))
    c2 = MOI.add_constraint(model, f, MOI.EqualTo(2.0))
    c3 = MOI.add_constraint(model, f, MOI.Interval(3.0, 4.0))
    c4 = MOI.add_constraint(model, q, MOI.GreaterThan(5.0))
    @test MOI.get(model, MOI.ConstraintSet(), c1) == MOI.LessThan(1.0)
    @test MOI.get(model, MOI.ConstraintSet(), c2) == MOI.EqualTo(2.0)
    @test MOI.get(model, MOI.ConstraintSet(), c3) == MOI.Interval(3.0, 4.0)
    @test MOI.get(model, MOI.ConstraintSet(), c4) == MOI.GreaterThan(5.0)
    for (S, ci) in [
        (MOI.LessThan{Float64}, c1),
        (MOI.EqualTo{Float64}, c2),
        (MOI.Interval{Float64}, c3),
    ]
        @test MOI.get(model, MOI.ListOfConstraintIndices{F,S}()) == [ci]
        @test MOI.get(model, MOI.NumberOfConstraints{F,S}()) == 1
    end
    Q = MOI.ScalarQuadraticFunction{Float64}
    S = MOI.GreaterThan{Float64}
    c5 = MOI.add_constraint(model, f, MOI.GreaterThan(6.0))
    @test MOI.get(model, MOI.ListOfConstraintIndices{Q,S}()) == [c4]
    @test MOI.get(model, MOI.ListOfConstraintIndices{F,S}()) == [c5]
    # The gradient of a quadratic objective with affine and off-diagonal
    # terms.
    g = MOI.ScalarQuadraticFunction(
        [
            MOI.ScalarQuadraticTerm(2.0, x, x),
            MOI.ScalarQuadraticTerm(1.0, x, y),
        ],
        [MOI.ScalarAffineTerm(3.0, x)],
        0.0,
    )
    Nonlinear.set_objective(model, g)
    d = Nonlinear.Evaluator(model, Nonlinear.SparseReverseMode())
    MOI.initialize(d, [:Grad, :Jac])
    xv = [1.0, 2.0]
    @test MOI.eval_objective(d, xv) == 1.0 + 2.0 + 3.0
    grad = fill(NaN, 2)
    MOI.eval_objective_gradient(d, grad, xv)
    @test grad == [2.0 * 1.0 + 2.0 + 3.0, 1.0]
    return
end

function test_quad_only_with_empty_inner()
    model = Nonlinear.ModelWithQuad(Nonlinear.Model())
    x = MOI.add_variable(model)
    Nonlinear.add_constraint(
        model,
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0),
        MOI.GreaterThan(1.0),
    )
    d = Nonlinear.Evaluator(model, Nonlinear.SparseReverseMode())
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
