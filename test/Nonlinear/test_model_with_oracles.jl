# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestNonlinearModelWithOracles

using Test
import MathOptInterface as MOI

function _oracle()
    return MOI.VectorNonlinearOracle(;
        dimension = 1,
        l = [0.0],
        u = [4.0],
        eval_f = (y, x) -> (y[1] = x[1]^2),
        jacobian_structure = [(1, 1)],
        eval_jacobian = (J, x) -> (J[1] = 2x[1]),
        hessian_lagrangian_structure = [(1, 1)],
        eval_hessian_lagrangian = (H, x, μ) -> (H[1] = 2μ[1]),
    )
end

function test_moi_model_stack()
    inner = MOI.Nonlinear.Model()
    oracles = MOI.Nonlinear.ModelWithOracles(inner)
    model = MOI.Nonlinear.ModelWithQuad(oracles)
    @test model isa MOI.ModelLike
    @test oracles isa MOI.ModelLike
    @test inner isa MOI.ModelLike
    x = MOI.add_variable(model)
    q = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0),
        MOI.LessThan(2.0),
    )
    set = _oracle()
    c = MOI.add_constraint(model, MOI.VectorOfVariables([x]), set)
    @test MOI.supports_constraint(model, MOI.VectorOfVariables, typeof(set))
    @test MOI.get(model, MOI.ConstraintSet(), c) === set
    @test MOI.Utilities.rows(model, q) == 1
    @test MOI.Utilities.rows(model, c) == 2:2
    bounds = MOI.Utilities.constraint_bounds(model)
    @test bounds == MOI.Utilities.Hyperrectangle([-Inf, 0.0], [2.0, 4.0])
    variable_bounds = MOI.Utilities.variable_bounds(model)
    @test variable_bounds === model.variables
    @test variable_bounds.lower == [-Inf]
    @test variable_bounds.upper == [Inf]
    MOI.set(model, MOI.LagrangeMultiplierStart(), c, [0.5])
    @test MOI.get(model, MOI.LagrangeMultiplierStart(), c) == [0.5]
    f = MOI.ScalarQuadraticFunction(
        [MOI.ScalarQuadraticTerm(2.0, x, x)],
        MOI.ScalarAffineTerm{Float64}[],
        0.0,
    )
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    evaluator =
        MOI.Nonlinear.Evaluator(model, MOI.Nonlinear.SparseReverseMode(), [x])
    MOI.initialize(evaluator, [:Grad, :Jac, :Hess])
    @test MOI.eval_objective(evaluator, [2.0]) == -4.0
    g = zeros(2)
    MOI.eval_constraint(evaluator, g, [2.0])
    @test g == [2.0, 4.0]
end

function test_default_backend_model()
    model = MOI.Nonlinear.model(MOI.Nonlinear.SparseReverseMode())
    @test model isa MOI.Nonlinear.ModelWithQuad
    @test model.inner isa MOI.Nonlinear.ModelWithOracles
    @test model.inner.inner isa MOI.Nonlinear.Model
    return
end

function test_derivatives_and_offsets()
    model = MOI.Nonlinear.ModelWithOracles(MOI.Nonlinear.Model())
    x, y = MOI.VariableIndex.(1:2)
    f = MOI.ScalarNonlinearFunction(:*, Any[x, y])
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    c = MOI.add_constraint(model, f, MOI.EqualTo(6.0))
    a = MOI.add_constraint(model, MOI.VectorOfVariables([y]), _oracle())
    b = MOI.add_constraint(model, MOI.VectorOfVariables([x]), _oracle())
    @test MOI.Utilities.rows(model, a) == 1:1
    @test MOI.Utilities.rows(model, b) == 2:2
    @test MOI.Utilities.rows(model, c) == 3
    @test MOI.is_valid(model, a)
    @test !MOI.is_valid(model, typeof(a)(0))
    @test !MOI.is_valid(model, typeof(a)(3))
    @test MOI.is_valid(model, c)
    @test MOI.get(model, MOI.ConstraintFunction(), a) ==
          MOI.VectorOfVariables([y])
    @test MOI.get(
        model,
        MOI.NumberOfConstraints{MOI.VectorOfVariables,typeof(_oracle())}(),
    ) == 2
    @test MOI.get(
        model,
        MOI.ListOfConstraintIndices{MOI.VectorOfVariables,typeof(_oracle())}(),
    ) == [a, b]
    @test length(MOI.get(model, MOI.ListOfConstraintTypesPresent())) == 2
    @test MOI.supports(model, MOI.LagrangeMultiplierStart(), typeof(a))
    @test MOI.supports(model, MOI.ConstraintDualStart(), typeof(c))
    MOI.set(model, MOI.LagrangeMultiplierStart(), b, [4.0])
    MOI.set(model, MOI.ConstraintDualStart(), c, 5.0)
    @test MOI.get(model, MOI.ConstraintDualStart(), c) == 5.0
    @test MOI.Nonlinear.constraint_dual_starts(model) == [nothing, 4.0, 5.0]
    @test_throws DimensionMismatch MOI.add_constraint(
        model,
        MOI.VectorOfVariables([x, y]),
        _oracle(),
    )
    d = MOI.Nonlinear.Evaluator(
        model,
        MOI.Nonlinear.SparseReverseMode(),
        [y, x],
    )
    @test MOI.features_available(d) == [:Grad, :Jac, :Hess]
    MOI.initialize(d, [:Grad, :Jac, :Hess])
    # Reinitialization must discard old column maps and scratch buffers.
    MOI.initialize(d, [:Grad, :Jac, :Hess])
    point = [3.0, 2.0]
    @test MOI.eval_objective(d, point) == 6.0
    grad = zeros(2)
    MOI.eval_objective_gradient(d, grad, point)
    @test grad == [2.0, 3.0]
    g = zeros(3)
    MOI.eval_constraint(d, g, point)
    @test g == [9.0, 4.0, 6.0]
    structure = MOI.jacobian_structure(d)
    values = zeros(length(structure))
    MOI.eval_constraint_jacobian(d, values, point)
    J = zeros(3, 2)
    for ((i, j), v) in zip(structure, values)
        J[i, j] += v
    end
    @test J == [6.0 0.0; 0.0 4.0; 2.0 3.0]
    structure = MOI.hessian_lagrangian_structure(d)
    values = zeros(length(structure))
    MOI.eval_hessian_lagrangian(d, values, point, 7.0, [11.0, 13.0, 17.0])
    H = zeros(2, 2)
    for ((i, j), v) in zip(structure, values)
        H[i, j] += v
        if i != j
            H[j, i] += v
        end
    end
    @test H == [22.0 24.0; 24.0 26.0]
    @test !MOI.is_empty(model)
    MOI.empty!(model)
    @test MOI.is_empty(model)
    @test isempty(MOI.get(model, MOI.ListOfConstraintTypesPresent()))
    return
end

function test_feature_filtering()
    model = MOI.Nonlinear.ModelWithOracles(MOI.Nonlinear.Model())
    x = MOI.VariableIndex(1)
    d = MOI.Nonlinear.Evaluator(model, MOI.Nonlinear.SparseReverseMode(), [x])
    @test MOI.features_available(d) == [:Grad, :Jac, :JacVec, :Hess, :HessVec]
    s = MOI.VectorNonlinearOracle(;
        dimension = 1,
        l = [0.0],
        u = [1.0],
        eval_f = (g, x) -> (g[1] = x[1]),
        jacobian_structure = [(1, 1)],
        eval_jacobian = (J, x) -> (J[1] = 1.0),
    )
    MOI.add_constraint(model, MOI.VectorOfVariables([x]), s)
    @test MOI.features_available(d) == [:Grad, :Jac]
    return
end

function test_forwarding()
    inner = MOI.Utilities.Model{Float64}()
    model = MOI.Nonlinear.ModelWithOracles(inner)
    @test MOI.supports_incremental_interface(model)
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_add_constrained_variable(model, MOI.GreaterThan{Float64})
    x = MOI.add_variable(model)
    y, c = MOI.add_constrained_variable(model, MOI.GreaterThan(0.0))
    @test MOI.is_valid(model, x)
    @test MOI.is_valid(model, y)
    MOI.set(model, MOI.VariableName(), x, "x")
    @test MOI.get(model, MOI.VariableName(), x) == "x"
    @test MOI.supports_constraint(
        model,
        MOI.VariableIndex,
        MOI.GreaterThan{Float64},
    )
    @test MOI.get(model, MOI.ConstraintSet(), c) == MOI.GreaterThan(0.0)
    @test isempty(
        MOI.get(
            model,
            MOI.ListOfConstraintAttributesSet{
                MOI.VariableIndex,
                MOI.GreaterThan{Float64},
            }(),
        ),
    )
    return
end

function test_legacy_forwarding()
    model = MOI.Nonlinear.ModelWithOracles(MOI.Nonlinear.Model())
    x = MOI.VariableIndex(1)
    p = MOI.Nonlinear.add_parameter(model, 2.0)
    ex = MOI.Nonlinear.add_expression(model, :($p * $x))
    @test model[ex] isa MOI.Nonlinear.Expression
    MOI.Nonlinear.register_operator(model, :square, 1, z -> z^2)
    @test :square in MOI.get(model, MOI.ListOfSupportedNonlinearOperators())
    MOI.Nonlinear.add_constraint(model, :(square($ex)), MOI.LessThan(20.0))
    MOI.Nonlinear.set_objective(model, :(square($x)))
    quad = MOI.Nonlinear.ModelWithQuad(model)
    MOI.add_variable(quad)
    MOI.Nonlinear.set_objective(quad, :(square($x)))
    d = MOI.Nonlinear.Evaluator(quad, MOI.Nonlinear.SparseReverseMode(), [x])
    @test MOI.NLPBlockData(d).has_objective
    return
end

for name in names(@__MODULE__; all = true)
    if startswith(string(name), "test_")
        @testset "$name" begin
            getfield(@__MODULE__, name)()
        end
    end
end

end  # module
