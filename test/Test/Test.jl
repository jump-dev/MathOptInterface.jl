# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

using Test

import MathOptInterface as MOI

# Some tests are excluded because UniversalFallback accepts absolutely
# everything.

MOI.Test.runtests(
    MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    ),
    MOI.Test.Config(),
    exclude = [
        r"^test_model_ScalarFunctionConstantNotZero$",
        "test_model_copy_to_UnsupportedAttribute",
        "test_model_copy_to_UnsupportedConstraint",
        "test_model_supports_constraint_ScalarAffineFunction_EqualTo",
        "test_model_supports_constraint_VariableIndex_EqualTo",
        "test_model_supports_constraint_VectorOfVariables_Nonnegatives",
    ],
    warn_unsupported = true,
)

# Run the previously excluded tests, this time without UniversalFallback.

MOI.Test.runtests(
    MOI.Utilities.MockOptimizer(
        MOI.Utilities.Model{Float64}(),
        scalar_function_constant_non_zero = true,
    ),
    MOI.Test.Config();
    include = [
        r"^test_model_ScalarFunctionConstantNotZero$",
        "test_model_copy_to_UnsupportedAttribute",
        "test_model_copy_to_UnsupportedConstraint",
        "test_model_supports_constraint_ScalarAffineFunction_EqualTo",
        "test_model_supports_constraint_VariableIndex_EqualTo",
        "test_model_supports_constraint_VectorOfVariables_Nonnegatives",
    ],
    verbose = true,
)

# Test for Issue #1757

MOI.Test.test_model_ScalarFunctionConstantNotZero(
    MOI.Utilities.MockOptimizer(
        MOI.Utilities.Model{Float64}(),
        scalar_function_constant_non_zero = false,
    ),
    MOI.Test.Config(exclude = Any[MOI.ConstraintFunction]),
)

# Test exclude_tests_after. This should work despite no methods being added for IncompleteOptimizer
# because every test should get skipped.

struct IncompleteOptimizer <: MOI.AbstractOptimizer end

MOI.Test.runtests(
    IncompleteOptimizer(),
    MOI.Test.Config();
    exclude_tests_after = v"0.0.1",
)

# Non-Float64 tests

MOI.Test.runtests(
    MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{BigFloat}()),
        BigFloat,
    ),
    MOI.Test.Config(BigFloat),
    exclude = [
        # ========================= Expected failures ==========================
        # UniversalFallback supports these tests.
        "test_model_copy_to_UnsupportedAttribute",
        "test_model_copy_to_UnsupportedConstraint",
        "test_model_supports_constraint_ScalarAffineFunction_EqualTo",
        "test_model_supports_constraint_VariableIndex_EqualTo",
        "test_model_supports_constraint_VectorOfVariables_Nonnegatives",
    ],
)

# Special test for issue #2010

struct _UnsupportedModel <: MOI.ModelLike end

function MOI.supports_constraint(
    ::_UnsupportedModel,
    ::Type{MOI.VariableIndex},
    ::Type{MOI.ZeroOne},
)
    return true
end

MOI.Test.test_attribute_unsupported_constraint(
    _UnsupportedModel(),
    MOI.Test.Config(),
)

@testset "test_attribute_unsupported_constraint_fallback" begin
    model = _UnsupportedModel()
    F, S = MOI.VariableIndex, MOI.ZeroOne
    attr = MOI.ListOfConstraintIndices{F,S}()
    @test_throws MOI.GetAttributeNotAllowed(attr) MOI.get(model, attr)
    attr = MOI.NumberOfConstraints{F,S}()
    @test_throws MOI.GetAttributeNotAllowed(attr) MOI.get(model, attr)
end

@testset "test_warning_on_ambiguous" begin
    model = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    config = MOI.Test.Config()
    # `test_model_Name`` is our target of interest, `test_` is just to avoid
    # running any tests.
    exclude = ["test_model_Name", r"test_.+"]
    @test_logs (:warn,) MOI.Test.runtests(model, config; exclude = exclude)
    exclude = [r"^test_model_Name$", r"test_.+"]
    @test_logs MOI.Test.runtests(model, config; exclude = exclude)
end

@testset "test_HS071_evaluator" begin
    evaluator = MOI.Test.HS071(true, true)
    features = [:Grad, :Jac, :JacVec, :ExprGraph, :Hess, :HessVec]
    @test MOI.features_available(evaluator) == features
    @test_throws(
        ErrorException("Unsupported feature foo"),
        MOI.initialize(evaluator, [:foo]),
    )
    MOI.initialize(evaluator, features)
    x = [1.0, 2.0, 3.0, 4.0]
    @test MOI.eval_objective(evaluator, x) == 27.0
    g = fill(NaN, 2)
    MOI.eval_constraint(evaluator, g, x)
    @test g == [24.0, 30.0]
    grad = fill(NaN, 4)
    MOI.eval_objective_gradient(evaluator, grad, x)
    @test grad == [28.0, 4.0, 5.0, 6.0]
    @test MOI.jacobian_structure(evaluator) ==
          [(1, 1), (1, 2), (1, 3), (1, 4), (2, 1), (2, 2), (2, 3), (2, 4)]
    @test MOI.hessian_objective_structure(evaluator) ==
          [(1, 1), (2, 1), (3, 1), (4, 1), (4, 2), (4, 3)]
    @test MOI.hessian_constraint_structure(evaluator, 1) ==
          [(2, 1), (3, 1), (3, 2), (4, 1), (4, 2), (4, 3)]
    @test MOI.hessian_constraint_structure(evaluator, 2) ==
          [(1, 1), (2, 2), (3, 3), (4, 4)]
    @test MOI.hessian_lagrangian_structure(evaluator) == vcat(
        [(1, 1), (2, 1), (2, 2), (3, 1), (3, 2), (3, 3)],
        [(4, 1), (4, 2), (4, 3), (4, 4)],
    )
    @test MOI.constraint_gradient_structure(evaluator, 1) == [1, 2, 3, 4]
    @test MOI.constraint_gradient_structure(evaluator, 2) == [1, 2, 3, 4]
    ∇g = zeros(4)
    MOI.eval_constraint_gradient(evaluator, ∇g, x, 1)
    @test ∇g == [24.0, 12.0, 8.0, 6.0]
    MOI.eval_constraint_gradient(evaluator, ∇g, x, 2)
    @test ∇g == [2.0, 4.0, 6.0, 8.0]
    J = zeros(8)
    MOI.eval_constraint_jacobian(evaluator, J, x)
    @test J == [24.0, 12.0, 8.0, 6.0, 2.0, 4.0, 6.0, 8.0]
    y = zeros(2)
    w = [1.5, 2.5, 3.5, 4.5]
    MOI.eval_constraint_jacobian_product(evaluator, y, x, w)
    @test y == [121.0, 70.0]
    y = zeros(4)
    w2 = @view(w[1:2])
    MOI.eval_constraint_jacobian_transpose_product(evaluator, y, x, w2)
    @test y == [41.0, 28.0, 27.0, 29.0]
    σ = 1.0
    μ = [1.0, 1.0]
    H = zeros(4)
    MOI.eval_hessian_lagrangian_product(evaluator, H, x, w, σ, μ)
    @test H == [155.5, 61.0, 48.5, 49.0]
    H = zeros(6)
    MOI.eval_hessian_objective(evaluator, H, x)
    @test H == [8.0, 4.0, 4.0, 7.0, 1.0, 1.0]
    MOI.eval_hessian_constraint(evaluator, H, x, 1)
    @test H == [12.0, 8.0, 4.0, 6.0, 3.0, 2.0]
    H = zeros(4)
    MOI.eval_hessian_constraint(evaluator, H, x, 2)
    @test H == [2.0, 2.0, 2.0, 2.0]
    H = zeros(10)
    MOI.eval_hessian_lagrangian(evaluator, H, x, σ, μ)
    @test H == [10.0, 16.0, 2.0, 12.0, 4.0, 2.0, 13.0, 4.0, 3.0, 2.0]
    x = Expr.(:ref, :x, MOI.VariableIndex.(1:4))
    @test MOI.objective_expr(evaluator) ==
          :($(x[1]) * $(x[4]) * ($(x[1]) + $(x[2]) + $(x[3])) + $(x[3]))
    @test MOI.constraint_expr(evaluator, 1) ==
          :($(x[1]) * $(x[2]) * $(x[3]) * $(x[4]) >= 25.0)
    @test MOI.constraint_expr(evaluator, 2) ==
          :($(x[1])^2 + $(x[2])^2 + $(x[3])^2 + $(x[4])^2 == 40.0)
end
