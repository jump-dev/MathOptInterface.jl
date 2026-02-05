# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestTest

using Test
import MathOptInterface as MOI

function runtests()
    for name in names(@__MODULE__; all = true)
        if startswith("$name", "test_")
            @testset "$(name)" begin
                getfield(@__MODULE__, name)()
            end
        end
    end
    return
end

function test_issue_1757()
    MOI.Test.test_model_ScalarFunctionConstantNotZero(
        MOI.Utilities.MockOptimizer(
            MOI.Utilities.Model{Float64}(),
            scalar_function_constant_non_zero = false,
        ),
        MOI.Test.Config(exclude = Any[MOI.ConstraintFunction]),
    )
    return
end

struct IncompleteOptimizer <: MOI.AbstractOptimizer end

# Test exclude_tests_after. This should work despite no methods being added for
# IncompleteOptimizer because every test should get skipped.
function test_exclude_tests_after()
    MOI.Test.runtests(
        IncompleteOptimizer(),
        MOI.Test.Config();
        exclude_tests_after = v"0.0.1",
        verbose = true,
    )
    return
end

# Special test for issue #2010

struct _UnsupportedModel <: MOI.ModelLike end

function MOI.supports_constraint(
    ::_UnsupportedModel,
    ::Type{MOI.VariableIndex},
    ::Type{MOI.ZeroOne},
)
    return true
end

function test_issue_2010()
    MOI.Test.test_attribute_unsupported_constraint(
        _UnsupportedModel(),
        MOI.Test.Config(),
    )
    return
end

function test_attribute_unsupported_constraint_fallback()
    model = _UnsupportedModel()
    F, S = MOI.VariableIndex, MOI.ZeroOne
    attr = MOI.ListOfConstraintIndices{F,S}()
    @test_throws MOI.GetAttributeNotAllowed(attr) MOI.get(model, attr)
    attr = MOI.NumberOfConstraints{F,S}()
    @test_throws MOI.GetAttributeNotAllowed(attr) MOI.get(model, attr)
    return
end

function test_warning_on_ambiguous()
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
    return
end

function test_FeasibilitySenseEvaluator()
    evaluator = MOI.Test.FeasibilitySenseEvaluator(true)
    @test MOI.features_available(evaluator) == [:Grad, :Jac, :Hess, :ExprGraph]
    @test MOI.hessian_lagrangian_structure(evaluator) == [(1, 1)]
    evaluator = MOI.Test.FeasibilitySenseEvaluator(false)
    @test MOI.features_available(evaluator) == [:Grad, :Jac, :ExprGraph]
    @test_throws AssertionError MOI.hessian_lagrangian_structure(evaluator)
    return
end

function test_HS071_evaluator()
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
    return
end

function test_error_handler()
    err = ErrorException("error")
    @test_throws err try
        @assert false
    catch
        MOI.Test._error_handler(err, "test_foo", true)
    end
    err = MOI.AddVariableNotAllowed()
    @test_logs (:warn,) MOI.Test._error_handler(err, "test_foo", true)
    @test_nowarn MOI.Test._error_handler(err, "test_foo", false)
    return
end

"Test a model that errors getting time limit unless it was previously set"
mutable struct ModelTimeLimitSecErrorIfNotSet <: MOI.AbstractOptimizer
    time_limit::Union{Missing,Nothing,Float64}
end

MOI.supports(::ModelTimeLimitSecErrorIfNotSet, ::MOI.TimeLimitSec) = true

function MOI.get(model::ModelTimeLimitSecErrorIfNotSet, attr::MOI.TimeLimitSec)
    if model.time_limit === missing
        throw(MOI.GetAttributeNotAllowed(attr))
    end
    return model.time_limit::Union{Nothing,Float64}
end

function MOI.set(model::ModelTimeLimitSecErrorIfNotSet, ::MOI.TimeLimitSec, v)
    model.time_limit = v
    return
end

function test_attribute_TimeLimitSec()
    model = ModelTimeLimitSecErrorIfNotSet(missing)
    MOI.Test.test_attribute_TimeLimitSec(model, MOI.Test.Config())
    return
end

end  # module

TestTest.runtests()
