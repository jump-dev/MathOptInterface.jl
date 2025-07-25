# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestNonlinear

using Test
import MathOptInterface as MOI
import ForwardDiff
import LinearAlgebra

const Nonlinear = MOI.Nonlinear

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

_hessian(f, x) = LinearAlgebra.LowerTriangular(ForwardDiff.hessian(f, x))

function test_copy()
    model = Nonlinear.Model()
    @test_throws(
        ErrorException("Copying nonlinear problems not yet implemented"),
        copy(model),
    )
    return
end

function test_parse_unable()
    model = Nonlinear.Model()
    x = MOI.VariableIndex(1)
    input = :(f($x))
    @test_throws(
        MOI.UnsupportedNonlinearOperator(:f),
        Nonlinear.set_objective(model, input),
    )
    return
end

function test_parse_sin()
    model = Nonlinear.Model()
    x = MOI.VariableIndex(1)
    Nonlinear.set_objective(model, :(sin($x)))
    evaluator = Nonlinear.Evaluator(model)
    MOI.initialize(evaluator, [:ExprGraph])
    @test MOI.objective_expr(evaluator) == :(sin(x[$x]))
    return
end

function test_parse_sin_squared()
    model = Nonlinear.Model()
    x = MOI.VariableIndex(1)
    Nonlinear.set_objective(model, :(sin($x)^2))
    evaluator = Nonlinear.Evaluator(model)
    MOI.initialize(evaluator, [:ExprGraph])
    @test MOI.objective_expr(evaluator) == :(sin(x[$x])^2.0)
    return
end

function test_parse_ifelse()
    model = Nonlinear.Model()
    x = MOI.VariableIndex(1)
    Nonlinear.set_objective(model, :(ifelse($x, 1, 2)))
    evaluator = Nonlinear.Evaluator(model)
    MOI.initialize(evaluator, [:ExprGraph])
    @test MOI.objective_expr(evaluator) == :(ifelse(x[$x], 1.0, 2.0))
    return
end

function test_parse_ifelse_inequality_less()
    model = Nonlinear.Model()
    x = MOI.VariableIndex(1)
    Nonlinear.set_objective(model, :(ifelse($x < 1, $x - 1, $x + 1)))
    evaluator = Nonlinear.Evaluator(model)
    MOI.initialize(evaluator, [:ExprGraph])
    @test MOI.objective_expr(evaluator) ==
          :(ifelse(x[$x] < 1.0, x[$x] - 1.0, x[$x] + 1.0))
    return
end

function test_parse_ifelse_inequality_greater()
    model = Nonlinear.Model()
    x = MOI.VariableIndex(1)
    Nonlinear.set_objective(model, :(ifelse($x > 1, $x - 1, $x + 1)))
    evaluator = Nonlinear.Evaluator(model)
    MOI.initialize(evaluator, [:ExprGraph])
    @test MOI.objective_expr(evaluator) ==
          :(ifelse(x[$x] > 1.0, x[$x] - 1.0, x[$x] + 1.0))
    return
end

function test_parse_ifelse_comparison()
    model = Nonlinear.Model()
    x = MOI.VariableIndex(1)
    Nonlinear.set_objective(model, :(ifelse(0 <= $x <= 1, $x - 1, $x + 1)))
    evaluator = Nonlinear.Evaluator(model)
    MOI.initialize(evaluator, [:ExprGraph])
    @test MOI.objective_expr(evaluator) ==
          :(ifelse(0.0 <= x[$x] <= 1.0, x[$x] - 1.0, x[$x] + 1.0))
    return
end

function test_parse_ifelse_logic_inequality()
    model = Nonlinear.Model()
    x = MOI.VariableIndex(1)
    expr = :(ifelse($x < 1.0 || $x > 2.0, $x - 1.0, $x + 1.0))
    Nonlinear.set_objective(model, expr)
    evaluator = Nonlinear.Evaluator(model)
    MOI.initialize(evaluator, [:ExprGraph])
    @test MOI.objective_expr(evaluator) ==
          :(ifelse(x[$x] < 1.0 || x[$x] > 2.0, x[$x] - 1.0, x[$x] + 1.0))
    return
end

function test_parse_splat_prod()
    model = Nonlinear.Model()
    x = MOI.VariableIndex.(1:3)
    Nonlinear.set_objective(model, :(*($x...)))
    evaluator = Nonlinear.Evaluator(model)
    MOI.initialize(evaluator, [:ExprGraph])
    @test MOI.objective_expr(evaluator) ==
          :(x[$(x[1])] * x[$(x[2])] * x[$(x[3])])
    return
end

function test_parse_splat_top_level()
    model = Nonlinear.Model()
    x = MOI.VariableIndex.(1:3)
    @test_throws(
        ErrorException(
            "Unsupported use of the splatting operator. This is only " *
            "supported in the arguments of a function call.",
        ),
        Nonlinear.set_objective(model, :(x...)),
    )
    return
end

function test_parse_splat_expr()
    model = Nonlinear.Model()
    x = MOI.VariableIndex.(1:3)
    @test_throws(
        ErrorException(
            "Unsupported use of the splatting operator. JuMP supports " *
            "splatting only symbols. For example, `x...` is ok, but " *
            "`(x + 1)...`, `[x; y]...` and `g(f(y)...)` are not.",
        ),
        Nonlinear.set_objective(model, :(*((x ./ 2)...))),
    )
    return
end

function test_parse_univariate_prod()
    model = Nonlinear.Model()
    x = MOI.VariableIndex(1)
    Nonlinear.set_objective(model, :(*($x)))
    evaluator = Nonlinear.Evaluator(model)
    MOI.initialize(evaluator, [:ExprGraph])
    @test MOI.objective_expr(evaluator) == :(*(x[$x]))
    return
end

function test_parse_string()
    model = Nonlinear.Model()
    x = MOI.VariableIndex(1)
    @test_throws(
        ErrorException(
            "Unexpected object abc of type String in nonlinear expression.",
        ),
        Nonlinear.set_objective(model, :($x + "abc")),
    )
    return
end

function test_parse_array()
    model = Nonlinear.Model()
    x = [MOI.VariableIndex(1)]
    c = [1.0]
    @test_throws(
        ErrorException(
            "Unexpected array $(c') in nonlinear expression. Nonlinear " *
            "expressions may contain only scalar expressions.",
        ),
        Nonlinear.set_objective(model, :($(c') * $x)),
    )
    return
end

function test_parse_unsupported_expression()
    model = Nonlinear.Model()
    x = (y = 1,)
    @test_throws(
        ErrorException("Unsupported expression: $(:(x.y))"),
        Nonlinear.set_objective(model, :(x.y)),
    )
    return
end

function test_moi_variable_parse()
    model = Nonlinear.Model()
    x = MOI.VariableIndex(1)
    expr = Nonlinear.parse_expression(model, :($x))
    @test expr.nodes == [Nonlinear.Node(Nonlinear.NODE_MOI_VARIABLE, 1, -1)]
    @test isempty(expr.values)
    return
end

function test_expression_parse()
    model = Nonlinear.Model()
    x = MOI.VariableIndex(1)
    ex = Nonlinear.add_expression(model, :(sin($x)^2))
    @test model[ex] isa Nonlinear.Expression
    return
end

function test_parameter_parse()
    model = Nonlinear.Model()
    p = Nonlinear.add_parameter(model, 1.2)
    expr = Nonlinear.parse_expression(model, :($p))
    @test expr.nodes == [Nonlinear.Node(Nonlinear.NODE_PARAMETER, 1, -1)]
    @test isempty(expr.values)
    @test model.parameters == [1.2]
    return
end

function test_parameter_set()
    model = Nonlinear.Model()
    p = Nonlinear.add_parameter(model, 1.2)
    @test model.parameters == [1.2]
    @test model[p] == 1.2
    model[p] = 2.1
    @test model.parameters == [2.1]
    @test model[p] == 2.1
    return
end

function test_set_objective()
    model = Nonlinear.Model()
    x = MOI.VariableIndex(1)
    input = :($x^2 + 1)
    Nonlinear.set_objective(model, input)
    @test model.objective == Nonlinear.parse_expression(model, input)
    evaluator = Nonlinear.Evaluator(model)
    MOI.initialize(evaluator, [:ExprGraph])
    @test MOI.objective_expr(evaluator) == :(x[$x]^2.0 + 1.0)
    return
end

function test_set_objective_subexpression()
    model = Nonlinear.Model()
    x = MOI.VariableIndex(1)
    input = :($x^2 + 1)
    expr = Nonlinear.add_expression(model, input)
    Nonlinear.set_objective(model, :($expr^2))
    evaluator = Nonlinear.Evaluator(model)
    MOI.initialize(evaluator, [:ExprGraph])
    @test MOI.objective_expr(evaluator) == :((x[$x]^2.0 + 1.0)^2.0)
    return
end

function test_set_objective_nested_subexpression()
    model = Nonlinear.Model()
    x = MOI.VariableIndex(1)
    input = :($x^2 + 1)
    expr = Nonlinear.add_expression(model, input)
    expr_2 = Nonlinear.add_expression(model, :($expr^2))
    Nonlinear.set_objective(model, :($expr_2^2))
    evaluator = Nonlinear.Evaluator(model)
    MOI.initialize(evaluator, [:ExprGraph])
    @test MOI.objective_expr(evaluator) == :(((x[$x]^2.0 + 1.0)^2.0)^2.0)
    return
end

function test_set_objective_parameter()
    model = Nonlinear.Model()
    x = MOI.VariableIndex(1)
    p = Nonlinear.add_parameter(model, 1.2)
    Nonlinear.set_objective(model, :($x^2 + $p))
    evaluator = Nonlinear.Evaluator(model)
    MOI.initialize(evaluator, [:ExprGraph])
    @test MOI.objective_expr(evaluator) == :(x[$x]^2.0 + 1.2)
    return
end

function test_add_constraint_less_than()
    model = Nonlinear.Model()
    x = MOI.VariableIndex(1)
    func = :($x^2 + 1)
    set = MOI.LessThan(1.0)
    c = Nonlinear.add_constraint(model, func, set)
    @test model[c].set == set
    evaluator = Nonlinear.Evaluator(model)
    MOI.initialize(evaluator, [:ExprGraph])
    @test MOI.constraint_expr(evaluator, 1) == :(x[$x]^2.0 + 1.0 <= 1.0)
    return
end

function test_add_constraint_delete()
    model = Nonlinear.Model()
    x = MOI.VariableIndex(1)
    c1 = Nonlinear.add_constraint(model, :($x^2 + 1), MOI.LessThan(1.0))
    _ = Nonlinear.add_constraint(model, :(sqrt($x)), MOI.LessThan(1.0))
    evaluator = Nonlinear.Evaluator(model)
    MOI.initialize(evaluator, [:ExprGraph])
    @test MOI.constraint_expr(evaluator, 1) == :(x[$x]^2.0 + 1.0 <= 1.0)
    @test MOI.constraint_expr(evaluator, 2) == :(sqrt(x[$x]) <= 1.0)
    Nonlinear.delete(model, c1)
    evaluator = Nonlinear.Evaluator(model)
    MOI.initialize(evaluator, [:ExprGraph])
    @test MOI.constraint_expr(evaluator, 1) == :(sqrt(x[$x]) <= 1.0)
    @test_throws BoundsError MOI.constraint_expr(evaluator, 2)
    return
end

function test_add_constraint_greater_than()
    model = Nonlinear.Model()
    x = MOI.VariableIndex(1)
    func = :($x^2 + 1)
    set = MOI.GreaterThan(1.0)
    c = Nonlinear.add_constraint(model, func, set)
    @test model[c].set == set
    evaluator = Nonlinear.Evaluator(model)
    MOI.initialize(evaluator, [:ExprGraph])
    @test MOI.constraint_expr(evaluator, 1) == :(x[$x]^2.0 + 1.0 >= 1.0)
    return
end

function test_add_constraint_equal_to()
    model = Nonlinear.Model()
    x = MOI.VariableIndex(1)
    func, set = :($x^2 + 1), MOI.EqualTo(1.0)
    c = Nonlinear.add_constraint(model, func, set)
    @test model[c].set == set
    evaluator = Nonlinear.Evaluator(model)
    MOI.initialize(evaluator, [:ExprGraph])
    @test MOI.constraint_expr(evaluator, 1) == :(x[$x]^2.0 + 1.0 == 1.0)
    return
end

function test_add_constraint_interval()
    model = Nonlinear.Model()
    x = MOI.VariableIndex(1)
    func, set = :($x^2 + 1), MOI.Interval(-1.0, 1.0)
    c = Nonlinear.add_constraint(model, func, set)
    @test model[c].set == set
    evaluator = Nonlinear.Evaluator(model)
    MOI.initialize(evaluator, [:ExprGraph])
    @test MOI.constraint_expr(evaluator, 1) == :(-1.0 <= x[$x]^2.0 + 1.0 <= 1.0)
    return
end

function test_eval_univariate_function()
    r = Nonlinear.OperatorRegistry()
    for (op, x, y) in [
        (:+, 1.0, 1.0),
        (:-, 1.0, -1.0),
        (:abs, -1.1, 1.1),
        (:abs, 1.1, 1.1),
        (:sin, 1.1, sin(1.1)),
    ]
        id = r.univariate_operator_to_id[op]
        @test Nonlinear.eval_univariate_function(r, op, x) == y
        @test Nonlinear.eval_univariate_function(r, id, x) == y
    end
    return
end

function test_eval_univariate_missing_hessian()
    r = Nonlinear.OperatorRegistry()
    x = 2.0
    @test Nonlinear.eval_univariate_function(r, :asec, x) ≈ asec(x)
    @test Nonlinear.eval_univariate_gradient(r, :asec, x) ≈
          1 / (abs(x) * sqrt(x^2 - 1))
    @test_throws(
        ErrorException("Hessian is not defined for operator asec"),
        Nonlinear.eval_univariate_hessian(r, :asec, x),
    )
    return
end

function test_eval_univariate_hessian_bad_id()
    r = Nonlinear.OperatorRegistry()
    err = ErrorException("Invalid id for univariate operator: -1")
    @test_throws err Nonlinear.eval_univariate_function(r, -1, 1.0)
    @test_throws err Nonlinear.eval_univariate_gradient(r, -1, 1.0)
    @test_throws err Nonlinear.eval_univariate_hessian(r, -1, 1.0)
    return
end

function test_eval_univariate_gradient()
    r = Nonlinear.OperatorRegistry()
    for (op, x, y) in [
        (:+, 1.2, 1.0),
        (:-, 1.2, -1.0),
        (:abs, -1.1, -1.0),
        (:abs, 1.1, 1.0),
        (:sin, 1.1, cos(1.1)),
    ]
        id = r.univariate_operator_to_id[op]
        @test Nonlinear.eval_univariate_gradient(r, op, x) == y
        @test Nonlinear.eval_univariate_gradient(r, id, x) == y
    end
    return
end

function test_eval_univariate_function_and_gradient()
    r = Nonlinear.OperatorRegistry()
    for (op, x, y) in [
        (:+, 1.2, (1.2, 1.0)),
        (:-, 1.2, (-1.2, -1.0)),
        (:abs, -1.1, (1.1, -1.0)),
        (:abs, 1.1, (1.1, 1.0)),
        (:sin, 1.1, (sin(1.1), cos(1.1))),
    ]
        id = r.univariate_operator_to_id[op]
        @test Nonlinear.eval_univariate_function_and_gradient(r, op, x) == y
        @test Nonlinear.eval_univariate_function_and_gradient(r, id, x) == y
    end
    return
end

function test_eval_univariate_hessian()
    r = Nonlinear.OperatorRegistry()
    for (op, x, y) in [
        (:+, 1.2, 0.0),
        (:-, 1.2, 0.0),
        (:abs, -1.1, 0.0),
        (:abs, 1.1, 0.0),
        (:sin, 1.0, -sin(1.0)),
    ]
        id = r.univariate_operator_to_id[op]
        @test Nonlinear.eval_univariate_hessian(r, op, x) == y
        @test Nonlinear.eval_univariate_hessian(r, id, x) == y
    end
    return
end

function test_eval_univariate_function_registered_method_error()
    r = Nonlinear.Model()
    f(x::Float64) = sin(x)^2
    @test_throws ErrorException Nonlinear.register_operator(r, :f, 1, f)
    return
end

function test_univariate_function_register_twice()
    r = Nonlinear.Model()
    f(x) = x
    Nonlinear.register_operator(r, :f, 1, f)
    @test_throws(
        ErrorException("Operator f is already registered."),
        Nonlinear.register_operator(r, :f, 1, f),
    )
    @test_throws(
        ErrorException("Operator f is already registered."),
        Nonlinear.register_operator(r, :f, 2, f),
    )
    return
end

function test_multivariate_function_register_twice()
    r = Nonlinear.Model()
    f(x, y) = x + y
    Nonlinear.register_operator(r, :f, 2, f)
    @test_throws(
        ErrorException("Operator f is already registered."),
        Nonlinear.register_operator(r, :f, 1, f),
    )
    @test_throws(
        ErrorException("Operator f is already registered."),
        Nonlinear.register_operator(r, :f, 2, f),
    )
    return
end

function test_auto_register()
    r = Nonlinear.OperatorRegistry()
    f(x, y) = x + y
    @test_throws ErrorException Nonlinear.assert_registered(r, :f, 2)
    @test_logs (:warn,) Nonlinear.register_operator_if_needed(r, :f, 2, f)
    Nonlinear.assert_registered(r, :f, 2)
    return
end

function test_register_univariate_function_return_type()
    r = Nonlinear.OperatorRegistry()
    f(x) = x < 1 ? "x" : x
    @test_throws(
        ErrorException(
            "Expected return type of `Float64` from the user-defined " *
            "function :f, but got `String`.",
        ),
        Nonlinear.register_operator(r, :f, 1, f),
    )
    return
end

function test_eval_univariate_function_return_type()
    r = Nonlinear.OperatorRegistry()
    f(x) = x < 1 ? x : "x"
    Nonlinear.register_operator(r, :f, 1, f)
    @test_throws(
        ErrorException(
            "Expected return type of Float64 from a user-defined function, " *
            "but got String.",
        ),
        Nonlinear.eval_univariate_function(r, :f, 1.2),
    )
    return
end

function test_eval_univariate_function_registered()
    r = Nonlinear.OperatorRegistry()
    f(x) = sin(x)^2
    grad_calls = 0
    f′(x) = (grad_calls += 1; 2 * sin(x) * cos(x))
    hess_calls = 0
    f′′(x) = (hess_calls += 1; 2 * (cos(x)^2 - sin(x)^2))
    Nonlinear.register_operator(r, :f, 1, f)
    x = 1.2
    @test Nonlinear.eval_univariate_function(r, :f, x) ≈ f(x)
    @test Nonlinear.eval_univariate_gradient(r, :f, x) ≈ f′(x)
    @test grad_calls == 1
    @test Nonlinear.eval_univariate_hessian(r, :f, x) ≈ f′′(x)
    @test hess_calls == 1
    return
end

function test_eval_univariate_function_registered_grad()
    r = Nonlinear.OperatorRegistry()
    f(x) = sin(x)^2
    grad_calls = 0
    f′(x) = (grad_calls += 1; 2 * sin(x) * cos(x))
    hess_calls = 0
    f′′(x) = (hess_calls += 1; 2 * (cos(x)^2 - sin(x)^2))
    Nonlinear.register_operator(r, :f, 1, f, f′)
    @test grad_calls == 2
    x = 1.2
    @test Nonlinear.eval_univariate_function(r, :f, x) ≈ f(x)
    @test Nonlinear.eval_univariate_gradient(r, :f, x) ≈ f′(x)
    @test grad_calls == 4
    @test Nonlinear.eval_univariate_hessian(r, :f, x) ≈ f′′(x)
    @test hess_calls == 1
    return
end

function test_eval_univariate_function_registered_grad_hess()
    r = Nonlinear.OperatorRegistry()
    f(x) = sin(x)^2
    grad_calls = 0
    f′(x) = (grad_calls += 1; 2 * sin(x) * cos(x))
    hess_calls = 0
    f′′(x) = (hess_calls += 1; 2 * (cos(x)^2 - sin(x)^2))
    Nonlinear.register_operator(r, :f, 1, f, f′, f′′)
    x = 1.2
    @test Nonlinear.eval_univariate_function(r, :f, x) ≈ f(x)
    @test Nonlinear.eval_univariate_gradient(r, :f, x) ≈ f′(x)
    @test grad_calls == 2
    @test Nonlinear.eval_univariate_hessian(r, :f, x) ≈ f′′(x)
    @test hess_calls == 2
    return
end

function test_eval_multivariate_function()
    r = Nonlinear.OperatorRegistry()
    x = [1.1, 2.2]
    @test Nonlinear.eval_multivariate_function(r, :+, x) ≈ 3.3
    @test Nonlinear.eval_multivariate_function(r, :-, x) ≈ -1.1
    @test Nonlinear.eval_multivariate_function(r, :*, x) ≈ 1.1 * 2.2
    @test Nonlinear.eval_multivariate_function(r, :^, x) ≈ 1.1^2.2
    @test Nonlinear.eval_multivariate_function(r, :/, x) ≈ 1.1 / 2.2
    @test Nonlinear.eval_multivariate_function(r, :ifelse, [1; x]) == 1.1
    @test Nonlinear.eval_multivariate_function(r, :ifelse, [0; x]) == 2.2
    return
end

function test_eval_multivariate_gradient()
    r = Nonlinear.OperatorRegistry()
    x = [1.1, 2.2]
    g = zeros(2)
    Nonlinear.eval_multivariate_gradient(r, :+, g, x)
    @test g == [1.0, 1.0]
    Nonlinear.eval_multivariate_gradient(r, :-, g, x)
    @test g == [1.0, -1.0]
    Nonlinear.eval_multivariate_gradient(r, :*, g, x)
    @test g ≈ [2.2, 1.1]
    Nonlinear.eval_multivariate_gradient(r, :^, g, x)
    @test g ≈ [2.2 * 1.1^1.2, 1.1^2.2 * log(1.1)]
    Nonlinear.eval_multivariate_gradient(r, :^, g, [1.1, 1.0])
    @test g ≈ [1.0, 1.1 * log(1.1)]
    Nonlinear.eval_multivariate_gradient(r, :^, g, [1.1, 2.0])
    @test g ≈ [2.0 * 1.1, 1.1^2.0 * log(1.1)]
    Nonlinear.eval_multivariate_gradient(r, :^, g, [-1.1, 2.0])
    @test g[1] ≈ 2.0 * -1.1
    @test isnan(g[2])
    Nonlinear.eval_multivariate_gradient(r, :/, g, x)
    @test g ≈ [1 / 2.2, -1.1 / 2.2^2]
    g = zeros(3)
    Nonlinear.eval_multivariate_gradient(r, :ifelse, g, [1; x])
    @test g ≈ [0.0, 1.0, 0.0]
    Nonlinear.eval_multivariate_gradient(r, :ifelse, g, [0; x])
    @test g ≈ [0.0, 0.0, 1.0]
    return
end

function test_eval_multivariate_gradient_mult()
    r = Nonlinear.OperatorRegistry()
    x = [1.1, 0.0, 2.2]
    g = zeros(3)
    Nonlinear.eval_multivariate_gradient(r, :*, g, x)
    @test g ≈ [0.0, 1.1 * 2.2, 0.0]
    x = [1.1, 3.3, 2.2]
    Nonlinear.eval_multivariate_gradient(r, :*, g, x)
    @test g ≈ [3.3 * 2.2, 1.1 * 2.2, 1.1 * 3.3]
    return
end

function test_eval_multivariate_gradient_univariate_mult()
    r = Nonlinear.OperatorRegistry()
    x = [1.1]
    g = zeros(1)
    Nonlinear.eval_multivariate_gradient(r, :*, g, x)
    @test g == [1.0]
    return
end

function test_eval_multivariate_hessian_shortcut()
    r = Nonlinear.OperatorRegistry()
    x = [1.1]
    H = LinearAlgebra.LowerTriangular(zeros(1, 1))
    for op in (:+, :-, :ifelse)
        @test !MOI.Nonlinear.eval_multivariate_hessian(r, op, H, x)
    end
    return
end

function test_eval_multivariate_hessian_prod()
    r = Nonlinear.OperatorRegistry()
    # 1-arg *
    x = [1.1]
    H = LinearAlgebra.LowerTriangular(zeros(1, 1))
    @test Nonlinear.eval_multivariate_hessian(r, :*, H, x)
    @test H ≈ _hessian(x -> *(x[1]), x)
    # 2-arg *
    x = [1.1, 2.2]
    H = LinearAlgebra.LowerTriangular(zeros(2, 2))
    @test Nonlinear.eval_multivariate_hessian(r, :*, H, x)
    @test H ≈ _hessian(x -> x[1] * x[2], x)
    # 3-arg *
    x = [1.1, 2.2, 3.3]
    H = LinearAlgebra.LowerTriangular(zeros(3, 3))
    @test Nonlinear.eval_multivariate_hessian(r, :*, H, x)
    @test H ≈ _hessian(x -> x[1] * x[2] * x[3], x)
    return
end

function test_eval_multivariate_hessian_prod_allocated()
    r = Nonlinear.OperatorRegistry()
    x = [1.1, 2.2]
    H = LinearAlgebra.LowerTriangular(zeros(2, 2))
    Nonlinear.eval_multivariate_hessian(r, :*, H, x)  # ensure precompiled
    @test (@allocated Nonlinear.eval_multivariate_hessian(r, :*, H, x)) == 0
    return
end

function test_eval_multivariate_hessian_exponentiation()
    r = Nonlinear.OperatorRegistry()
    # ^1.0
    x = [1.1, 1.0]
    H = LinearAlgebra.LowerTriangular(zeros(2, 2))
    @test Nonlinear.eval_multivariate_hessian(r, :^, H, x)
    @test H ≈ _hessian(x -> x[1]^x[2], x)
    # ^2.0
    x = [1.1, 2.0]
    H = LinearAlgebra.LowerTriangular(zeros(2, 2))
    @test Nonlinear.eval_multivariate_hessian(r, :^, H, x)
    @test H ≈ _hessian(x -> x[1]^x[2], x)
    # 2-arg ^
    x = [1.1, 2.2]
    H = LinearAlgebra.LowerTriangular(zeros(2, 2))
    @test Nonlinear.eval_multivariate_hessian(r, :^, H, x)
    @test H ≈ _hessian(x -> x[1]^x[2], x)
    return
end

function test_eval_multivariate_hessian_exponentiation_allocated()
    r = Nonlinear.OperatorRegistry()
    # ^1.0
    x = [1.1, 1.0]
    H = LinearAlgebra.LowerTriangular(zeros(2, 2))
    @test Nonlinear.eval_multivariate_hessian(r, :^, H, x)
    for x2 in [1.0, 2.0, 2.2]
        x[2] = x2
        @test (@allocated Nonlinear.eval_multivariate_hessian(r, :^, H, x)) == 0
    end
    return
end

function test_eval_multivariate_hessian_division()
    r = Nonlinear.OperatorRegistry()
    # 2-arg /
    x = [1.1, 2.2]
    H = LinearAlgebra.LowerTriangular(zeros(2, 2))
    @test Nonlinear.eval_multivariate_hessian(r, :/, H, x)
    @test H ≈ _hessian(x -> x[1] / x[2], x)
    return
end

function test_eval_multivariate_hessian_division_allocated()
    r = Nonlinear.OperatorRegistry()
    x = [1.1, 2.2]
    H = LinearAlgebra.LowerTriangular(zeros(2, 2))
    Nonlinear.eval_multivariate_hessian(r, :/, H, x)  # ensure precompiled
    @test (@allocated Nonlinear.eval_multivariate_hessian(r, :/, H, x)) == 0
    return
end

function test_eval_multivariate_function_registered()
    r = Nonlinear.OperatorRegistry()
    f(x...) = x[1]^2 + x[1] * x[2] + x[2]^2
    Nonlinear.register_operator(r, :f, 2, f)
    x = [1.1, 2.2]
    @test Nonlinear.eval_multivariate_function(r, :f, x) ≈ f(x...)
    g = zeros(2)
    Nonlinear.eval_multivariate_gradient(r, :f, g, x)
    @test g ≈ [2 * x[1] + x[2], x[1] + 2 * x[2]]
    H = LinearAlgebra.LowerTriangular(zeros(2, 2))
    @test_throws(
        ErrorException,
        Nonlinear.eval_multivariate_hessian(r, :f, H, x),
    )
    return
end

function test_eval_multivariate_function_registered_log()
    r = Nonlinear.OperatorRegistry()
    f(x...) = log(x[1] - 1)
    Nonlinear.register_operator(r, :f, 2, f)
    x = [1.1, 2.2]
    @test Nonlinear.eval_multivariate_function(r, :f, x) ≈ f(x...)
    x = [0.0, 0.0]
    g = zeros(2)
    @test_throws DomainError Nonlinear.eval_multivariate_gradient(r, :f, g, x)
    return
end

function test_eval_multivariate_function_method_error()
    r = Nonlinear.OperatorRegistry()
    function f(x...)
        if x[1] > 1
            y = zeros(Float64, 1)
            y[1] = x[1]
            return y[1]^2
        end
        return x[1]^2 + x[1] * x[2] + x[2]^2
    end
    Nonlinear.register_operator(r, :f, 2, f)
    x = [1.1, 2.2]
    g = [0.0, 0.0]
    @test_throws(
        ErrorException,
        Nonlinear.eval_multivariate_gradient(r, :f, g, x),
    )
    return
end

function test_eval_multivariate_function_registered_grad()
    r = Nonlinear.OperatorRegistry()
    f(x...) = x[1]^2 + x[1] * x[2] + x[2]^2
    grad_calls = 0
    function ∇f(g, x...)
        grad_calls += 1
        g[1] = 2 * x[1] + x[2]
        g[2] = x[1] + 2 * x[2]
        return
    end
    Nonlinear.register_operator(r, :f, 2, f, ∇f)
    x = [1.1, 2.2]
    @test Nonlinear.eval_multivariate_function(r, :f, x) ≈ f(x...)
    g = zeros(2)
    Nonlinear.eval_multivariate_gradient(r, :f, g, x)
    @test g ≈ [2 * x[1] + x[2], x[1] + 2 * x[2]]
    @test grad_calls == 1
    H = LinearAlgebra.LowerTriangular(zeros(2, 2))
    @test_throws(
        ErrorException("Hessian is not defined for operator f"),
        Nonlinear.eval_multivariate_hessian(r, :f, H, x),
    )
    return
end

function test_eval_multivariate_function_registered_hessian()
    r = Nonlinear.OperatorRegistry()
    f(x...) = x[1]^2 + x[1] * x[2] + x[2]^2
    grad_calls = 0
    function ∇f(g, x...)
        grad_calls += 1
        g[1] = 2 * x[1] + x[2]
        g[2] = x[1] + 2 * x[2]
        return
    end
    hess_calls = 0
    function ∇²f(H, x...)
        hess_calls += 1
        H[1, 1] = 2.0
        H[2, 1] = 1.0
        H[2, 2] = 2.0
        return
    end
    Nonlinear.register_operator(r, :f, 2, f, ∇f, ∇²f)
    x = [1.1, 2.2]
    @test Nonlinear.eval_multivariate_function(r, :f, x) ≈ f(x...)
    g = zeros(2)
    Nonlinear.eval_multivariate_gradient(r, :f, g, x)
    @test g ≈ [2 * x[1] + x[2], x[1] + 2 * x[2]]
    @test grad_calls == 1
    H = LinearAlgebra.LowerTriangular(zeros(2, 2))
    @test Nonlinear.eval_multivariate_hessian(r, :f, H, x) == true
    @test H == [2.0 0.0; 1.0 2.0]
    @test hess_calls == 1
    return
end

function test_eval_logic_function()
    r = Nonlinear.OperatorRegistry()
    for lhs in (true, false), rhs in (true, false)
        @test Nonlinear.eval_logic_function(r, :&&, lhs, rhs) == (lhs && rhs)
        @test Nonlinear.eval_logic_function(r, :||, lhs, rhs) == (lhs || rhs)
        @test_throws(
            AssertionError,
            Nonlinear.eval_logic_function(r, :⊻, lhs, rhs),
        )
    end
    return
end

function test_eval_comprison_function()
    r = Nonlinear.OperatorRegistry()
    for lhs in (true, false), rhs in (true, false)
        @test Nonlinear.eval_comparison_function(r, :<=, lhs, rhs) ==
              (lhs <= rhs)
        @test Nonlinear.eval_comparison_function(r, :>=, lhs, rhs) ==
              (lhs >= rhs)
        @test Nonlinear.eval_comparison_function(r, :(==), lhs, rhs) ==
              (lhs == rhs)
        @test Nonlinear.eval_comparison_function(r, :<, lhs, rhs) == (lhs < rhs)
        @test Nonlinear.eval_comparison_function(r, :>, lhs, rhs) == (lhs > rhs)
        @test_throws(
            AssertionError,
            Nonlinear.eval_comparison_function(r, :⊻, lhs, rhs),
        )
    end
    return
end

function test_features_available()
    model = Nonlinear.Model()
    evaluator = Nonlinear.Evaluator(model)
    @test MOI.features_available(evaluator) == [:ExprGraph]
    return
end

function test_features_available_Default()
    model = Nonlinear.Model()
    evaluator = Nonlinear.Evaluator(
        model,
        Nonlinear.ExprGraphOnly(),
        MOI.VariableIndex[],
    )
    @test MOI.features_available(evaluator) == [:ExprGraph]
    return
end

function test_add_constraint_ordinal_index()
    model = Nonlinear.Model()
    x = MOI.VariableIndex(1)
    constraints = [
        Nonlinear.add_constraint(model, :($x), MOI.LessThan(1.0 * i)) for
        i in 1:4
    ]
    for i in 1:4
        @test MOI.is_valid(model, constraints[i])
    end
    evaluator = Nonlinear.Evaluator(model)
    MOI.initialize(evaluator, Symbol[])
    for i in 1:4
        @test Nonlinear.ordinal_index(evaluator, constraints[i]) == i
    end
    Nonlinear.delete(model, constraints[1])
    Nonlinear.delete(model, constraints[3])
    @test !MOI.is_valid(model, constraints[1])
    @test MOI.is_valid(model, constraints[2])
    @test !MOI.is_valid(model, constraints[3])
    @test MOI.is_valid(model, constraints[4])
    MOI.initialize(evaluator, Symbol[])
    @test Nonlinear.ordinal_index(evaluator, constraints[2]) == 1
    @test_throws(
        ErrorException("Invalid constraint index $(constraints[3])"),
        Nonlinear.ordinal_index(evaluator, constraints[3]),
    )
    @test Nonlinear.ordinal_index(evaluator, constraints[4]) == 2
    return
end

function test_show()
    model = Nonlinear.Model()
    evaluator = Nonlinear.Evaluator(model)
    @test occursin(":ExprGraph", sprint(show, evaluator))
    return
end

function test_evaluate_comparison()
    model = Nonlinear.Model()
    x = MOI.VariableIndex(1)
    ex = Nonlinear.add_expression(model, :(ifelse($x < 1, -1.0, 1.0)))
    @test Nonlinear.evaluate(Dict(x => 1.1), model, ex) == 1.0
    @test Nonlinear.evaluate(Dict(x => 0.9), model, ex) == -1.0
    return
end

function test_evaluate_domain_error()
    model = Nonlinear.Model()
    x = MOI.VariableIndex(1)
    ex = Nonlinear.add_expression(model, :(ifelse($x > 0, log($x), 0.0)))
    @test Nonlinear.evaluate(Dict(x => 1.1), model, ex) == log(1.1)
    @test Nonlinear.evaluate(Dict(x => 0.0), model, ex) == 0.0
    ex = Nonlinear.add_expression(model, :(ifelse($x > 0, $x^1.5, -(-$x)^1.5)))
    @test Nonlinear.evaluate(Dict(x => 1.1), model, ex) ≈ 1.1^1.5
    @test Nonlinear.evaluate(Dict(x => -1.1), model, ex) ≈ -(1.1^1.5)
    return
end

function test_evaluate_logic()
    model = Nonlinear.Model()
    x = MOI.VariableIndex(1)
    ex = Nonlinear.add_expression(model, :(ifelse($x < 0 || $x > 1, -1.0, 1.0)))
    @test Nonlinear.evaluate(Dict(x => 1.1), model, ex) == -1.0
    @test Nonlinear.evaluate(Dict(x => 0.9), model, ex) == 1.0
    @test Nonlinear.evaluate(Dict(x => -0.9), model, ex) == -1.0
    return
end

function test_evaluate_subexpressions()
    model = Nonlinear.Model()
    x = MOI.VariableIndex(1)
    p = Nonlinear.add_parameter(model, 1.23)
    ex = Nonlinear.add_expression(model, :(*($p, $x, $x)))
    ex = Nonlinear.add_expression(model, :($ex + sqrt($ex)))
    ex_v = 1.23 * 1.1 * 1.1
    @test Nonlinear.evaluate(Dict(x => 1.1), model, ex) ≈ ex_v + sqrt(ex_v)
    model[p] = 3.21
    ex_v = 3.21 * 1.2 * 1.2
    @test Nonlinear.evaluate(Dict(x => 1.2), model, ex) ≈ ex_v + sqrt(ex_v)
    return
end

function test_evaluate_manny_arguments()
    model = MOI.Nonlinear.Model()
    x = MOI.VariableIndex.(1:20)
    v = Dict(xi => xi.value for xi in x)
    expr = MOI.Nonlinear.add_expression(
        model,
        Expr(:call, :+, [:(sqrt($(x[i]))) for i in 1:20]...),
    )
    @test Nonlinear.evaluate(v, model, expr) ≈ sum(sqrt(i) for i in 1:20)
    return
end

function test_NLPBlockData()
    model = Nonlinear.Model()
    x = MOI.VariableIndex(1)
    evaluator = Nonlinear.Evaluator(model, Nonlinear.ExprGraphOnly(), [x])
    block = MOI.NLPBlockData(evaluator)
    @test block.has_objective == false
    @test length(block.constraint_bounds) == 0
    return
end

function test_parse_atan2()
    model = Nonlinear.Model()
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    θ = π / 4
    Nonlinear.add_constraint(model, :(atan($x, $y)), MOI.LessThan(θ))
    evaluator = Nonlinear.Evaluator(model)
    MOI.initialize(evaluator, [:ExprGraph])
    @test MOI.constraint_expr(evaluator, 1) == :(atan(x[$x], x[$y]) <= $θ)
    return
end

function test_eval_atan2()
    r = Nonlinear.OperatorRegistry()
    x = [1.1, 2.2]
    @test Nonlinear.eval_multivariate_function(r, :atan, x) ≈ atan(x[1], x[2])
    g = zeros(2)
    Nonlinear.eval_multivariate_gradient(r, :atan, g, x)
    @test g[1] ≈ x[2] / (x[1]^2 + x[2]^2)
    @test g[2] ≈ -x[1] / (x[1]^2 + x[2]^2)
    H = LinearAlgebra.LowerTriangular(zeros(2, 2))
    @test Nonlinear.eval_multivariate_hessian(r, :atan, H, x)
    @test H[1, 1] ≈ -2 * x[2] * x[1] / (x[1]^2 + x[2]^2)^2
    @test H[2, 1] ≈ (x[1]^2 - x[2]^2) / (x[1]^2 + x[2]^2)^2
    @test H[2, 2] ≈ 2 * x[2] * x[1] / (x[1]^2 + x[2]^2)^2
    return
end

function test_deep_recursion()
    model = Nonlinear.Model()
    x = MOI.VariableIndex(1)
    y = Expr(:call, :sin, x)
    for _ in 1:20_000
        y = Expr(:call, :^, Expr(:call, :sqrt, y), 2)
    end
    start = time()
    @test Nonlinear.parse_expression(model, y) isa Nonlinear.Expression
    # A conservative bound to check we're not doing something expensive.
    @test time() - start < 1.0
    return
end

function test_min_operator()
    model = Nonlinear.Model()
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    Nonlinear.set_objective(model, :(min($x, $y)))
    evaluator = Nonlinear.Evaluator(model)
    MOI.initialize(evaluator, [:ExprGraph])
    @test MOI.objective_expr(evaluator) == :(min(x[$x], x[$y]))

    r = Nonlinear.OperatorRegistry()
    x = [1.1, 2.2]
    @test Nonlinear.eval_multivariate_function(r, :min, x) == 1.1
    g = zeros(2)
    Nonlinear.eval_multivariate_gradient(r, :min, g, x)
    @test g == [1.0, 0.0]
    H = LinearAlgebra.LowerTriangular(zeros(2, 2))
    @test Nonlinear.eval_multivariate_hessian(r, :min, H, x)
    @test H[1, 1] == 1.0
    @test H[2, 1] == H[2, 2] == 0.0

    x = [1.1, -2.2]
    @test Nonlinear.eval_multivariate_function(r, :min, x) == -2.2
    g = zeros(2)
    Nonlinear.eval_multivariate_gradient(r, :min, g, x)
    @test g == [0.0, 1.0]
    H = LinearAlgebra.LowerTriangular(zeros(2, 2))
    @test Nonlinear.eval_multivariate_hessian(r, :min, H, x)
    @test H[2, 2] == 1.0
    @test H[1, 1] == H[2, 1] == 0.0
    return
end

function test_max_operator()
    model = Nonlinear.Model()
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    Nonlinear.set_objective(model, :(max($x, $y)))
    evaluator = Nonlinear.Evaluator(model)
    MOI.initialize(evaluator, [:ExprGraph])
    @test MOI.objective_expr(evaluator) == :(max(x[$x], x[$y]))

    r = Nonlinear.OperatorRegistry()
    x = [1.1, -2.2]
    @test Nonlinear.eval_multivariate_function(r, :max, x) == 1.1
    g = zeros(2)
    Nonlinear.eval_multivariate_gradient(r, :max, g, x)
    @test g == [1.0, 0.0]
    H = LinearAlgebra.LowerTriangular(zeros(2, 2))
    @test Nonlinear.eval_multivariate_hessian(r, :max, H, x)
    @test H[1, 1] == 1.0
    @test H[2, 1] == H[2, 2] == 0.0

    x = [1.1, 2.2]
    @test Nonlinear.eval_multivariate_function(r, :max, x) == 2.2
    g = zeros(2)
    Nonlinear.eval_multivariate_gradient(r, :max, g, x)
    @test g == [0.0, 1.0]
    H = LinearAlgebra.LowerTriangular(zeros(2, 2))
    @test Nonlinear.eval_multivariate_hessian(r, :max, H, x)
    @test H[2, 2] == 1.0
    @test H[1, 1] == H[2, 1] == 0.0
    return
end

function test_pow_complex_result()
    r = Nonlinear.OperatorRegistry()
    x = [-1.0, 1.5]
    @test isnan(Nonlinear.eval_multivariate_function(r, :^, x))
    g = zeros(2)
    Nonlinear.eval_multivariate_gradient(r, :^, g, x)
    @test all(isnan, g)
    H = LinearAlgebra.LowerTriangular(zeros(2, 2))
    Nonlinear.eval_multivariate_hessian(r, :^, H, x)
    @test all(iszero, H)
    @test Nonlinear.eval_multivariate_function(r, :^, Int[-2, 3]) == -8
    return
end

struct _NoReverse{T} <: AbstractArray{T,1}
    data::Vector{T}
end

Base.eachindex(x::_NoReverse) = eachindex(x.data)
Base.getindex(x::_NoReverse, args...) = getindex(x.data, args...)

function test_parse_splat_no_reverse()
    model = Nonlinear.Model()
    x = MOI.VariableIndex.(1:2)
    y = _NoReverse(x)
    expr = Nonlinear.parse_expression(model, :(+($y...)))
    @test expr == Nonlinear.parse_expression(model, :(+($(x[1]), $(x[2]))))
    return
end

function test_scalar_nonlinear_function_parse_expression()
    model = MOI.Utilities.Model{Float64}()
    x = MOI.add_variable(model)
    f = MOI.ScalarNonlinearFunction(
        :+,
        Any[x, MOI.ScalarNonlinearFunction(:sin, Any[x])],
    )
    nlp_model = MOI.Nonlinear.Model()
    e1 = MOI.Nonlinear.add_expression(nlp_model, f)
    e2 = MOI.Nonlinear.add_expression(nlp_model, :($x + sin($x)))
    @test nlp_model[e1] == nlp_model[e2]
    return
end

function test_scalar_nonlinear_function_parse_scalaraffinefunction()
    model = MOI.Utilities.Model{Float64}()
    x = MOI.add_variable(model)
    y = MOI.add_variable(model)
    terms = MOI.ScalarAffineTerm{Float64}[]
    for (f, expr) in (
        MOI.ScalarAffineFunction(terms, 0.0) => :(0.0),
        MOI.ScalarAffineFunction(terms, 1.0) => :(1.0),
        (1.0 * x + 2.0) => :($x + 2.0),
        (2.0 * x + 2.0) => :(2.0 * $x + 2.0),
        (2.0 * x + -3.0 * y) => :(2.0 * $x + -3.0 * $y),
        (2.0 * x) => :(2.0 * $x),
        (1.0 * x) => :($x),
    )
        nlp_model = MOI.Nonlinear.Model()
        f1 = MOI.Nonlinear.add_expression(nlp_model, f)
        f2 = MOI.Nonlinear.add_expression(nlp_model, expr)
        @test nlp_model[f1] == nlp_model[f2]
    end
    return
end

function test_scalar_nonlinear_function_parse_scalarquadraticfunction()
    model = MOI.Utilities.Model{Float64}()
    x = MOI.add_variable(model)
    y = MOI.add_variable(model)
    terms = MOI.ScalarAffineTerm{Float64}[]
    qterms = MOI.ScalarQuadraticTerm{Float64}[]
    aterm = MOI.ScalarAffineTerm(2.0, x)
    for (f, expr) in (
        MOI.ScalarQuadraticFunction(qterms, terms, 0.0) => :(0.0),
        MOI.ScalarQuadraticFunction(qterms, terms, 1.0) => :(1.0),
        MOI.ScalarQuadraticFunction(qterms, [aterm], 0.0) => :(2.0 * $x),
        (1.0 * x * x + 1.0 * x + 1.0) => :($x * $x + $x + 1),
        (1.0 * x * x + 1.0 * x) => :($x * $x + $x),
        (1.0 * x * x + 2.0 * x) => :($x * $x + 2.0 * $x),
        (2.0 * x * x + 2.0 * x) => :(2.0 * $x * $x + 2.0 * $x),
        (1.0 * x * x) => :($x * $x),
        (1.5 * x * x + 2.5 * x * y + 3.5 * x + 2.0) =>
            :(1.5 * $x * $x + 2.5 * $x * $y + 3.5 * $x + 2.0),
    )
        nlp_model = MOI.Nonlinear.Model()
        f1 = MOI.Nonlinear.add_expression(nlp_model, f)
        f2 = MOI.Nonlinear.add_expression(nlp_model, expr)
        @test nlp_model[f1] == nlp_model[f2]
    end
    return
end

function test_scalar_nonlinear_function_parse_logic_or()
    model = MOI.Utilities.Model{Float64}()
    x = MOI.add_variable(model)
    f = MOI.ScalarNonlinearFunction(:||, Any[x, x])
    nlp_model = MOI.Nonlinear.Model()
    e1 = MOI.Nonlinear.add_expression(nlp_model, f)
    e2 = MOI.Nonlinear.add_expression(nlp_model, :($x || $x))
    @test nlp_model[e1] == nlp_model[e2]
    return
end

function test_scalar_nonlinear_function_parse_logic_and()
    model = MOI.Utilities.Model{Float64}()
    x = MOI.add_variable(model)
    f = MOI.ScalarNonlinearFunction(:&&, Any[x, x])
    nlp_model = MOI.Nonlinear.Model()
    e1 = MOI.Nonlinear.add_expression(nlp_model, f)
    e2 = MOI.Nonlinear.add_expression(nlp_model, :($x && $x))
    @test nlp_model[e1] == nlp_model[e2]
    return
end

function test_scalar_nonlinear_function_parse_comparison()
    model = MOI.Utilities.Model{Float64}()
    x = MOI.add_variable(model)
    f = MOI.ScalarNonlinearFunction(:<, Any[x, 1])
    nlp_model = MOI.Nonlinear.Model()
    e1 = MOI.Nonlinear.add_expression(nlp_model, f)
    e2 = MOI.Nonlinear.add_expression(nlp_model, :($x < 1))
    @test nlp_model[e1] == nlp_model[e2]
    return
end

function test_scalar_nonlinear_function_parse_unknown()
    model = MOI.Utilities.Model{Float64}()
    x = MOI.add_variable(model)
    f = MOI.ScalarNonlinearFunction(:foo, Any[x, 1])
    nlp_model = MOI.Nonlinear.Model()
    @test_throws(
        MOI.UnsupportedNonlinearOperator(:foo),
        MOI.Nonlinear.add_expression(nlp_model, f),
    )
    return
end

function test_ListOfSupportedNonlinearOperators()
    model = MOI.Nonlinear.Model()
    ops = MOI.get(model, MOI.ListOfSupportedNonlinearOperators())
    @test ops isa Vector{Symbol}
    @test length(ops) > 70
    @test :|| in ops
    @test :sin in ops
    @test :> in ops
    @test :ifelse in ops
    return
end

function test_parse_univariate_splatting()
    model = MOI.Nonlinear.Model()
    MOI.Nonlinear.register_operator(model, :f, 1, x -> 2x)
    x = [MOI.VariableIndex(1)]
    @test MOI.Nonlinear.parse_expression(model, :(f($x...))) ==
          MOI.Nonlinear.parse_expression(model, :(f($(x[1]))))
    return
end

function test_parse_unsupported_operator()
    model = MOI.Nonlinear.Model()
    x = [MOI.VariableIndex(1)]
    @test_throws(
        MOI.UnsupportedNonlinearOperator(:f),
        MOI.Nonlinear.parse_expression(model, :(f($x...))),
    )
    return
end

function test_is_empty()
    model = MOI.Nonlinear.Model()
    @test MOI.is_empty(model)
    x = MOI.VariableIndex(1)
    Nonlinear.set_objective(model, :(log($x)))
    @test !MOI.is_empty(model)
    MOI.empty!(model)
    @test MOI.is_empty(model)
    Nonlinear.add_constraint(model, :(log($x)), MOI.GreaterThan(1.0))
    @test !MOI.is_empty(model)
    MOI.empty!(model)
    @test MOI.is_empty(model)
    Nonlinear.add_expression(model, :(sin($x)^2))
    @test !MOI.is_empty(model)
    MOI.empty!(model)
    @test MOI.is_empty(model)
    Nonlinear.add_parameter(model, 1.2)
    @test !MOI.is_empty(model)
    MOI.empty!(model)
    @test MOI.is_empty(model)
    f(x) = log(x + 1)
    Nonlinear.register_operator(model, :f, 1, f)
    @test !MOI.is_empty(model)
    MOI.empty!(model)
    @test MOI.is_empty(model)
    return
end

function test_unsupported_features_expr_graph_only()
    evaluator = Nonlinear.Evaluator(
        Nonlinear.Model(),
        Nonlinear.ExprGraphOnly(),
        MOI.VariableIndex[],
    )
    @test_throws(
        ErrorException(
            "Unable to initialize `Nonlinear.Evaluator` because the " *
            "following features are not supported: $([:Grad])",
        ),
        MOI.initialize(evaluator, [:ExprGraph, :Grad]),
    )
    return
end

function test_automatic_differentiation_backend()
    inner = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    model = MOI.Utilities.CachingOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
        MOI.Utilities.MockOptimizer(inner),
    )
    MOI.set(
        model,
        MOI.AutomaticDifferentiationBackend(),
        MOI.Nonlinear.ExprGraphOnly(),
    )
    @test MOI.get(inner, MOI.AutomaticDifferentiationBackend()) ==
          MOI.Nonlinear.ExprGraphOnly()
    return
end

function test_univariate_sign()
    f(y, p) = sign(y) * abs(y)^p
    ∇f(y, p) = p * abs(y)^(p - 1)
    ∇²f(y, p) = sign(y) * p * (p - 1) * abs(y)^(p - 2)
    for p in (-0.5, 0.5, 2.0)
        x = MOI.VariableIndex(1)
        model = MOI.Nonlinear.Model()
        MOI.Nonlinear.set_objective(model, :(sign($x) * abs($x)^$p))
        evaluator = MOI.Nonlinear.Evaluator(
            model,
            MOI.Nonlinear.SparseReverseMode(),
            [x],
        )
        MOI.initialize(evaluator, [:Grad, :Hess])
        for y in (-10.0, -1.2, 1.2, 10.0)
            @test MOI.eval_objective(evaluator, [y]) ≈ f(y, p)
            g = [NaN]
            MOI.eval_objective_gradient(evaluator, g, [y])
            @test g[1] ≈ ∇f(y, p)
            H = zeros(length(MOI.hessian_objective_structure(evaluator)))
            MOI.eval_hessian_objective(evaluator, H, [y])
            @test H[1] ≈ ∇²f(y, p)
        end
    end
end

function test_show_Model()
    model = MOI.Nonlinear.Model()
    @test sprint(show, model) ==
          "A Nonlinear.Model with:\n 0 objectives\n 0 parameters\n 0 expressions\n 0 constraints"
    p = MOI.Nonlinear.add_parameter(model, 2.0)
    @test sprint(show, model) ==
          "A Nonlinear.Model with:\n 0 objectives\n 1 parameter\n 0 expressions\n 0 constraints"
    return
end

function test_set_objective_nothing()
    model = MOI.Nonlinear.Model()
    x = MOI.VariableIndex(1)
    MOI.Nonlinear.set_objective(model, :(sin($x)))
    @test sprint(show, model) ==
          "A Nonlinear.Model with:\n 1 objective\n 0 parameters\n 0 expressions\n 0 constraints"
    MOI.Nonlinear.set_objective(model, nothing)
    @test sprint(show, model) ==
          "A Nonlinear.Model with:\n 0 objectives\n 0 parameters\n 0 expressions\n 0 constraints"
    return
end

function test_copy_evaluator()
    model = Nonlinear.Model()
    x = MOI.VariableIndex(1)
    evaluator = Nonlinear.Evaluator(model, Nonlinear.ExprGraphOnly(), [x])
    @test_throws(
        ErrorException("Copying nonlinear problems not yet implemented"),
        copy(evaluator),
    )
    return
end

function test_no_objective()
    model = Nonlinear.Model()
    x = MOI.VariableIndex(1)
    evaluator = Nonlinear.Evaluator(model, Nonlinear.ExprGraphOnly(), [x])
    @test_throws(
        ErrorException(
            "Unable to query objective_expr because no nonlinear objective was set",
        ),
        MOI.objective_expr(evaluator),
    )
    return
end

function test_convert_to_expr()
    model = Nonlinear.Model()
    x = MOI.VariableIndex(1)
    expr = MOI.Nonlinear.add_expression(model, :(sin($x)))
    evaluator = Nonlinear.Evaluator(model, Nonlinear.ExprGraphOnly(), [x])
    @test MOI.Nonlinear.convert_to_expr(
        evaluator,
        model[expr];
        moi_output_format = true,
    ) == :(sin(x[$x]))
    @test MOI.Nonlinear.convert_to_expr(
        evaluator,
        model[expr];
        moi_output_format = false,
    ) == :(sin($x))
    return
end

function test_create_binary_switch()
    target = Expr(
        :if,
        Expr(:call, :(<=), :id, 2),
        Expr(
            :if,
            Expr(:call, :(==), :id, 1),
            :a,
            Expr(:if, Expr(:call, :(==), :id, 2), :b),
        ),
        Expr(
            :if,
            Expr(:call, :(==), :id, 3),
            :c,
            Expr(:if, Expr(:call, :(==), :id, 4), :d),
        ),
    )
    @test MOI.Nonlinear._create_binary_switch(1:4, [:a, :b, :c, :d]) == target
    # Just test that these functions don't error. We'll test their contents by
    # evaluating the actual fuctions that are `@eval`ed.
    MOI.Nonlinear._generate_eval_univariate()
    MOI.Nonlinear._generate_eval_univariate_2nd_deriv()
    return
end

function test_intercept_ForwardDiff_MethodError()
    r = Nonlinear.OperatorRegistry()
    f(x::Float64) = sin(x)^2
    g(x) = x > 1 ? f(x) : zero(x)
    Nonlinear.register_operator(r, :g, 1, g)
    @test Nonlinear.eval_univariate_function(r, :g, 0.0) == 0.0
    @test Nonlinear.eval_univariate_function(r, :g, 2.0) ≈ sin(2.0)^2
    @test Nonlinear.eval_univariate_gradient(r, :g, 0.0) == 0.0
    @test_throws ErrorException Nonlinear.eval_univariate_gradient(r, :g, 2.0)
    return
end

function test_extract_subexpression()
    model = Nonlinear.Model()
    x = MOI.VariableIndex(1)
    sub = MOI.ScalarNonlinearFunction(:^, Any[x, 3])
    f = MOI.ScalarNonlinearFunction(:+, Any[sub, sub])
    expr = Nonlinear.parse_expression(model, f)
    display(expr.nodes)
    @test expr == Nonlinear.Expression(
        [
            Nonlinear.Node(Nonlinear.NODE_CALL_MULTIVARIATE, 1, -1),
            Nonlinear.Node(Nonlinear.NODE_SUBEXPRESSION, 1, 1),
            Nonlinear.Node(Nonlinear.NODE_SUBEXPRESSION, 1, 1),
        ],
        Float64[],
    )
    expected_sub = Nonlinear.Expression(
        [
            Nonlinear.Node(Nonlinear.NODE_CALL_MULTIVARIATE, 4, -1)
            Nonlinear.Node(Nonlinear.NODE_MOI_VARIABLE, 1, 1)
            Nonlinear.Node(Nonlinear.NODE_VALUE, 1, 1)
        ],
        [3.0],
    )
    @test model.expressions == [expected_sub]
    @test model.cache[sub] == Nonlinear.ExpressionIndex(1)

    h = MOI.ScalarNonlinearFunction(:*, Any[2, sub, 1])
    g = MOI.ScalarNonlinearFunction(:+, Any[sub, h])
    expr = MOI.Nonlinear.parse_expression(model, g)
    expected_g = Nonlinear.Expression(
        [
            Nonlinear.Node(Nonlinear.NODE_CALL_MULTIVARIATE, 1, -1)
            Nonlinear.Node(Nonlinear.NODE_SUBEXPRESSION, 1, 1)
            Nonlinear.Node(Nonlinear.NODE_CALL_MULTIVARIATE, 3, 1)
            Nonlinear.Node(Nonlinear.NODE_VALUE, 1, 3)
            Nonlinear.Node(Nonlinear.NODE_SUBEXPRESSION, 1, 3)
            Nonlinear.Node(Nonlinear.NODE_VALUE, 2, 3)
        ],
        [2.0, 1.0],
    )
    @test expr == expected_g
    # It should have detected the sub-expressions that was the same as `f`
    @test model.expressions == [expected_sub]
    # This means that it didn't get to extract from `g`, let's also test
    # with extraction by starting with an empty model

    model = Nonlinear.Model()
    MOI.Nonlinear.set_objective(model, g)
    @test model.objective == expected_g
    @test model.expressions == [expected_sub]
    # Test that the objective function gets rewritten as we reuse `h`
    # Also test that we don't change the parents in the stack of `h`
    # by creating a long stack
    prod = MOI.ScalarNonlinearFunction(:*, [h, x])
    sum = MOI.ScalarNonlinearFunction(:*, [x, x, x, x, prod])
    expr = Nonlinear.parse_expression(model, sum)
    @test isempty(model.objective.values)
    @test model.objective.nodes == [
        Nonlinear.Node(Nonlinear.NODE_CALL_MULTIVARIATE, 1, -1),
        Nonlinear.Node(Nonlinear.NODE_SUBEXPRESSION, 1, 1),
        Nonlinear.Node(Nonlinear.NODE_SUBEXPRESSION, 2, 1),
    ]
    @test model.expressions == [
        expected_sub,
        Nonlinear.Expression(
            [
                Nonlinear.Node(Nonlinear.NODE_CALL_MULTIVARIATE, 3, -1),
                Nonlinear.Node(Nonlinear.NODE_VALUE, 1, 1),
                Nonlinear.Node(Nonlinear.NODE_SUBEXPRESSION, 1, 1),
                Nonlinear.Node(Nonlinear.NODE_VALUE, 2, 1),
            ],
            [2.0, 1.0],
        ),
    ]
    @test isempty(expr.values)
    @test expr.nodes == [
        Nonlinear.Node(Nonlinear.NODE_CALL_MULTIVARIATE, 3, -1),
        Nonlinear.Node(Nonlinear.NODE_MOI_VARIABLE, 1, 1),
        Nonlinear.Node(Nonlinear.NODE_MOI_VARIABLE, 1, 1),
        Nonlinear.Node(Nonlinear.NODE_MOI_VARIABLE, 1, 1),
        Nonlinear.Node(Nonlinear.NODE_MOI_VARIABLE, 1, 1),
        Nonlinear.Node(Nonlinear.NODE_CALL_MULTIVARIATE, 3, 1),
        Nonlinear.Node(Nonlinear.NODE_SUBEXPRESSION, 2, 6),
        Nonlinear.Node(Nonlinear.NODE_MOI_VARIABLE, 1, 6),
    ]
    return
end

end  # TestNonlinear

TestNonlinear.runtests()
