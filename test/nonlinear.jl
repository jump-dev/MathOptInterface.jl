function roundtrip_nonlinear_expression(expr, variable_to_string,
                                        string_to_variable)
    node_list = MathOptFormat.Object[]
    object = MathOptFormat.convert_expr_to_mof(expr, node_list,
                                               variable_to_string)
    @test MathOptFormat.convert_mof_to_expr(object, node_list,
                                            string_to_variable) == expr
end

# hs071
# min x1 * x4 * (x1 + x2 + x3) + x3
# st  x1 * x2 * x3 * x4 >= 25
#     x1^2 + x2^2 + x3^2 + x4^2 = 40
#     1 <= x1, x2, x3, x4 <= 5
struct ExprEvaluator <: MOI.AbstractNLPEvaluator
    objective::Expr
    constraints::Vector{Expr}
end
MOI.features_available(::ExprEvaluator) = [:ExprGraph]
MOI.initialize(::ExprEvaluator, features) = nothing
MOI.objective_expr(evaluator::ExprEvaluator) = evaluator.objective
MOI.constraint_expr(evaluator::ExprEvaluator, i::Int) = evaluator.constraints[i]

function HS071()
    return MOI.NLPBlockData(
        MOI.NLPBoundsPair.([25, 40], [Inf, 40]),
        ExprEvaluator(:(x[1] * x[4] * (x[1] + x[2] + x[3]) + x[3]),
                      [:(x[1] * x[2] * x[3] * x[4] >= 25),
                       :(x[1]^2 + x[2]^2 + x[3]^2 + x[4]^2 == 40)]),
        true)
end

@testset "Nonlinear functions" begin
    @testset "HS071 via MOI" begin
        model = MathOptFormat.Model()
        x = MOI.add_variables(model, 4)
        for (index, variable) in enumerate(x)
            MOI.set(model, MOI.VariableName(), variable, "var_$(index)")
        end
        MOI.add_constraints(model, MOI.SingleVariable.(x),
                            Ref(MOI.Interval(1.0, 5.0)))
        MOI.set(model, MOI.NLPBlock(), HS071())
        MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
        MOI.write_to_file(model, "test.mof.json")
        if VERSION >= v"0.7"
            @test replace(read("test.mof.json", String), '\r' => "") ==
                replace(read("passing_models/nlp.mof.json", String), '\r' => "")
        else
            @test replace(readstring("test.mof.json"), '\r', "") ==
                replace(readstring("passing_models/nlp.mof.json"), '\r', "")
        end
    end
    @testset "Error handling" begin
        node_list = MathOptFormat.Object[]
        string_to_variable = Dict{String, MOI.VariableIndex}()
        variable_to_string = Dict{MOI.VariableIndex, String}()
        # Test unsupported function for Expr -> MOF.
        @test_throws Exception MathOptFormat.convert_expr_to_mof(
            :(not_supported_function(x)), node_list, variable_to_string)
        # Test unsupported function for MOF -> Expr.
        @test_throws Exception MathOptFormat.convert_mof_to_expr(
            MathOptFormat.Object("head"=>"not_supported_function", "value"=>1),
            node_list, string_to_variable)
        # Test n-ary function with no arguments.
        @test_throws Exception MathOptFormat.convert_expr_to_mof(
            :(min()), node_list, variable_to_string)
        # Test unary function with two arguments.
        @test_throws Exception MathOptFormat.convert_expr_to_mof(
            :(sin(x, y)), node_list, variable_to_string)
        # Test binary function with one arguments.
        @test_throws Exception MathOptFormat.convert_expr_to_mof(
            :(^(x)), node_list, variable_to_string)
        # An expression with something other than :call as the head.
        @test_throws Exception MathOptFormat.convert_expr_to_mof(
            :(a <= b <= c), node_list, variable_to_string)
        # Hit the default fallback with an un-interpolated complex number.
        @test_throws Exception MathOptFormat.convert_expr_to_mof(
            :(1 + 2im), node_list, variable_to_string)
        # Invalid number of variables.
        @test_throws Exception MathOptFormat.substitute_variables(
            :(x[1] * x[2]), [MOI.VariableIndex(1)])
        # Function-in-Set
        @test_throws Exception MathOptFormat.extract_function_and_set(
            :(foo in set))
        # Not a constraint.
        @test_throws Exception MathOptFormat.extract_function_and_set(:(x^2))
        # Two-sided constraints
        @test MathOptFormat.extract_function_and_set(:(1 <= x <= 2)) ==
            MathOptFormat.extract_function_and_set(:(2 >= x >= 1)) ==
            (:x, MOI.Interval(1, 2))
        # Less-than constraint.
        @test MathOptFormat.extract_function_and_set(:(x <= 2)) ==
            (:x, MOI.LessThan(2))
    end
    @testset "Roundtrip nonlinear expressions" begin
        x = MOI.VariableIndex(123)
        y = MOI.VariableIndex(456)
        z = MOI.VariableIndex(789)
        string_to_var = Dict{String, MOI.VariableIndex}("x"=>x, "y"=>y, "z"=>z)
        var_to_string = Dict{MOI.VariableIndex, String}(x=>"x", y=>"y", z=>"z")
        for expr in [2, 2.34, 2 + 3im, x, :(1 + $x), :($x - 1),
                     :($x + $y), :($x + $y - $z), :(2 * $x), :($x * $y),
                     :($x / 2), :(2 / $x), :($x / $y), :($x / $y / $z), :(2^$x),
                     :($x^2), :($x^$y), :($x^(2 * $y + 1)), :(sin($x)),
                     :(sin($x + $y)), :(2 * $x + sin($x)^2 + $y),
                     :(sin($(3im))^2 + cos($(3im))^2), :($(1 + 2im) * $x)]
            roundtrip_nonlinear_expression(expr, var_to_string, string_to_var)
        end
    end
    @testset "Reading and Writing" begin
        # Write to file.
        model = MathOptFormat.Model()
        (x, y) = MOI.add_variables(model, 2)
        MOI.set(model, MOI.VariableName(), x, "var_x")
        MOI.set(model, MOI.VariableName(), y, "y")
        con = MOI.add_constraint(model,
                 MathOptFormat.Nonlinear(:(2 * $x + sin($x)^2 - $y)),
                 MOI.EqualTo(1.0))
        MOI.set(model, MOI.ConstraintName(), con, "con")
        MOI.write_to_file(model, "test.mof.json")
        # Read the model back in.
        model2 = MathOptFormat.Model()
        MOI.read_from_file(model2, "test.mof.json")
        con2 = MOI.get(model2, MOI.ConstraintIndex, "con")
        foo2 = MOI.get(model2, MOI.ConstraintFunction(), con2)
        # Test that we recover the constraint.
        @test foo2.expr == :(2 * $x + sin($x)^2 - $y)
        @test MOI.get(model, MOI.ConstraintSet(), con) ==
                MOI.get(model2, MOI.ConstraintSet(), con2)
    end
end
