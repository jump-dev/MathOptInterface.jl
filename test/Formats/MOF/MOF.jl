using MathOptInterface, Test

const MOI = MathOptInterface
const MOIU = MOI.Utilities
const MOF = MOI.Formats.MOF
const TEST_MOF_FILE = "test.mof.json"

@test sprint(show, MOF.Model()) == "A MathOptFormat Model"

function roundtrip_nonlinear_expression(expr, variable_to_string,
                                        string_to_variable)
    node_list = MOF.Object[]
    object = MOF.convert_expr_to_mof(expr, node_list,
                                               variable_to_string)
    @test MOF.convert_mof_to_expr(object, node_list,
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
        model = MOF.Model()
        x = MOI.add_variables(model, 4)
        for (index, variable) in enumerate(x)
            MOI.set(model, MOI.VariableName(), variable, "var_$(index)")
        end
        MOI.add_constraints(model, MOI.SingleVariable.(x),
                            Ref(MOI.Interval(1.0, 5.0)))
        MOI.set(model, MOI.NLPBlock(), HS071())
        MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
        MOI.write_to_file(model, TEST_MOF_FILE)
        @test replace(read(TEST_MOF_FILE, String), '\r' => "") ==
            replace(read(joinpath(@__DIR__, "nlp.mof.json"), String), '\r' => "")
        MOF.validate(TEST_MOF_FILE)
    end
    @testset "Error handling" begin
        node_list = MOF.Object[]
        string_to_variable = Dict{String, MOI.VariableIndex}()
        variable_to_string = Dict{MOI.VariableIndex, String}()
        # Test unsupported function for Expr -> MOF.
        @test_throws Exception MOF.convert_expr_to_mof(
            :(not_supported_function(x)), node_list, variable_to_string)
        # Test unsupported function for MOF -> Expr.
        @test_throws Exception MOF.convert_mof_to_expr(
            MOF.Object("head"=>"not_supported_function", "value"=>1),
            node_list, string_to_variable)
        # Test n-ary function with no arguments.
        @test_throws Exception MOF.convert_expr_to_mof(
            :(min()), node_list, variable_to_string)
        # Test unary function with two arguments.
        @test_throws Exception MOF.convert_expr_to_mof(
            :(sin(x, y)), node_list, variable_to_string)
        # Test binary function with one arguments.
        @test_throws Exception MOF.convert_expr_to_mof(
            :(^(x)), node_list, variable_to_string)
        # An expression with something other than :call as the head.
        @test_throws Exception MOF.convert_expr_to_mof(
            :(a <= b <= c), node_list, variable_to_string)
        # Hit the default fallback with an un-interpolated complex number.
        @test_throws Exception MOF.convert_expr_to_mof(
            :(1 + 2im), node_list, variable_to_string)
        # Invalid number of variables.
        @test_throws Exception MOF.substitute_variables(
            :(x[1] * x[2]), [MOI.VariableIndex(1)])
        # Function-in-Set
        @test_throws Exception MOF.extract_function_and_set(
            :(foo in set))
        # Not a constraint.
        @test_throws Exception MOF.extract_function_and_set(:(x^2))
        # Two-sided constraints
        @test MOF.extract_function_and_set(:(1 <= x <= 2)) ==
            MOF.extract_function_and_set(:(2 >= x >= 1)) ==
            (:x, MOI.Interval(1, 2))
        # Less-than constraint.
        @test MOF.extract_function_and_set(:(x <= 2)) ==
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
                     :(sin($(3im))^2 + cos($(3im))^2), :($(1 + 2im) * $x),
                     :(ceil($x)), :(floor($x)), :($x < $y), :($x <= $y),
                     :($x > $y), :($x >= $y), :($x == $y), :($x != $y),
                     # :($x && $y), :($x || $y),
                     :(ifelse($x > 0, 1, $y))]
            roundtrip_nonlinear_expression(expr, var_to_string, string_to_var)
        end
    end
    @testset "Reading and Writing" begin
        # Write to file.
        model = MOF.Model()
        (x, y) = MOI.add_variables(model, 2)
        MOI.set(model, MOI.VariableName(), x, "var_x")
        MOI.set(model, MOI.VariableName(), y, "y")
        con = MOI.add_constraint(model,
                 MOF.Nonlinear(:(2 * $x + sin($x)^2 - $y)),
                 MOI.EqualTo(1.0))
        MOI.set(model, MOI.ConstraintName(), con, "con")
        MOI.write_to_file(model, TEST_MOF_FILE)
        # Read the model back in.
        model2 = MOF.Model()
        MOI.read_from_file(model2, TEST_MOF_FILE)
        con2 = MOI.get(model2, MOI.ConstraintIndex, "con")
        foo2 = MOI.get(model2, MOI.ConstraintFunction(), con2)
        # Test that we recover the constraint.
        @test foo2.expr == :(2 * $x + sin($x)^2 - $y)
        @test MOI.get(model, MOI.ConstraintSet(), con) ==
                MOI.get(model2, MOI.ConstraintSet(), con2)
        MOF.validate(TEST_MOF_FILE)
    end
end

struct UnsupportedSet <: MOI.AbstractSet end
struct UnsupportedFunction <: MOI.AbstractFunction end

function test_model_equality(model_string, variables, constraints)
    model = MOF.Model()
    MOIU.loadfromstring!(model, model_string)
    MOI.write_to_file(model, TEST_MOF_FILE)
    model_2 = MOF.Model()
    MOI.read_from_file(model_2, TEST_MOF_FILE)
    MOIU.test_models_equal(model, model_2, variables, constraints)
    MOF.validate(TEST_MOF_FILE)
end

@testset "read_from_file" begin
    model = MOF.Model()
    model_zip = MOI.Formats.read_from_file(
        joinpath(@__DIR__, "empty_model.mof.json.gz"))
    MOIU.test_models_equal(model, model_zip, String[], String[])
    model_unzip = MOI.Formats.read_from_file(
        joinpath(@__DIR__, "empty_model.mof.json"))
    MOIU.test_models_equal(model, model_unzip, String[], String[])
end

@testset "Error handling: read_from_file" begin
    failing_models_dir = joinpath(@__DIR__, "failing_models")

    @testset "Non-empty model" begin
        model = MOF.Model(warn=true)
        MOI.add_variable(model)
        @test !MOI.is_empty(model)
        exception = ErrorException(
            "Cannot read model from file as destination model is not empty.")
        @test_throws exception MOI.read_from_file(
            model, joinpath(@__DIR__, "empty_model.mof.json"))
        options = MOF.get_options(model)
        @test options.warn
        MOI.empty!(model)
        @test MOI.is_empty(model)
        MOI.read_from_file(
            model, joinpath(@__DIR__, "empty_model.mof.json"))
        options2 = MOF.get_options(model)
        @test options2.warn
    end

    @testset "$(filename)" for filename in filter(
        f -> endswith(f, ".mof.json"), readdir(failing_models_dir))
        @test_throws Exception MOI.read_from_file(MOF.Model(),
            joinpath(failing_models_dir, filename))
    end
end
@testset "Names" begin
    @testset "Blank variable name" begin
        model = MOF.Model()
        variable_index = MOI.add_variable(model)
        @test_throws Exception MOF.moi_to_object(variable_index, model)
        MOI.Formats.create_unique_names(model, warn=true)
        @test MOF.moi_to_object(variable_index, model) ==
            MOF.Object("name" => "x1")
    end
    @testset "Duplicate variable name" begin
        model = MOF.Model()
        x = MOI.add_variable(model)
        MOI.set(model, MOI.VariableName(), x, "x")
        y = MOI.add_variable(model)
        MOI.set(model, MOI.VariableName(), y, "x")
        @test MOF.moi_to_object(x, model) == MOF.Object("name" => "x")
        @test MOF.moi_to_object(y, model) == MOF.Object("name" => "x")
        MOI.Formats.create_unique_names(model, warn=true)
        @test MOF.moi_to_object(x, model) == MOF.Object("name" => "x")
        @test MOF.moi_to_object(y, model) == MOF.Object("name" => "x_1")
    end
    @testset "Blank constraint name" begin
        model = MOF.Model()
        x = MOI.add_variable(model)
        MOI.set(model, MOI.VariableName(), x, "x")
        c = MOI.add_constraint(model, MOI.SingleVariable(x), MOI.ZeroOne())
        name_map = Dict(x => "x")
        MOI.Formats.create_unique_names(model, warn=true)
        @test MOF.moi_to_object(c, model, name_map)["name"] == "c1"
    end
    @testset "Duplicate constraint name" begin
        model = MOF.Model()
        x = MOI.add_variable(model)
        MOI.set(model, MOI.VariableName(), x, "x")
        c1 = MOI.add_constraint(model, MOI.SingleVariable(x), MOI.LessThan(1.0))
        c2 = MOI.add_constraint(model, MOI.SingleVariable(x), MOI.GreaterThan(0.0))
        MOI.set(model, MOI.ConstraintName(), c1, "c")
        MOI.set(model, MOI.ConstraintName(), c2, "c")
        name_map = Dict(x => "x")
        @test MOF.moi_to_object(c1, model, name_map)["name"] == "c"
        @test MOF.moi_to_object(c2, model, name_map)["name"] == "c"
        MOI.Formats.create_unique_names(model, warn=true)
        @test MOF.moi_to_object(c1, model, name_map)["name"] == "c_1"
        @test MOF.moi_to_object(c2, model, name_map)["name"] == "c"
    end
end
@testset "round trips" begin
    @testset "Empty model" begin
        model = MOF.Model()
        MOI.write_to_file(model, TEST_MOF_FILE)
        model_2 = MOF.Model()
        MOI.read_from_file(model_2, TEST_MOF_FILE)
        MOIU.test_models_equal(model, model_2, String[], String[])
    end
    @testset "FEASIBILITY_SENSE" begin
        model = MOF.Model(validate=false)
        x = MOI.add_variable(model)
        MOI.set(model, MOI.VariableName(), x, "x")
        MOI.set(model, MOI.ObjectiveSense(), MOI.FEASIBILITY_SENSE)
        MOI.set(model, MOI.ObjectiveFunction{MOI.SingleVariable}(),
            MOI.SingleVariable(x))
        MOI.write_to_file(model, TEST_MOF_FILE)
        model_2 = MOF.Model(validate=false)
        MOI.read_from_file(model_2, TEST_MOF_FILE)
        MOIU.test_models_equal(model, model_2, ["x"], String[])
    end
    @testset "Empty function term" begin
        model = MOF.Model()
        x = MOI.add_variable(model)
        MOI.set(model, MOI.VariableName(), x, "x")
        c = MOI.add_constraint(model,
            MOI.ScalarAffineFunction(MOI.ScalarAffineTerm{Float64}[], 0.0),
            MOI.GreaterThan(1.0)
        )
        MOI.set(model, MOI.ConstraintName(), c, "c")
        MOI.write_to_file(model, TEST_MOF_FILE)
        model_2 = MOF.Model()
        MOI.read_from_file(model_2, TEST_MOF_FILE)
        MOIU.test_models_equal(model, model_2, ["x"], ["c"])
    end
    @testset "min objective" begin
        test_model_equality("""
            variables: x
            minobjective: x
        """, ["x"], String[])
    end
    @testset "max objective" begin
        test_model_equality("""
            variables: x
            maxobjective: x
        """, ["x"], String[])
    end
    @testset "min scalaraffine" begin
        test_model_equality("""
            variables: x
            minobjective: 1.2x + 0.5
        """, ["x"], String[])
    end
    @testset "max scalaraffine" begin
        test_model_equality("""
            variables: x
            maxobjective: 1.2x + 0.5
        """, ["x"], String[])
    end
    @testset "singlevariable-in-lower" begin
        test_model_equality("""
            variables: x
            minobjective: 1.2x + 0.5
            c1: x >= 1.0
        """, ["x"], ["c1"])
    end
    @testset "singlevariable-in-upper" begin
        test_model_equality("""
            variables: x
            maxobjective: 1.2x + 0.5
            c1: x <= 1.0
        """, ["x"], ["c1"])
    end
    @testset "singlevariable-in-interval" begin
        test_model_equality("""
            variables: x
            minobjective: 1.2x + 0.5
            c1: x in Interval(1.0, 2.0)
        """, ["x"], ["c1"])
    end
    @testset "singlevariable-in-equalto" begin
        test_model_equality("""
            variables: x
            minobjective: 1.2x + 0.5
            c1: x == 1.0
        """, ["x"], ["c1"])
    end
    @testset "singlevariable-in-zeroone" begin
        test_model_equality("""
            variables: x
            minobjective: 1.2x + 0.5
            c1: x in ZeroOne()
        """, ["x"], ["c1"])
    end
    @testset "singlevariable-in-integer" begin
        test_model_equality("""
            variables: x
            minobjective: 1.2x + 0.5
            c1: x in Integer()
        """, ["x"], ["c1"])
    end
    @testset "singlevariable-in-Semicontinuous" begin
        test_model_equality("""
            variables: x
            minobjective: 1.2x + 0.5
            c1: x in Semicontinuous(1.0, 2.0)
        """, ["x"], ["c1"])
    end
    @testset "singlevariable-in-Semiinteger" begin
        test_model_equality("""
            variables: x
            minobjective: 1.2x + 0.5
            c1: x in Semiinteger(1.0, 2.0)
        """, ["x"], ["c1"])
    end
    @testset "scalarquadratic-objective" begin
        test_model_equality("""
            variables: x
            minobjective: 1.0*x*x + -2.0x + 1.0
        """, ["x"], String[])
    end
    @testset "SOS1" begin
        test_model_equality("""
            variables: x, y, z
            minobjective: x
            c1: [x, y, z] in SOS1([1.0, 2.0, 3.0])
        """, ["x", "y", "z"], ["c1"])
    end
    @testset "SOS2" begin
        test_model_equality("""
            variables: x, y, z
            minobjective: x
            c1: [x, y, z] in SOS2([1.0, 2.0, 3.0])
        """, ["x", "y", "z"], ["c1"])
    end
    @testset "Reals" begin
        test_model_equality("""
            variables: x, y, z
            minobjective: x
            c1: [x, y, z] in Reals(3)
        """, ["x", "y", "z"], ["c1"])
    end
    @testset "Zeros" begin
        test_model_equality("""
            variables: x, y, z
            minobjective: x
            c1: [x, y, z] in Zeros(3)
        """, ["x", "y", "z"], ["c1"])
    end
    @testset "Nonnegatives" begin
        test_model_equality("""
            variables: x, y, z
            minobjective: x
            c1: [x, y, z] in Nonnegatives(3)
        """, ["x", "y", "z"], ["c1"])
    end
    @testset "Nonpositives" begin
        test_model_equality("""
            variables: x, y, z
            minobjective: x
            c1: [x, y, z] in Nonpositives(3)
        """, ["x", "y", "z"], ["c1"])
    end
    @testset "PowerCone" begin
        test_model_equality("""
            variables: x, y, z
            minobjective: x
            c1: [x, y, z] in PowerCone(2.0)
        """, ["x", "y", "z"], ["c1"])
    end
    @testset "DualPowerCone" begin
        test_model_equality("""
            variables: x, y, z
            minobjective: x
            c1: [x, y, z] in DualPowerCone(0.5)
        """, ["x", "y", "z"], ["c1"])
    end
    @testset "GeometricMeanCone" begin
        test_model_equality("""
            variables: x, y, z
            minobjective: x
            c1: [x, y, z] in GeometricMeanCone(3)
        """, ["x", "y", "z"], ["c1"])
    end
    @testset "vectoraffine-in-zeros" begin
        test_model_equality("""
            variables: x, y
            minobjective: x
            c1: [1.0x + -3.0, 2.0y + -4.0] in Zeros(2)
        """, ["x", "y"], ["c1"])
    end
    @testset "vectorquadratic-in-nonnegatives" begin
        test_model_equality("""
            variables: x, y
            minobjective: x
            c1: [1.0*x*x + -2.0x + 1.0, 2.0y + -4.0] in Nonnegatives(2)
        """, ["x", "y"], ["c1"])
    end
    @testset "ExponentialCone" begin
        test_model_equality("""
            variables: x, y, z
            minobjective: x
            c1: [x, y, z] in ExponentialCone()
        """, ["x", "y", "z"], ["c1"])
    end
    @testset "DualExponentialCone" begin
        test_model_equality("""
            variables: x, y, z
            minobjective: x
            c1: [x, y, z] in DualExponentialCone()
        """, ["x", "y", "z"], ["c1"])
    end
    @testset "SecondOrderCone" begin
        test_model_equality("""
            variables: x, y, z
            minobjective: x
            c1: [x, y, z] in SecondOrderCone(3)
        """, ["x", "y", "z"], ["c1"])
    end
    @testset "RotatedSecondOrderCone" begin
        test_model_equality("""
            variables: x, y, z
            minobjective: x
            c1: [x, y, z] in RotatedSecondOrderCone(3)
        """, ["x", "y", "z"], ["c1"])
    end
    @testset "PositiveSemidefiniteConeTriangle" begin
        test_model_equality("""
            variables: x1, x2, x3
            minobjective: x1
            c1: [x1, x2, x3] in PositiveSemidefiniteConeTriangle(2)
        """, ["x1", "x2", "x3"], ["c1"])
    end
    @testset "PositiveSemidefiniteConeSquare" begin
        test_model_equality("""
            variables: x1, x2, x3, x4
            minobjective: x1
            c1: [x1, x2, x3, x4] in PositiveSemidefiniteConeSquare(2)
        """, ["x1", "x2", "x3", "x4"], ["c1"])
    end
    @testset "LogDetConeTriangle" begin
        test_model_equality("""
            variables: t, u, x1, x2, x3
            minobjective: x1
            c1: [t, u, x1, x2, x3] in LogDetConeTriangle(2)
        """, ["t", "u", "x1", "x2", "x3"], ["c1"])
    end
    @testset "LogDetConeSquare" begin
        test_model_equality("""
            variables: t, u, x1, x2, x3, x4
            minobjective: x1
            c1: [t, u, x1, x2, x3, x4] in LogDetConeSquare(2)
        """, ["t", "u", "x1", "x2", "x3", "x4"], ["c1"])
    end
    @testset "RootDetConeTriangle" begin
        test_model_equality("""
            variables: t, x1, x2, x3
            minobjective: x1
            c1: [t, x1, x2, x3] in RootDetConeTriangle(2)
        """, ["t", "x1", "x2", "x3"], ["c1"])
    end
    @testset "RootDetConeSquare" begin
        test_model_equality("""
            variables: t, x1, x2, x3, x4
            minobjective: x1
            c1: [t, x1, x2, x3, x4] in RootDetConeSquare(2)
        """, ["t", "x1", "x2", "x3", "x4"], ["c1"])
    end
    @testset "IndicatorSet" begin
        test_model_equality("""
            variables: x, y
            minobjective: x
            c1: [x, y] in IndicatorSet{ACTIVATE_ON_ONE}(GreaterThan(1.0))
            c2: x >= 0.0
        """, ["x", "y"], ["c1", "c2"])

        test_model_equality("""
            variables: x, y
            minobjective: x
            c1: [x, y] in IndicatorSet{ACTIVATE_ON_ZERO}(GreaterThan(1.0))
            c2: x >= 0.0
        """, ["x", "y"], ["c1", "c2"])
    end
    @testset "NormOneCone" begin
        test_model_equality("""
            variables: x, y
            minobjective: x
            c1: [x, y] in NormOneCone(2)
            c2: x >= 0.0
        """, ["x", "y"], ["c1", "c2"])
    end
    @testset "NormInfinityCone" begin
        test_model_equality("""
            variables: x, y
            minobjective: x
            c1: [x, y] in NormInfinityCone(2)
            c2: x >= 0.0
        """, ["x", "y"], ["c1", "c2"])
    end
    # Clean up
    sleep(1.0)  # allow time for unlink to happen
    rm(TEST_MOF_FILE, force=true)
end
