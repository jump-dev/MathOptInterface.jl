import MathOptInterface
using Test

const MOI = MathOptInterface
const MOIU = MOI.Utilities
const SDPA = MOI.FileFormats.SDPA
const SDPA_TEST_FILE = "test.sdpa"
const SDPA_MODELS_DIR = joinpath(@__DIR__, "models")

function set_var_and_con_names(model::MOI.ModelLike)
    variable_names = String[]
    for j in MOI.get(model, MOI.ListOfVariableIndices())
        var_name_j = "v" * string(j.value)
        push!(variable_names, var_name_j)
        MOI.set(model, MOI.VariableName(), j, var_name_j)
    end

    idx = 0
    constraint_names = String[]
    for i in Iterators.flatten((
        MOI.get(model, MOI.ListOfConstraintIndices{MOI.SingleVariable, MOI.Integer}()),
        MOI.get(model, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives}()),
        MOI.get(model, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.PositiveSemidefiniteConeTriangle}())))
        idx += 1
        con_name_i = "c" * string(idx)
        push!(constraint_names, con_name_i)
        MOI.set(model, MOI.ConstraintName(), i, con_name_i)
    end

    return (variable_names, constraint_names)
end

function test_write_then_read(model_string::String)
    model1 = SDPA.Model()
    MOIU.loadfromstring!(model1, model_string)
    (variable_names, constraint_names) = set_var_and_con_names(model1)

    MOI.write_to_file(model1, SDPA_TEST_FILE)
    model2 = SDPA.Model()
    MOI.read_from_file(model2, SDPA_TEST_FILE)
    set_var_and_con_names(model2)
    if MOI.get(model1, MOI.ObjectiveSense()) == MOI.MAX_SENSE
        MOI.set(model2, MOI.ObjectiveSense(), MOI.MAX_SENSE)
        attr = MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}()
        obj = MOI.get(model2, attr)
        MOI.set(model2, attr, MOIU.operate(-, Float64, obj))
    end

    MOIU.test_models_equal(model1, model2, variable_names, constraint_names)
end

function test_read(filename::String, model_string::String)
    model1 = MOI.FileFormats.Model(filename = filename)
    MOIU.loadfromstring!(model1, model_string)
    (variable_names, constraint_names) = set_var_and_con_names(model1)

    model2 = SDPA.Model()
    MOI.read_from_file(model2, filename)
    set_var_and_con_names(model2)

    MOIU.test_models_equal(model1, model2, variable_names, constraint_names)
end

@test sprint(show, SDPA.Model()) == "A SemiDefinite Programming Algorithm Format (SDPA) model"

@testset "Support errors" begin
    @testset "$set variable bound" for set in [
            MOI.EqualTo(1.0),
            MOI.LessThan(1.0),
            MOI.GreaterThan(1.0),
            MOI.Interval(1.0, 2.0),
            MOI.Semiinteger(1.0, 2.0),
            MOI.Semicontinuous(1.0, 2.0),
            MOI.ZeroOne()
        ]
        model_string = """
        variables: x
        minobjective: 1x
        c: x in $set
        """
        model = SDPA.Model()
        @test !MOI.supports_constraint(model, MOI.SingleVariable, typeof(set))
        err = MOI.UnsupportedConstraint{MOI.SingleVariable, typeof(set)}
        @test_throws err MOIU.loadfromstring!(model, model_string)
    end
end

@testset "Deleted variables error with $T" for T in [Int, Float64]
    model = SDPA.Model(; number_type = T)
    x = MOI.add_variable(model)
    MOI.delete(model, x)
    y = MOI.add_variable(model)
    fy = MOI.SingleVariable(y)
    MOI.add_constraint(model, MOIU.vectorize([one(T) * fy]), MOI.Nonnegatives(1))
    err = ErrorException("Non-contiguous variable indices not supported. This might be due to deleted variables.")
    @test_throws err MOI.write_to_file(model, SDPA_TEST_FILE)
end

@testset "Objective function with $T" for T in [Int, Float64]
    model = SDPA.Model(; number_type = T)
    @test !MOI.supports(model, MOI.ObjectiveFunction{MOI.SingleVariable}())
    @test !MOI.supports(model, MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{T}}())
end

@testset "Read errors" begin
    @testset "Non-empty model" begin
        model = SDPA.Model()
        MOI.add_variable(model)
        err = ErrorException("Cannot read in file because model is not empty.")
        @test_throws err MOI.read_from_file(model,
            joinpath(SDPA_MODELS_DIR, "example_A.dat-s"))
    end

    @testset "Bad number of blocks" begin
        model = SDPA.Model()
        err = ErrorException("The number of blocks (3) does not match the length of the list of blocks dimensions (2).")
        @test_throws err MOI.read_from_file(model,
            joinpath(SDPA_MODELS_DIR, "bad_blocks.sdpa"))
    end

    @testset "Bad number of variables" begin
        model = SDPA.Model()
        err = ErrorException("The number of variables (3) does not match the length of the list of coefficients for the objective function vector of coefficients (2).")
        @test_throws err MOI.read_from_file(model,
            joinpath(SDPA_MODELS_DIR, "bad_vars.sdpa"))
    end

    @testset "Wrong number of values in entry" begin
        model = SDPA.Model()
        err = ErrorException("Invalid line specifying entry: 0 1 2 2. There are 4 values instead of 5.")
        @test_throws err MOI.read_from_file(model,
            joinpath(SDPA_MODELS_DIR, "bad_entry.sdpa"))
    end

    @testset "Non-diagonal entry in diagonal block" begin
        model = SDPA.Model()
        err = ErrorException("Invalid line specifying entry: 0 1 1 2 1.0. `1 != 2` while block 1 has dimension 2 so it is a diagonal block.")
        @test_throws err MOI.read_from_file(model,
            joinpath(SDPA_MODELS_DIR, "bad_diag.sdpa"))
    end
end

@testset "Write errors" begin
    @testset "Nonzero constant in objective" begin
        model = SDPA.Model()
        MOIU.loadfromstring!(model, """
            variables: x
            minobjective: x + 1
        """)
        err = ErrorException(
            "Nonzero constant in objective function not supported. Note that " *
            "the constant may be added by the substitution of a bridged variable."
        )
        @test_throws err MOI.write_to_file(model, SDPA_TEST_FILE)
    end

    # TODO NLP not supported test.
end

@testset "Model name" begin
    model = SDPA.Model()
    MOI.set(model, MOI.Name(), "FooBar")
    MOI.write_to_file(model, SDPA_TEST_FILE)
    @test readlines(SDPA_TEST_FILE) == ["\"FooBar", "0", "0", "", ""]
end

write_read_models = [
    ("min ScalarAffine", """
        variables: x, y
        minobjective: 1.2x + -1y
    """),
    ("max ScalarAffine", """
        variables: x, y
        maxobjective: 1.2x + -1y
    """),
    ("VectorAffineFunction in Nonnegatives", """
        variables: x, y
        minobjective: 1.2x
        c1: [1.1 * x, y + 1] in Nonnegatives(2)
    """),
    ("VectorAffineFunction in PositiveSemidefiniteConeTriangle", """
        variables: x, y, z
        minobjective: 1.2x
        c1: [1.1x, y + 1, 2x + z] in PositiveSemidefiniteConeTriangle(2)
    """),
]
@testset "Write/read $model_name" for (model_name, model_string) in
    write_read_models
    test_write_then_read(model_string)
end

example_models = [
    ("example_A.dat-s", """
        variables: x, y
        minobjective: 10x + 20y
        c1: [x + -1, 0, x + -2] in PositiveSemidefiniteConeTriangle(2)
        c2: [5y + -3, 2y, 6y + -4] in PositiveSemidefiniteConeTriangle(2)
    """),
    ("example_B.sdpa", """
        variables: x
        minobjective: 1x
        c1: [0, 1x + -1, 0] in PositiveSemidefiniteConeTriangle(2)
    """),
    ("example_integer.sdpa", """
        variables: x, y, z
        minobjective: 1x + -2y + -1z
        c1: [1x, 1y, 1z] in PositiveSemidefiniteConeTriangle(2)
        c2: [1z, 1x, 2.1] in PositiveSemidefiniteConeTriangle(2)
        c3: [1x + 1y + 1z + -1, -1x + -1y + -1z + 8] in Nonnegatives(2)
        c4: x in Integer()
        c5: y in Integer()
        c6: z in Integer()
    """),
]
@testset "Read and write/read $model_name" for (model_name, model_string) in example_models
    test_read(joinpath(SDPA_MODELS_DIR, model_name), model_string)
    test_write_then_read(model_string)
end

# Clean up.
sleep(1.0)  # Allow time for unlink to happen.
rm(SDPA_TEST_FILE, force = true)
