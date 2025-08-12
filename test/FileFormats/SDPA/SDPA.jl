# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestSDPA

using Test

import MathOptInterface as MOI
import MathOptInterface.Utilities as MOIU
import MathOptInterface.FileFormats: SDPA

const SDPA_TEST_FILE = "test.sdpa"
const SDPA_MODELS_DIR = joinpath(@__DIR__, "models")

function runtests()
    for name in names(@__MODULE__, all = true)
        if startswith("$(name)", "test_")
            @testset "$name" begin
                getfield(@__MODULE__, name)()
            end
        end
    end
    sleep(1.0)  # Allow time for unlink to happen.
    rm(SDPA_TEST_FILE, force = true)
    return
end

function _set_var_and_con_names(model::MOI.ModelLike)
    variable_names = String[]
    for j in MOI.get(model, MOI.ListOfVariableIndices())
        var_name_j = "v" * string(j.value)
        push!(variable_names, var_name_j)
        MOI.set(model, MOI.VariableName(), j, var_name_j)
    end

    idx = 0
    constraint_names = String[]
    single_variable_constraints = Tuple[]
    for i in MOI.get(
        model,
        MOI.ListOfConstraintIndices{MOI.VariableIndex,MOI.Integer}(),
    )
        idx += 1
        x = MOI.get(model, MOI.VariableName(), MOI.VariableIndex(i.value))
        push!(single_variable_constraints, (x, MOI.Integer()))
    end
    for i in Iterators.flatten((
        MOI.get(
            model,
            MOI.ListOfConstraintIndices{
                MOI.VectorAffineFunction{Float64},
                MOI.Nonnegatives,
            }(),
        ),
        MOI.get(
            model,
            MOI.ListOfConstraintIndices{
                MOI.VectorAffineFunction{Float64},
                MOI.PositiveSemidefiniteConeTriangle,
            }(),
        ),
    ))
        idx += 1
        con_name_i = "c" * string(idx)
        push!(constraint_names, con_name_i)
        MOI.set(model, MOI.ConstraintName(), i, con_name_i)
    end

    return variable_names, constraint_names, single_variable_constraints
end

function _test_write_then_read(model_string::String)
    model1 = SDPA.Model()
    MOIU.loadfromstring!(model1, model_string)
    args = _set_var_and_con_names(model1)

    MOI.write_to_file(model1, SDPA_TEST_FILE)
    model2 = SDPA.Model()
    MOI.read_from_file(model2, SDPA_TEST_FILE)
    _set_var_and_con_names(model2)
    if MOI.get(model1, MOI.ObjectiveSense()) == MOI.MAX_SENSE
        MOI.set(model2, MOI.ObjectiveSense(), MOI.MAX_SENSE)
        attr = MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}()
        obj = MOI.get(model2, attr)
        MOI.set(model2, attr, MOIU.operate(-, Float64, obj))
    end

    return MOI.Test.util_test_models_equal(model1, model2, args...)
end

function _test_read(filename::String, model_string::String)
    model1 = MOI.FileFormats.Model(filename = filename)
    MOIU.loadfromstring!(model1, model_string)
    args = _set_var_and_con_names(model1)
    model2 = SDPA.Model()
    MOI.read_from_file(model2, filename)
    _set_var_and_con_names(model2)
    return MOI.Test.util_test_models_equal(model1, model2, args...)
end

function test_show()
    @test sprint(summary, SDPA.Model()) == "MOI.FileFormats.SDPA.Model"
    return
end

function test_support()
    for set in [
        MOI.EqualTo(1.0),
        MOI.LessThan(1.0),
        MOI.GreaterThan(1.0),
        MOI.Interval(1.0, 2.0),
        MOI.Semiinteger(1.0, 2.0),
        MOI.Semicontinuous(1.0, 2.0),
        MOI.ZeroOne(),
    ]
        model_string = """
        variables: x
        minobjective: 1x
        x in $set
        """
        model = SDPA.Model()
        @test !MOI.supports_constraint(model, MOI.VariableIndex, typeof(set))
        err = MOI.UnsupportedConstraint{MOI.VariableIndex,typeof(set)}
        @test_throws err MOIU.loadfromstring!(model, model_string)
    end
end

function test_delete()
    for T in [Int, Float64]
        model = SDPA.Model(; number_type = T)
        x = MOI.add_variable(model)
        MOI.delete(model, x)
        y = MOI.add_variable(model)
        MOI.add_constraint(
            model,
            MOIU.vectorize([one(T) * y]),
            MOI.Nonnegatives(1),
        )
        err = ErrorException(
            "Non-contiguous variable indices not supported. This might be due to deleted variables.",
        )
        @test_throws err MOI.write_to_file(model, SDPA_TEST_FILE)
    end
end

function test_objective()
    for T in [Int, Float64]
        model = SDPA.Model(; number_type = T)
        @test !MOI.supports(model, MOI.ObjectiveFunction{MOI.VariableIndex}())
        @test !MOI.supports(
            model,
            MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{T}}(),
        )
        model = SDPA.Model(; coefficient_type = T)
        @test !MOI.supports(model, MOI.ObjectiveFunction{MOI.VariableIndex}())
        @test !MOI.supports(
            model,
            MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{T}}(),
        )
    end
end

function test_nonempty()
    model = SDPA.Model()
    MOI.add_variable(model)
    err = ErrorException("Cannot read in file because model is not empty.")
    @test_throws err MOI.read_from_file(
        model,
        joinpath(SDPA_MODELS_DIR, "example_A.dat-s"),
    )
end

function test_bad_blocks()
    model = SDPA.Model()
    err = ErrorException(
        "The number of blocks (3) does not match the length of the list of blocks dimensions (2).",
    )
    @test_throws err MOI.read_from_file(
        model,
        joinpath(SDPA_MODELS_DIR, "bad_blocks.sdpa"),
    )
end

function test_bad_number_variables()
    model = SDPA.Model()
    err = ErrorException(
        "The number of variables (3) does not match the length of the list of coefficients for the objective function vector of coefficients (2).",
    )
    @test_throws err MOI.read_from_file(
        model,
        joinpath(SDPA_MODELS_DIR, "bad_vars.sdpa"),
    )
end

function test_wrong_number_of_values_in_entry()
    model = SDPA.Model()
    err = ErrorException(
        "Invalid line specifying entry: 0 1 2 2. There are 4 values instead of 5.",
    )
    @test_throws err MOI.read_from_file(
        model,
        joinpath(SDPA_MODELS_DIR, "bad_entry.sdpa"),
    )
end

function test_nondiagonal_entry()
    model = SDPA.Model()
    err = ErrorException(
        "Invalid line specifying entry: 0 1 1 2 1.0. `1 != 2` while block 1 has dimension 2 so it is a diagonal block.",
    )
    @test_throws err MOI.read_from_file(
        model,
        joinpath(SDPA_MODELS_DIR, "bad_diag.sdpa"),
    )
end

function test_nonzero_in_objective()
    model = SDPA.Model()
    MOIU.loadfromstring!(
        model,
        """
variables: x
minobjective: x + 1
""",
    )
    err = ErrorException(
        "Nonzero constant in objective function not supported. Note that " *
        "the constant may be added by the substitution of a bridged variable.",
    )
    @test_throws err MOI.write_to_file(model, SDPA_TEST_FILE)
end

function test_model_name()
    model = SDPA.Model()
    MOI.set(model, MOI.Name(), "FooBar")
    MOI.write_to_file(model, SDPA_TEST_FILE)
    @test readlines(SDPA_TEST_FILE) == ["\"FooBar", "0", "0", "", ""]
end

const _WRITE_READ_MODELS = [
    (
        "min ScalarAffine",
        """
    variables: x, y
    minobjective: 1.2x + -1y
""",
    ),
    (
        "max ScalarAffine",
        """
    variables: x, y
    maxobjective: 1.2x + -1y
""",
    ),
    (
        "VectorAffineFunction in Nonnegatives",
        """
    variables: x, y
    minobjective: 1.2x
    c1: [1.1 * x, y + 1] in Nonnegatives(2)
""",
    ),
    (
        "VectorAffineFunction in PositiveSemidefiniteConeTriangle",
        """
    variables: x, y, z
    minobjective: 1.2x
    c1: [1.1x, y + 1, 2x + z] in PositiveSemidefiniteConeTriangle(2)
""",
    ),
]

function test_write_read_models()
    for (model_name, model_string) in _WRITE_READ_MODELS
        _test_write_then_read(model_string)
    end
end

const _EXAMPLE_MODELS = [
    (
        "example_A.dat-s",
        """
    variables: x, y
    minobjective: 10x + 20y
    c1: [x + -1, 0, x + y + -2] in PositiveSemidefiniteConeTriangle(2)
    c2: [5y + -3, 2y, 6y + -4] in PositiveSemidefiniteConeTriangle(2)
""",
    ),
    (
        "example_B.sdpa",
        """
    variables: x
    minobjective: 1x
    c1: [0, 1x + -1, 0] in PositiveSemidefiniteConeTriangle(2)
""",
    ),
    (
        "example_integer.sdpa",
        """
    variables: x, y, z
    minobjective: 1x + -2y + -1z
    c1: [1x, 1y, 1z] in PositiveSemidefiniteConeTriangle(2)
    c2: [1z, 1x, 2.1] in PositiveSemidefiniteConeTriangle(2)
    c3: [1x + 1y + 1z + -1, -1x + -1y + -1z + 8] in Nonnegatives(2)
    x in Integer()
    y in Integer()
    z in Integer()
""",
    ),
    (
        "issue_1541.dat-s",
        """
    variables: x, y, z
    minobjective: -1y + -2z
    c1: [-1x + -1y + -1z + 1, 1x, 1y, 1z] in Nonnegatives(4)
    c2: [1x + 3y, 4y + 2z, -1z] in PositiveSemidefiniteConeTriangle(2)
""",
    ),
]

function test_examples()
    for (model_name, model_string) in _EXAMPLE_MODELS
        _test_read(joinpath(SDPA_MODELS_DIR, model_name), model_string)
        _test_write_then_read(model_string)
    end
end

# See https://github.com/jump-dev/MathOptInterface.jl/issues/1541
_spacer(char) = [" ", "$char", " $char", "$char ", " $char "]

function test_dim_reader()
    fn(line) = SDPA._dim_to_set.(SDPA._split(line))
    for (a, b) in ['{' => '}', '(' => ')']
        for pre in _spacer(a), sep in _spacer(','), suf in _spacer(b)
            @test fn("$(pre)-4$(sep)2$suf") ==
                  [MOI.Nonnegatives(4), MOI.PositiveSemidefiniteConeTriangle(2)]
            @test fn("$(pre)2$(sep)-4$suf") ==
                  [MOI.PositiveSemidefiniteConeTriangle(2), MOI.Nonnegatives(4)]
        end
    end
    return
end

function test_integer_before_variables()
    file = """
    *INTEGER
    *1
    """
    io = IOBuffer()
    print(io, file)
    seekstart(io)
    model = SDPA.Model()
    @test_throws(
        ErrorException(
            "The number of variables should be given before *INTEGER section.",
        ),
        read!(io, model),
    )
    return
end

function test_unsupported_variable_types()
    model = SDPA.Model()
    @test_throws(
        MOI.UnsupportedConstraint,
        MOI.add_constrained_variable(model, MOI.Parameter(2.0)),
    )
    @test_throws(
        MOI.UnsupportedConstraint,
        MOI.add_constrained_variable(model, MOI.Semicontinuous(2.0, 3.0)),
    )
    @test_throws(
        MOI.UnsupportedConstraint,
        MOI.add_constrained_variable(model, MOI.Semiinteger(2.0, 3.0)),
    )
    return
end

function test_example_A_integer_coefficients()
    filename = joinpath(SDPA_MODELS_DIR, "example_A_int.dat-s")
    model = MOI.FileFormats.SDPA.Model(; coefficient_type = Int)
    MOI.read_from_file(model, filename)
    for xi in MOI.get(model, MOI.ListOfVariableIndices())
        MOI.set(model, MOI.VariableName(), xi, "v$(xi.value)")
    end
    F, S = MOI.VectorAffineFunction{Int}, MOI.PositiveSemidefiniteConeTriangle
    for ci in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
        MOI.set(model, MOI.ConstraintName(), ci, "c$(ci.value)")
    end
    input = """
    variables: v1, v2
    minobjective::Int: 10v1 + 20v2
    c1::Int: [v1 + -1, 0, v1 + v2 + -2] in PositiveSemidefiniteConeTriangle(2)
    c2::Int: [5v2 + -3, 2v2, 6v2 + -4] in PositiveSemidefiniteConeTriangle(2)
    """
    target = MOI.Utilities.Model{Int}()
    MOI.Utilities.loadfromstring!(target, input)
    MOI.Test.util_test_models_equal(model, target, ["v1", "v2"], ["c1", "c2"])
    return
end

function test_unsupported_objectives()
    model = SDPA.Model()
    for (F, ret) in [
        MOI.VariableIndex => false,
        MOI.ScalarAffineFunction{Float64} => true,
        MOI.ScalarQuadraticFunction{Float64} => false,
        MOI.ScalarNonlinearFunction => false,
        MOI.VectorOfVariables => false,
        MOI.VectorAffineFunction{Float64} => false,
        MOI.VectorQuadraticFunction{Float64} => false,
        MOI.VectorNonlinearFunction => false,
    ]
        @test MOI.supports(model, MOI.ObjectiveFunction{F}()) == ret
    end
    return
end

end  # module

TestSDPA.runtests()
