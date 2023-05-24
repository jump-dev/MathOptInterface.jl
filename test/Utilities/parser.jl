# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestParser

using Test

import MathOptInterface as MOI
import MathOptInterface.Utilities as MOIU

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

function _struct_isequal(a::T, b::T) where {T}
    return all(f -> getfield(a, f) == getfield(b, f), fieldnames(T))
end

function test__parse_function()
    @test _struct_isequal(
        MOIU._parse_function(:x),
        MOIU._ParsedVariableIndex(:x),
    )
    @test _struct_isequal(
        MOIU._parse_function(:([x, y, z])),
        MOIU._ParsedVectorOfVariables([:x, :y, :z]),
    )

    @test _struct_isequal(
        MOIU._parse_function(:(x + y + 2.0)),
        MOIU._ParsedScalarAffineFunction(
            MOIU._ParsedScalarAffineTerm.([1.0, 1.0], [:x, :y]),
            2.0,
        ),
    )
    @test _struct_isequal(
        MOIU._parse_function(:(x + -3y + 2.0)),
        MOIU._ParsedScalarAffineFunction(
            MOIU._ParsedScalarAffineTerm.([1.0, -3.0], [:x, :y]),
            2.0,
        ),
    )
    @test _struct_isequal(
        MOIU._parse_function(:(2 * x * y + y + 1.0)),
        MOIU._ParsedScalarQuadraticFunction(
            MOIU._ParsedScalarQuadraticTerm.([2.0], [:x], [:y]),
            MOIU._ParsedScalarAffineTerm.([1.0], [:y]),
            1.0,
        ),
    )

    err = ErrorException(
        "Unsupported operator in `loadfromstring!`: `-`. " *
        "The parser is deliberately limited in the syntax it " *
        "accepts. Write `x - y` as `x + -1 * y`,  and `x^2` as " *
        "`x * x`.",
    )
    @test_throws err MOIU._parse_function(:(x - y))

    @test _struct_isequal(
        MOIU._parse_function(:([x, 2x + y + 5.0])),
        MOIU._ParsedVectorAffineFunction(
            MOIU._ParsedVectorAffineTerm.(
                Int64[1, 2, 2],
                MOIU._ParsedScalarAffineTerm.([1.0, 2.0, 1.0], [:x, :x, :y]),
            ),
            [0.0, 5.0],
        ),
    )
    @test _struct_isequal(
        MOIU._parse_function(:([x, 2x + y + 5.0, 1 * x * x])),
        MOIU._ParsedVectorQuadraticFunction(
            MOIU._ParsedVectorQuadraticTerm.(
                Int64[3],
                MOIU._ParsedScalarQuadraticTerm.([2.0], [:x], [:x]),
            ),
            MOIU._ParsedVectorAffineTerm.(
                Int64[1, 2, 2],
                MOIU._ParsedScalarAffineTerm.([1.0, 2.0, 1.0], [:x, :x, :y]),
            ),
            [0.0, 5.0, 0.0],
        ),
    )
    return
end

function test__separate_label()
    @test MOIU._separate_label(:(variables:x)) == (:variables, :x)
    @test MOIU._separate_label(:(variables:x, y)) == (:variables, :((x, y)))
    @test MOIU._separate_label(:(minobjective:x+y)) == (:minobjective, :(x + y))
    @test MOIU._separate_label(:(con1:2x <= 1)) == (:con1, :(2x <= 1))
    @test MOIU._separate_label(:(con1:[x, y] in S)) == (:con1, :([x, y] in S))
    return
end

function test_one_variable()
    s = """
    variables: x
    x >= 1.0
    """
    model = MOIU.Model{Float64}()
    x = MOI.add_variable(model)
    MOI.set(model, MOI.VariableName(), x, "x")
    bound = MOI.add_constraint(model, x, MOI.GreaterThan(1.0))

    model2 = MOIU.Model{Float64}()
    MOIU.loadfromstring!(model2, s)
    MOI.Test.util_test_models_equal(
        model,
        model2,
        ["x"],
        String[],
        [("x", MOI.GreaterThan{Float64}(1.0))],
    )
    return
end

function test_linear_constraints()
    s = """
    variables: x, y
    linear1: x + y >= 1.0
    linear2: x + y <= 1.0
    linear3: x + y == 1.0
    """
    model = MOIU.Model{Float64}()
    x = MOI.add_variable(model)
    y = MOI.add_variable(model)
    MOI.set(model, MOI.VariableName(), x, "x")
    MOI.set(model, MOI.VariableName(), y, "y")
    linear1 = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([1.0, 1.0], [x, y]),
            0.0,
        ),
        MOI.GreaterThan(1.0),
    )
    linear2 = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([1.0, 1.0], [x, y]),
            0.0,
        ),
        MOI.LessThan(1.0),
    )
    linear3 = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([1.0, 1.0], [x, y]),
            0.0,
        ),
        MOI.EqualTo(1.0),
    )
    MOI.set(model, MOI.ConstraintName(), linear1, "linear1")
    MOI.set(model, MOI.ConstraintName(), linear2, "linear2")
    MOI.set(model, MOI.ConstraintName(), linear3, "linear3")

    model2 = MOIU.Model{Float64}()
    MOIU.loadfromstring!(model2, s)
    MOI.Test.util_test_models_equal(
        model,
        model2,
        ["x", "y"],
        ["linear1", "linear2", "linear3"],
    )
    return
end

function test_minimization_linear_objective()
    s = """
    variables: x, y
    minobjective: x + -2y + 1.0
    """
    model = MOIU.Model{Float64}()
    x = MOI.add_variable(model)
    y = MOI.add_variable(model)
    MOI.set(model, MOI.VariableName(), x, "x")
    MOI.set(model, MOI.VariableName(), y, "y")
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([1.0, -2.0], [x, y]),
            1.0,
        ),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)

    model2 = MOIU.Model{Float64}()
    MOIU.loadfromstring!(model2, s)
    MOI.Test.util_test_models_equal(model, model2, ["x", "y"], String[])
    return
end

function test_maximization_linear_objective()
    s = """
    variables: x, y
    maxobjective: x + -2y + 1.0
    """
    model = MOIU.Model{Float64}()
    x = MOI.add_variable(model)
    y = MOI.add_variable(model)
    MOI.set(model, MOI.VariableName(), x, "x")
    MOI.set(model, MOI.VariableName(), y, "y")
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([1.0, -2.0], [x, y]),
            1.0,
        ),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)

    model2 = MOIU.Model{Float64}()
    MOIU.loadfromstring!(model2, s)
    MOI.Test.util_test_models_equal(model, model2, ["x", "y"], String[])
    return
end

function test_SecondOrderCone_constraints()
    s = """
    variables: x, y, z
    varsoc: [x,y,z] in SecondOrderCone(3)
    affsoc: [2x,y+1,-1*z] in SecondOrderCone(3)
    affsoc2: [1.0,2.0,3.0] in SecondOrderCone(3)
    """
    model = MOIU.Model{Float64}()
    x = MOI.add_variable(model)
    y = MOI.add_variable(model)
    z = MOI.add_variable(model)
    MOI.set(model, MOI.VariableName(), x, "x")
    MOI.set(model, MOI.VariableName(), y, "y")
    MOI.set(model, MOI.VariableName(), z, "z")
    varsoc = MOI.add_constraint(
        model,
        MOI.VectorOfVariables([x, y, z]),
        MOI.SecondOrderCone(3),
    )
    affsoc = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.(
                [1, 2, 3],
                MOI.ScalarAffineTerm.([2.0, 1.0, -1.0], [x, y, z]),
            ),
            [0.0, 1.0, 0.0],
        ),
        MOI.SecondOrderCone(3),
    )
    affsoc2 = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm{Float64}[],
            [1.0, 2.0, 3.0],
        ),
        MOI.SecondOrderCone(3),
    )
    MOI.set(model, MOI.ConstraintName(), varsoc, "varsoc")
    MOI.set(model, MOI.ConstraintName(), affsoc, "affsoc")
    MOI.set(model, MOI.ConstraintName(), affsoc2, "affsoc2")

    model2 = MOIU.Model{Float64}()
    MOIU.loadfromstring!(model2, s)
    MOI.Test.util_test_models_equal(
        model,
        model2,
        ["x", "y", "z"],
        ["varsoc", "affsoc", "affsoc2"],
    )
    return
end

function test_Invalid_variable_name()
    s = """
    variables: x
    y >= 1.0
    """
    model = MOIU.Model{Float64}()
    err = ErrorException("Invalid variable name y.")
    @test_throws err MOIU.loadfromstring!(model, s)
    return
end

function test_constrained_variables()
    model = MOI.Utilities.Model{Float64}()
    text = """
    constrainedvariable: [a, b, c] in Nonnegatives(3)
    constrainedvariable: d in LessThan(2.0)
    """
    MOI.Utilities.loadfromstring!(model, text)
    MOI.set(
        model,
        MOI.ConstraintName(),
        MOI.ConstraintIndex{MOI.VectorOfVariables,MOI.Nonnegatives}(1),
        "con1",
    )
    model2 = MOI.Utilities.Model{Float64}()
    text2 = """
    variables: a, b, c, d
    con1: [a, b, c] in Nonnegatives(3)
    d <= 2.0
    """
    MOI.Utilities.loadfromstring!(model2, text2)
    MOI.Test.util_test_models_equal(
        model,
        model2,
        ["a", "b", "c", "d"],
        ["con1"],
        [("d", MOI.LessThan{Float64}(2.0))],
    )
    return
end

function test_eltypes_int()
    model = MOI.Utilities.Model{Int}()
    text = """
    variables: x, y
    ::Int: x >= 1
    c::Int: 2 * x <= 3
    d::Int: [x, 4 * y] in Zeros(2)
    e::Int: [2 * x * x] in Nonnegatives(1)
    """
    MOI.Utilities.loadfromstring!(model, text)
    c = MOI.get(model, MOI.ConstraintIndex, "c")
    @test isa(
        MOI.get(model, MOI.ConstraintFunction(), c),
        MOI.ScalarAffineFunction{Int},
    )
    d = MOI.get(model, MOI.ConstraintIndex, "d")
    @test isa(
        MOI.get(model, MOI.ConstraintFunction(), d),
        MOI.VectorAffineFunction{Int},
    )
    e = MOI.get(model, MOI.ConstraintIndex, "e")
    @test isa(
        MOI.get(model, MOI.ConstraintFunction(), e),
        MOI.VectorQuadraticFunction{Int},
    )
    return
end

function test_eltypes_rational_int()
    model = MOI.Utilities.Model{Rational{Int}}()
    text = """
    variables: x, y
    c::Rational{Int}: 2 // 1 * x <= 1 // 3
    d::Rational{Int}: [x, 4 // 2 * y] in Zeros(2)
    e::Rational{Int}: [2 // 2 * x * x] in Nonnegatives(1)
    """
    MOI.Utilities.loadfromstring!(model, text)
    c = MOI.get(model, MOI.ConstraintIndex, "c")
    @test isa(
        MOI.get(model, MOI.ConstraintFunction(), c),
        MOI.ScalarAffineFunction{Rational{Int}},
    )
    d = MOI.get(model, MOI.ConstraintIndex, "d")
    @test isa(
        MOI.get(model, MOI.ConstraintFunction(), d),
        MOI.VectorAffineFunction{Rational{Int}},
    )
    e = MOI.get(model, MOI.ConstraintIndex, "e")
    @test isa(
        MOI.get(model, MOI.ConstraintFunction(), e),
        MOI.VectorQuadraticFunction{Rational{Int}},
    )
    return
end

function test_eltypes_complex_float64()
    # Work-around for https://github.com/jump-dev/MathOptInterface.jl/issues/1947
    model = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    text = """
    variables: x
    c::Complex{Float64}: (2.0 + 1im) * x == 0.0 + 0.0im
    """
    MOI.Utilities.loadfromstring!(model, text)
    x = MOI.get(model, MOI.VariableIndex, "x")
    c = MOI.get(model, MOI.ConstraintIndex, "c")
    f = MOI.get(model, MOI.ConstraintFunction(), c)
    @test f isa MOI.ScalarAffineFunction{Complex{Float64}}
    @test isapprox(
        MOI.ScalarAffineFunction(
            [MOI.ScalarAffineTerm(2.0 + 1.0im, x)],
            0.0 + 0.0im,
        ),
        f,
    )
    return
end

struct Set2175 <: MOI.AbstractScalarSet end

function test_parse_external_set_constraint()
    model = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    MOI.Utilities.loadfromstring!(
        model,
        "variables: x\nx in $(@__MODULE__).Set2175()",
    )
    constraints = MOI.get(model, MOI.ListOfConstraintTypesPresent())
    @test (MOI.VariableIndex, Set2175) in constraints
    return
end

function test_parse_external_set_constrained_variable()
    model = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    MOI.Utilities.loadfromstring!(
        model,
        "constrainedvariable: x in $(@__MODULE__).Set2175()",
    )
    constraints = MOI.get(model, MOI.ListOfConstraintTypesPresent())
    @test (MOI.VariableIndex, Set2175) in constraints
    return
end

end  # module

TestParser.runtests()
