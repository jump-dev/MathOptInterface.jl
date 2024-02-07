# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestMPS

using Test

import MathOptInterface as MOI
import MathOptInterface.Utilities as MOIU

const MPS = MOI.FileFormats.MPS2
const MPS_OLD = MOI.FileFormats.MPS

function runtests()
    for name in names(@__MODULE__, all = true)
        if startswith("$(name)", "test_")
            @testset "$name" begin
                getfield(@__MODULE__, name)()
            end
        end
    end
    return
end

function _test_write_to_file(input::String, output::String)
    model = MPS.Model()
    MOI.Utilities.loadfromstring!(model, input)
    data = sprint(write, model)
    if data != output
        print(data)
    end
    @test data == output
    return
end

function _test_model_equality(
    model_string,
    variables,
    constraints,
    args...;
    kwargs...,
)
    model = MPS.Model(; kwargs...)
    MOI.Utilities.loadfromstring!(model, model_string)
    io = IOBuffer()
    write(io, model)
    seekstart(io)
    model_2 = MPS_OLD.Model()
    read!(io, model_2)
    return MOI.Test.util_test_models_equal(
        model,
        model_2,
        variables,
        constraints,
        args...,
    )
end

function test_basic_1()
    _test_write_to_file(
        """
        variables: a, b, c, d
        minobjective: 2.0 * a + 1.0
        a >= 0.0
        b <= 1.0
        c == 2.0
        b in ZeroOne()
        d in Integer()
        d in Interval(1.1, 2.2)
        c1: 1.0 * a >= 3.0
        c2: 1.0 * b <= 3.1
        c3: 1.0 * c == 3.3
        c4: 1.0 * a + -1.1 * d in Interval(3.0, 5.0)
        """,
        """
        NAME
        OBJSENSE MIN
        ROWS
         N  OBJ
         G  c1
         L  c2
         E  c3
         G  c4
        COLUMNS
            a         OBJ       2
            a         c1        1
            a         c4        1
            MARKER    'MARKER'                 'INTORG'
            b         c2        1
            MARKER    'MARKER'                 'INTEND'
            c         c3        1
            MARKER    'MARKER'                 'INTORG'
            d         c4        -1.1
            MARKER    'MARKER'                 'INTEND'
        RHS
            rhs       OBJ       -1
            rhs       c1        3
            rhs       c2        3.1
            rhs       c3        3.3
            rhs       c4        3
        RANGES
            rhs       c4        2
        BOUNDS
         LO bounds    a         0
         LO bounds    b         1
         FX bounds    c         2
         LO bounds    d         1.1
         UP bounds    d         2.2
        ENDATA
        """
    )
    return
end

function test_basic_2()
    _test_write_to_file(
        """
        variables: a_really_long_name
        maxobjective: -1.2 * a_really_long_name + -2.4
        """,
        """
        NAME
        OBJSENSE MAX
        ROWS
         N  OBJ
        COLUMNS
            a_really_long_name OBJ -1.2
        RHS
            rhs       OBJ       2.4
        BOUNDS
         FR bounds    a_really_long_name
        ENDATA
        """
    )
end

function test_min_objective()
    return _test_model_equality(
        """
    variables: x
    minobjective: 1.0 * x
""",
        ["x"],
        String[],
    )
end

function test_objconst()
    return _test_model_equality(
        """
    variables: x
    minobjective: 1.1 * x + 1.2
""",
        ["x"],
        String[],
    )
end

function test_default_rhs_greater()
    return _test_model_equality(
        """
variables: x
minobjective: 1.0 * x
c1: 2.0 * x >= 0.0
""",
        ["x"],
        ["c1"],
    )
end

function test_default_rhs_less()
    return _test_model_equality(
        """
    variables: x
    minobjective: 1.0 * x
    c1: 2.0 * x <= 0.0
""",
        ["x"],
        ["c1"],
    )
end

function test_default_rhs_equal()
    return _test_model_equality(
        """
variables: x
minobjective: 1.0 * x
c1: 2.0 * x == 0.0
""",
        ["x"],
        ["c1"],
    )
end

function test_min_scalaraffine()
    return _test_model_equality(
        """
variables: x
minobjective: 1.2x
""",
        ["x"],
        String[],
    )
end

function test_scalaraffine_greaterthan()
    return _test_model_equality(
        """
variables: x
minobjective: 1.2x
c1: 1.1 * x >= 2.0
""",
        ["x"],
        ["c1"],
    )
end

function test_scalaraffine_lessthan()
    return _test_model_equality(
        """
variables: x
minobjective: 1.2x
c1: 1.1 * x <= 2.0
""",
        ["x"],
        ["c1"],
    )
end

function test_scalaraffine_equalto()
    return _test_model_equality(
        """
variables: x
minobjective: 1.2x
c1: 1.1 * x == 2.0
""",
        ["x"],
        ["c1"],
    )
end

function test_scalaraffine_interval()
    return _test_model_equality(
        """
variables: x
minobjective: 1.2x
c1: 1.1 * x in Interval(1.0, 2.0)
""",
        ["x"],
        ["c1"],
    )
end

function test_objsense_max()
    _test_model_equality(
        """
variables: x
maxobjective: 1.2x
c1: 1.0 * x >= 0.0
""",
        ["x"],
        ["c1"],
        # print_objsense = true,
    )
    _test_model_equality(
        """
variables: x
minobjective: 1.2x
c1: 1.0 * x >= 0.0
""",
        ["x"],
        ["c1"],
        # print_objsense = true,
    )
    return
end

end

TestMPS.runtests()
