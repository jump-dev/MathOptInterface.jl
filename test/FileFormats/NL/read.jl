# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestNonlinearRead

using Test
import MathOptInterface as MOI
const NL = MOI.FileFormats.NL

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

function test_next_int()
    model = NL._CacheModel()
    for i in (1, 9, 10, 11, 99)
        io = IOBuffer()
        print(io, " $i\n")
        seekstart(io)
        @test NL._next(Int, io, model) == i
        @test peek(io) == UInt8('\n')
    end
    return
end

function test_next_float()
    model = NL._CacheModel()
    for (s, x) in [
        ("-1", -1.0),
        ("1", 1.0),
        ("-2.1e+1", -21.0),
        ("2.1e+1", 21.0),
        ("-2.1e-01", -0.21),
        ("2.1e-01", 0.21),
        ("-2.1E-01", -0.21),
        ("2.1E-01", 0.21),
        ("-20.1e+0", -20.1),
        ("20.1e+00", 20.1),
        ("  20.1e+00", 20.1),
    ]
        io = IOBuffer()
        print(io, "$s\n")
        seekstart(io)
        @test NL._next(Float64, io, model) == x
        @test peek(io) == UInt8('\n')
    end
    return
end

function test_parse_expr()
    model = NL._CacheModel()
    io = IOBuffer()
    write(io, "o2\nv0\no2\nn2\no2\nv3\nv1\n")
    # (* x1 (* 2 (* x4 x2)))
    seekstart(io)
    x = MOI.VariableIndex.(1:4)
    @test NL._parse_expr(io, model) == :(*($(x[1]), *(2, *($(x[4]), $(x[2])))))
    @test eof(io)
    return
end

function test_parse_expr_nary()
    model = NL._CacheModel()
    io = IOBuffer()
    write(io, "o54\n4\no5\nv0\nn2\no5\nv2\nn2\no5\nv3\nn2\no5\nv1\nn2\n")
    seekstart(io)
    x = MOI.VariableIndex.(1:4)
    @test NL._parse_expr(io, model) ==
          :(+($(x[1])^2, $(x[3])^2, $(x[4])^2, $(x[2])^2))
    @test eof(io)
    return
end

function test_parse_expr_minimum()
    model = NL._CacheModel()
    io = IOBuffer()
    write(io, "o11\n3\nv0\nv1\nv2\n")
    seekstart(io)
    x = MOI.VariableIndex.(1:3)
    @test NL._parse_expr(io, model) == :(min($(x[1]), $(x[2]), $(x[3])))
    @test eof(io)
    return
end

function test_parse_expr_maximum()
    model = NL._CacheModel()
    io = IOBuffer()
    write(io, "o12\n3\nv0\nv1\nv2\n")
    seekstart(io)
    x = MOI.VariableIndex.(1:3)
    @test NL._parse_expr(io, model) == :(max($(x[1]), $(x[2]), $(x[3])))
    @test eof(io)
    return
end

function test_parse_header_binary()
    model = NL._CacheModel()
    NL._resize_variables(model, 4)
    io = IOBuffer()
    write(io, "b3 1 1 0\n")
    seekstart(io)
    @test_throws(
        ErrorException("Unable to parse NL file : unsupported mode b"),
        NL._parse_header(io, model),
    )
    return
end

function test_parse_expr_atan2()
    model = NL._CacheModel()
    io = IOBuffer()
    write(io, "o48\nv0\nv1\n")
    seekstart(io)
    x = MOI.VariableIndex.(1:2)
    @test NL._parse_expr(io, model) == :(atan($(x[1]), $(x[2])))
    @test eof(io)
    return
end

function test_parse_expr_atan()
    model = NL._CacheModel()
    io = IOBuffer()
    write(io, "o49\nv0\n")
    seekstart(io)
    x = MOI.VariableIndex.(1:1)
    @test NL._parse_expr(io, model) == :(atan($(x[1])))
    @test eof(io)
    return
end

function test_parse_header_assertion_errors()
    model = NL._CacheModel()
    for header in [
        "g4 1 1 0\n3 3 2 0 0 0\n",
        "g4 1 1 0\n3 3 1 0 0 0\n0 2\n",
        "g4 1 1 0\n3 3 1 0 0 0\n0 0\n1 0\n",
        "g4 1 1 0\n3 3 1 0 0 0\n0 0\n0 1\n",
        "g3 1 1 0\n4 2 1 0 1 0\n2 1\n0 0\n4 0 0\n1 0 0 1\n",
        "g3 1 1 0\n4 2 1 0 1 0\n2 1\n0 0\n4 0 0\n0 1 0 1\n",
        "g3 1 1 0\n4 2 1 0 1 0\n2 1\n0 0\n4 0 0\n0 0 1 1\n",
        "g3 1 1 0\n4 2 1 0 1 0\n2 1\n0 0\n4 0 0\n0 0 0 1\n0 0 0 2 0\n-1 0\n",
        "g3 1 1 0\n4 2 1 0 1 0\n2 1\n0 0\n4 0 0\n0 0 0 1\n0 0 0 2 0\n0 -1\n",
        "g3 1 1 0\n4 2 1 0 1 0\n2 1\n0 0\n4 0 0\n0 0 0 1\n0 0 0 2 0\n8 4\n1 0\n",
        "g3 1 1 0\n4 2 1 0 1 0\n2 1\n0 0\n4 0 0\n0 0 0 1\n0 0 0 2 0\n8 4\n0 1\n",
    ]
        io = IOBuffer()
        write(io, header)
        seekstart(io)
        @test_throws(AssertionError, NL._parse_header(io, model))
    end
    return
end

function test_parse_header_common_expressions()
    model = NL._CacheModel()
    err = ErrorException(
        "Unable to parse NL file : we don't support common exprs",
    )
    for header in [
        "g3 1 1 0\n4 2 1 0 1 0\n2 1\n0 0\n4 0 0\n0 0 0 1\n0 0 0 2 0\n8 4\n0 0\n1 0 0 0 0\n",
        "g3 1 1 0\n4 2 1 0 1 0\n2 1\n0 0\n4 0 0\n0 0 0 1\n0 0 0 2 0\n8 4\n0 0\n0 1 0 0 0\n",
        "g3 1 1 0\n4 2 1 0 1 0\n2 1\n0 0\n4 0 0\n0 0 0 1\n0 0 0 2 0\n8 4\n0 0\n0 0 1 0 0\n",
        "g3 1 1 0\n4 2 1 0 1 0\n2 1\n0 0\n4 0 0\n0 0 0 1\n0 0 0 2 0\n8 4\n0 0\n0 0 0 1 0\n",
        "g3 1 1 0\n4 2 1 0 1 0\n2 1\n0 0\n4 0 0\n0 0 0 1\n0 0 0 2 0\n8 4\n0 0\n0 0 0 0 1\n",
    ]
        io = IOBuffer()
        write(io, header)
        seekstart(io)
        @test_throws(err, NL._parse_header(io, model))
    end
    return
end

function test_parse_y_error()
    model = NL._CacheModel()
    NL._resize_variables(model, 4)
    io = IOBuffer()
    write(io, "y\n")
    seekstart(io)
    @test_throws(
        ErrorException("Unable to parse NL file: unhandled header y"),
        NL._parse_section(io, model),
    )
    return
end

function test_parse_O()
    model = NL._CacheModel()
    NL._resize_variables(model, 4)
    io = IOBuffer()
    write(
        io,
        """
O0 0# can stick a comment anywhere
o2
v0  # can stick a comment anywhere
o2
v2
o2
v3
v1
""",
    )
    seekstart(io)
    NL._parse_section(io, model)
    @test eof(io)
    @test model.sense == MOI.MIN_SENSE
    x = MOI.VariableIndex.(1:4)
    @test model.objective == :($(x[1]) * ($(x[3]) * ($(x[4]) * $(x[2]))))
    return
end

function test_parse_O_max()
    model = NL._CacheModel()
    NL._resize_variables(model, 4)
    io = IOBuffer()
    write(io, "O0 1\nv0\n")
    seekstart(io)
    NL._parse_section(io, model)
    @test eof(io)
    @test model.sense == MOI.MAX_SENSE
    x = MOI.VariableIndex(1)
    @test model.objective == :(+$x)
    return
end

function test_parse_x()
    model = NL._CacheModel()
    NL._resize_variables(model, 5)
    io = IOBuffer()
    write(
        io,
        """
x3
0 1.1# can stick a comment anywhere
3 2.2
2 3.3  # can stick a comment anywhere
""",
    )
    seekstart(io)
    NL._parse_section(io, model)
    @test eof(io)
    @test model.variable_primal == [1.1, 0.0, 3.3, 2.2, 0.0]
    return
end

function test_parse_d()
    model = NL._CacheModel()
    io = IOBuffer()
    write(
        io,
        """
d3
0 1.1# can stick a comment anywhere
3 2.2
2 3.3  # can stick a comment anywhere
""",
    )
    seekstart(io)
    NL._parse_section(io, model)
    @test eof(io)
    return
end

function test_parse_r()
    model = NL._CacheModel()
    NL._resize_constraints(model, 5)
    io = IOBuffer()
    write(
        io,
        """
r# can stick a comment anywhere
1 3.3
3
0 1.1 2.2  # can stick a comment anywhere
4 5.5
2 4.4# can stick a comment anywhere
""",
    )
    seekstart(io)
    NL._parse_section(io, model)
    @test eof(io)
    @test model.constraint_lower == [-Inf, -Inf, 1.1, 5.5, 4.4]
    @test model.constraint_upper == [3.3, Inf, 2.2, 5.5, Inf]
    return
end

function test_parse_b()
    model = NL._CacheModel()
    NL._resize_variables(model, 5)
    io = IOBuffer()
    write(
        io,
        """
b# can stick a comment anywhere
1 3.3
3# can stick a comment anywhere
0 1.1 2.2
4 5.5
2 4.4 # can stick a comment anywhere
""",
    )
    seekstart(io)
    NL._parse_section(io, model)
    @test eof(io)
    @test model.variable_lower == [-Inf, -Inf, 1.1, 5.5, 4.4]
    @test model.variable_upper == [3.3, Inf, 2.2, 5.5, Inf]
    return
end

function test_parse_k()
    model = NL._CacheModel()
    NL._resize_variables(model, 3)
    io = IOBuffer()
    write(
        io,
        """
k
2 # can stick a comment anywhere
4
""",
    )
    seekstart(io)
    NL._parse_section(io, model)
    @test eof(io)
    return
end

function test_parse_J()
    model = NL._CacheModel()
    NL._resize_constraints(model, 3)
    io = IOBuffer()
    write(
        io,
        """
J1 2  # can stick a comment anywhere
2 1.1
3 2.2  # can stick a comment anywhere
""",
    )
    seekstart(io)
    NL._parse_section(io, model)
    @test eof(io)
    @test model.constraints[1] == :()
    x, y = MOI.VariableIndex(3), MOI.VariableIndex(4)
    @test model.constraints[2] == :(1.1 * $x + 2.2 * $y)
    @test model.constraints[3] == :()
    return
end

function test_parse_J_zeros()
    model = NL._CacheModel()
    NL._resize_constraints(model, 1)
    io = IOBuffer()
    write(
        io,
        """
J0 2
0 0
1 0
""",
    )
    seekstart(io)
    NL._parse_section(io, model)
    @test eof(io)
    @test model.constraints[1] == :()
    return
end

function test_parse_C_J()
    model = NL._CacheModel()
    NL._resize_constraints(model, 1)
    io = IOBuffer()
    write(
        io,
        """
C0
o2
v0
v1
J0 2
0 1.1
1 2.2
""",
    )
    seekstart(io)
    NL._parse_section(io, model)
    NL._parse_section(io, model)
    @test eof(io)
    x, y = MOI.VariableIndex(1), MOI.VariableIndex(2)
    @test model.constraints[1] == :((1.1 * $x + 2.2 * $y) + $x * $y)
    return
end

function test_parse_J_C()
    model = NL._CacheModel()
    NL._resize_constraints(model, 1)
    io = IOBuffer()
    write(
        io,
        """
J0 2
0 1.1
1 2.2
C0
o2
v0
v1
""",
    )
    seekstart(io)
    NL._parse_section(io, model)
    NL._parse_section(io, model)
    @test eof(io)
    x, y = MOI.VariableIndex(1), MOI.VariableIndex(2)
    @test model.constraints[1] == :((1.1 * $x + 2.2 * $y) + $x * $y)
    return
end

function test_parse_G()
    model = NL._CacheModel()
    NL._resize_constraints(model, 3)
    io = IOBuffer()
    write(
        io,
        """
G0 2 # can stick a comment anywhere
2 1.1  # can stick a comment anywhere
3 2.2
""",
    )
    seekstart(io)
    NL._parse_section(io, model)
    @test eof(io)
    x, y = MOI.VariableIndex(3), MOI.VariableIndex(4)
    @test model.objective == :(1.1 * $x + 2.2 * $y)
    return
end

function test_parse_G_zeros()
    model = NL._CacheModel()
    NL._resize_constraints(model, 3)
    io = IOBuffer()
    write(
        io,
        """
G0 3
0 0
1 0
2 0
""",
    )
    seekstart(io)
    NL._parse_section(io, model)
    @test eof(io)
    @test model.objective == :()
    return
end

function test_parse_O_G()
    model = NL._CacheModel()
    NL._resize_variables(model, 4)
    io = IOBuffer()
    write(
        io,
        """
O0 0
o2
v0
v1
G0 2
0 1.1
1 2.2
""",
    )
    seekstart(io)
    NL._parse_section(io, model)
    NL._parse_section(io, model)
    @test eof(io)
    x, y = MOI.VariableIndex(1), MOI.VariableIndex(2)
    @test model.objective == :((1.1 * $x + 2.2 * $y) + $x * $y)
    return
end

function test_parse_G_O()
    model = NL._CacheModel()
    NL._resize_variables(model, 4)
    io = IOBuffer()
    write(
        io,
        """
G0 2
0 -1.1
1 2.2e+01
O0 0
o2
v0
v1
""",
    )
    seekstart(io)
    NL._parse_section(io, model)
    NL._parse_section(io, model)
    @test eof(io)
    x, y = MOI.VariableIndex(1), MOI.VariableIndex(2)
    @test model.objective == :((-1.1 * $x + 22.0 * $y) + $x * $y)
    return
end

function test_hs071()
    model = NL.Model()
    open(joinpath(@__DIR__, "data", "hs071.nl"), "r") do io
        return read!(io, model)
    end
    model_print = sprint(print, model)
    _in_ = @static Sys.iswindows() ? "in" : "∈"
    for line in [
        "(1.1 * v[1] + 1.4 * v[2] + 1.2 * v[3] + 1.3 * v[4]) + +2.0",
        "v[1] >= 1.1",
        "v[2] >= 1.4",
        "v[3] >= 1.2",
        "v[4] >= 1.3",
        "v[1] <= 5.1",
        "v[2] <= 5.4",
        "v[3] <= 1.0",
        "v[4] <= 5.3",
        "v[3] $_in_ ℤ",
        "v[4] $_in_ ℤ",
        "v[1] * (v[3] * (v[4] * v[2])) >= 25.0",
        "v[1] ^ 2.0 + v[3] ^ 2.0 + v[4] ^ 2.0 + v[2] ^ 2.0 == 40.0",
    ]
        @test occursin(line, model_print)
    end
    return
end

function test_parse_header_integrality_obj()
    model = NL._CacheModel()
    NL._resize_variables(model, 4)
    io = IOBuffer()
    write(
        io,
        """g3 0 1 0	# problem test_simple
2 1 1 0 0	# vars, constraints, objectives, ranges, eqns
1 1	# nonlinear constraints, objectives
0 0	# network constraints: nonlinear, linear
1 2 0	# nonlinear vars in constraints, objectives, both
0 0 0 1	# linear network variables; functions; arith, flags
0 0 0 0 0	# discrete variables: binary, integer, nonlinear (b,c,o)
1 1	# nonzeros in Jacobian, gradients
0 0	# max name lengths: constraints, variables
0 0 0 0 0	# common exprs: b,c,o,c1,o1\n""",
    )
    seekstart(io)
    NL._parse_header(io, model)
    @test model.variable_type[1] == NL._CONTINUOUS
    @test model.variable_type[2] == NL._CONTINUOUS
    return
end

function test_parse_header_integrality_obj_int()
    model = NL._CacheModel()
    NL._resize_variables(model, 4)
    io = IOBuffer()
    write(
        io,
        """g3 0 1 0	# problem test_simple
2 1 1 0 0	# vars, constraints, objectives, ranges, eqns
1 1	# nonlinear constraints, objectives
0 0	# network constraints: nonlinear, linear
1 2 0	# nonlinear vars in constraints, objectives, both
0 0 0 1	# linear network variables; functions; arith, flags
0 0 0 0 1	# discrete variables: binary, integer, nonlinear (b,c,o)
1 1	# nonzeros in Jacobian, gradients
0 0	# max name lengths: constraints, variables
0 0 0 0 0	# common exprs: b,c,o,c1,o1\n""",
    )
    seekstart(io)
    NL._parse_header(io, model)
    @test model.variable_type[1] == NL._CONTINUOUS
    @test model.variable_type[2] == NL._INTEGER
    return
end

function test_parse_header_integrality_both()
    model = NL._CacheModel()
    NL._resize_variables(model, 4)
    io = IOBuffer()
    write(
        io,
        """g3 0 1 0	# problem test_simple
2 1 1 0 0	# vars, constraints, objectives, ranges, eqns
1 1	# nonlinear constraints, objectives
0 0	# network constraints: nonlinear, linear
2 1 1	# nonlinear vars in constraints, objectives, both
0 0 0 1	# linear network variables; functions; arith, flags
0 0 0 0 0	# discrete variables: binary, integer, nonlinear (b,c,o)
1 1	# nonzeros in Jacobian, gradients
0 0	# max name lengths: constraints, variables
0 0 0 0 0	# common exprs: b,c,o,c1,o1\n""",
    )
    seekstart(io)
    NL._parse_header(io, model)
    @test model.variable_type[1] == NL._CONTINUOUS
    @test model.variable_type[2] == NL._CONTINUOUS
    return
end

function test_parse_header_integrality_both_int()
    model = NL._CacheModel()
    NL._resize_variables(model, 4)
    io = IOBuffer()
    write(
        io,
        """g3 0 1 0	# problem test_simple
2 1 1 0 0	# vars, constraints, objectives, ranges, eqns
1 1	# nonlinear constraints, objectives
0 0	# network constraints: nonlinear, linear
2 1 1	# nonlinear vars in constraints, objectives, both
0 0 0 1	# linear network variables; functions; arith, flags
0 0 1 0 0	# discrete variables: binary, integer, nonlinear (b,c,o)
1 1	# nonzeros in Jacobian, gradients
0 0	# max name lengths: constraints, variables
0 0 0 0 0	# common exprs: b,c,o,c1,o1\n""",
    )
    seekstart(io)
    NL._parse_header(io, model)
    @test model.variable_type[1] == NL._INTEGER
    @test model.variable_type[2] == NL._CONTINUOUS
    return
end

"""
    test_mac_minlp()

This function tests reading the files in Sven Leyffer's MINLP testset, which is
available at: https://wiki.mcs.anl.gov/leyffer/index.php/MacMINLP.

If the folder isn't available locally, the test is skipped.
"""
function test_mac_minlp()
    dir = joinpath(@__DIR__, "MacMINLP")
    if !isdir(dir)
        return
    end
    exclude = [
        # Has subexpressions
        "space-25-r.nl",
        "space-960-ir.nl",
        "space-960-r.nl",
        "top1-15x05.nl",
    ]
    for file in filter(f -> !(f in exclude) && endswith(f, ".nl"), readdir(dir))
        model = NL.Model()
        open(joinpath(dir, file), "r") do io
            return read!(io, model)
        end
    end
    return
end

end

TestNonlinearRead.runtests()
