# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestNonlinearRead

using Test
import MathOptInterface

const MOI = MathOptInterface
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
    x = MOI.VariableIndex.(1:4)
    @test model.objective == :($(x[1]) * ($(x[3]) * ($(x[4]) * $(x[2]))))
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

function test_hs071()
    model = open(joinpath(@__DIR__, "data", "hs071.nl"), "r") do io
        return read(io, NL.Model)
    end
    print(model)
    dest = NL.Model()
    MOI.copy_to(dest, model)
    new_model = sprint(write, dest)
    # print(new_model)
    return
end

end

TestNonlinearRead.runtests()
