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
    cache = zeros(UInt8, 63)
    for i in (1, 9, 10, 11, 99)
        io = IOBuffer()
        print(io, " $i\n")
        seekstart(io)
        @test NL._next(Int, io, cache) == i
    end
    return
end

function test_parse_expr()
    io = IOBuffer()
    write(io, "o2\nv0\no2\nn2\no2\nv3\nv1\n")
    # (* x1 (* 2 (* x4 x2)))
    seekstart(io)
    x = MOI.VariableIndex.(1:4)
    @test NL._parse_expr(io) == :(*($(x[1]), *(2, *($(x[4]), $(x[2])))))
    return
end

function test_parse_expr_nary()
    io = IOBuffer()
    write(io, "o54\n4\no5\nv0\nn2\no5\nv2\nn2\no5\nv3\nn2\no5\nv1\nn2\n")
    seekstart(io)
    x = MOI.VariableIndex.(1:4)
    @test NL._parse_expr(io) == :(+($(x[1])^2, $(x[3])^2, $(x[4])^2, $(x[2])^2))
    return
end

function test_hs071()
    model = open(joinpath(@__DIR__, "data", "hs071.nl"), "r") do io
        return read(io, NL.Model)
    end
    print(model)
    return
end

end

TestNonlinearRead.runtests()
