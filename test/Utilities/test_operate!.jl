# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestOperate

using Test

import MathOptInterface as MOI

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

# f(x) = a + bx + cx^2
function _test_function(coefficients::NTuple{3,T}) where {T}
    a, b, c = coefficients
    x = MOI.VariableIndex(1)
    if !iszero(c)
        return MOI.ScalarQuadraticFunction(
            [MOI.ScalarQuadraticTerm(c, x, x)],
            [MOI.ScalarAffineTerm(b, x)],
            a,
        )
    end
    if !iszero(b)
        return MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(b, x)], a)
    end
    if !iszero(a)
        return a
    end
    return x
end

function _test_function(coefficients::Vector{NTuple{3,T}}) where {T}
    return MOI.Utilities.operate(vcat, T, _test_function.(coefficients)...)
end

function test_operate_1a()
    for coef in (
        (0, 0, 0),
        (1, 0, 0),
        (0, 1, 0),
        (0, 0, 1),
        (1, 1, 1),
        [(0, 0, 0)],
        [(1, 0, 0)],
        [(0, 1, 0)],
        [(0, 0, 1)],
        [(1, 1, 1)],
    )
        f = _test_function(coef)
        @test MOI.Utilities.operate(+, Int, f) == f
    end
    return
end

function test_operate_1b()
    F = (
        (0, 0, 0),
        (1, 0, 0),
        (0, 1, 0),
        (0, 0, 1),
        (1, 1, 1),
        [(0, 0, 0)],
        [(1, 0, 0)],
        [(0, 1, 0)],
        [(0, 0, 1)],
        [(1, 1, 1)],
    )
    special_cases = Dict((0, 0, 0) => (0, 1, 0))
    for i in 1:5, j in 1:5
        fi, fj = _test_function(F[i]), _test_function(F[j])
        Fi = get(special_cases, F[i], F[i])
        Fj = get(special_cases, F[j], F[j])
        fk = _test_function(Fi .+ Fj)
        @test MOI.Utilities.operate(+, Int, fi, fj) ≈ fk
    end
    for i in 6:10, j in 6:10
        fi, fj = _test_function(F[i]), _test_function(F[j])
        k = map(zip(F[i], F[j])) do (x, y)
            return get(special_cases, x, x) .+ get(special_cases, y, y)
        end
        @test MOI.Utilities.operate(+, Int, fi, fj) ≈ _test_function(k)
    end
    return
end

function test_operate_2a()
    T = Int
    for (f, g) in (
        (0, 0, 0) => (0, -1, 0),
        (1, 0, 0) => (-1, 0, 0),
        (0, 1, 0) => (0, -1, 0),
        (0, 0, 1) => (0, 0, -1),
        (1, 1, 1) => (-1, -1, -1),
        [(0, 0, 0)] => [(0, -1, 0)],
        [(1, 0, 0)] => [(-1, 0, 0)],
        [(0, 1, 0)] => [(0, -1, 0)],
        [(0, 0, 1)] => [(0, 0, -1)],
        [(1, 1, 1)] => [(-1, -1, -1)],
    )
        @test MOI.Utilities.operate(-, T, _test_function(f)) ≈ _test_function(g)
    end
    return
end

function test_operate_2b()
    F = (
        (0, 0, 0),
        (1, 0, 0),
        (0, 1, 0),
        (0, 0, 1),
        (1, 1, 1),
        [(0, 0, 0)],
        [(1, 0, 0)],
        [(0, 1, 0)],
        [(0, 0, 1)],
        [(1, 1, 1)],
    )
    special_cases = Dict((0, 0, 0) => (0, 1, 0))
    for i in 1:5, j in 1:5
        fi, fj = _test_function(2 .* F[i]), _test_function(F[j])
        Fi = get(special_cases, 2 .* F[i], 2 .* F[i])
        Fj = get(special_cases, F[j], F[j])
        Fk = Fi .- Fj
        fk = _test_function(get(special_cases, Fk, Fk))
        if (i, j) in ((1, 1), (1, 3))
            fk = zero(MOI.ScalarAffineFunction{Int})
        end
        @test MOI.Utilities.operate(-, Int, fi, fj) ≈ fk
    end
    for i in 6:10, j in 6:10
        F2 = [2 .* fi for fi in F[i]]
        fi, fj = _test_function(F2), _test_function(F[j])
        k = map(zip(F2, F[j])) do (x, y)
            return get(special_cases, x, x) .- get(special_cases, y, y)
        end
        fk = _test_function(k)
        if (i, j) in ((6, 6), (6, 8))
            fk = MOI.VectorAffineFunction(MOI.VectorAffineTerm{Int}[], [0])
        end
        @test MOI.Utilities.operate(-, Int, fi, fj) ≈ fk
    end
    return
end

function test_operate_3a()
    T = Int
    for (f, g) in (
        (0, 0, 0) => (0, 3, 0),
        (1, 0, 0) => (3, 0, 0),
        (0, 1, 0) => (0, 3, 0),
        (0, 0, 1) => (0, 0, 3),
        (1, 1, 1) => (3, 3, 3),
        [(0, 0, 0)] => [(0, 3, 0)],
        [(1, 0, 0)] => [(3, 0, 0)],
        [(0, 1, 0)] => [(0, 3, 0)],
        [(0, 0, 1)] => [(0, 0, 3)],
        [(1, 1, 1)] => [(3, 3, 3)],
    )
        f = _test_function(f)
        @test MOI.Utilities.operate(*, T, 3, f) ≈ _test_function(g)
    end
    return
end

function test_operate_3b()
    T = Int
    for (f, g) in (
        (0, 0, 0) => (0, 3, 0),
        (1, 0, 0) => (3, 0, 0),
        (0, 1, 0) => (0, 3, 0),
        (0, 0, 1) => (0, 0, 3),
        (1, 1, 1) => (3, 3, 3),
        [(0, 0, 0)] => [(0, 3, 0)],
        [(1, 0, 0)] => [(3, 0, 0)],
        [(0, 1, 0)] => [(0, 3, 0)],
        [(0, 0, 1)] => [(0, 0, 3)],
        [(1, 1, 1)] => [(3, 3, 3)],
    )
        f = _test_function(f)
        @test MOI.Utilities.operate(*, T, f, 3) ≈ _test_function(g)
    end
    return
end

function test_operate_4a()
    T = Float64
    for (f, g) in (
        (0.0, 0.0, 0.0) => (0.0, 0.5, 0.0),
        (1.0, 0.0, 0.0) => (0.5, 0.0, 0.0),
        (0.0, 1.0, 0.0) => (0.0, 0.5, 0.0),
        (0.0, 0.0, 1.0) => (0.0, 0.0, 0.5),
        (1.0, 1.0, 1.0) => (0.5, 0.5, 0.5),
        [(0.0, 0.0, 0.0)] => [(0.0, 0.5, 0.0)],
        [(1.0, 0.0, 0.0)] => [(0.5, 0.0, 0.0)],
        [(0.0, 1.0, 0.0)] => [(0.0, 0.5, 0.0)],
        [(0.0, 0.0, 1.0)] => [(0.0, 0.0, 0.5)],
        [(1.0, 1.0, 1.0)] => [(0.5, 0.5, 0.5)],
    )
        f = _test_function(f)
        @test MOI.Utilities.operate(/, T, f, 2.0) ≈ _test_function(g)
    end
    return
end

function test_operate_6a()
    T = Float64
    @test MOI.Utilities.operate(imag, T, _test_function((0.0, 0.0, 0.0))) ≈
          zero(MOI.ScalarAffineFunction{T})
    @test MOI.Utilities.operate(imag, T, _test_function([(0.0, 0.0, 0.0)])) ≈
          MOI.VectorAffineFunction(MOI.VectorAffineTerm{T}[], [0.0])
    return
end

end  # module

TestOperate.runtests()
