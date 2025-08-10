# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestOperate

using Test

import LinearAlgebra
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

function _test_function(pair::Pair{Symbol,<:Any})
    head, arg = pair
    if arg isa Vector
        args = Any[_test_function(a) for a in arg]
        return MOI.ScalarNonlinearFunction(head, args)
    else
        return MOI.ScalarNonlinearFunction(head, Any[_test_function(arg)])
    end
end

function _test_function(pairs::Vector{<:Any})
    return MOI.VectorNonlinearFunction(Any[_test_function(f) for f in pairs])
end

function test_operate_1a()
    for coef in (
        (0, 0, 0),
        (1, 0, 0),
        (0, 1, 0),
        (0, 0, 1),
        (1, 1, 1),
        :log => (0, 0, 0),
        [(0, 0, 0)],
        [(1, 0, 0)],
        [(0, 1, 0)],
        [(0, 0, 1)],
        [(1, 1, 1)],
        [:log => (0, 0, 0)],
    )
        f = _test_function(coef)
        @test MOI.Utilities.operate(+, Int, f) == f
        @test MOI.Utilities.operate!(+, Int, f) == f
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
        :log => (0, 0, 0),
        [(0, 0, 0)],
        [(1, 0, 0)],
        [(0, 1, 0)],
        [(0, 0, 1)],
        [(1, 1, 1)],
        [:log => (0, 0, 0)],
    )
    special_cases = Dict((0, 0, 0) => (0, 1, 0))
    for i in 1:6, j in 1:6
        fi, fj = _test_function(F[i]), _test_function(F[j])
        Fi = get(special_cases, F[i], F[i])
        Fj = get(special_cases, F[j], F[j])
        if i == 6 || j == 6
            fk = MOI.ScalarNonlinearFunction(:+, Any[fi, fj])
        else
            fk = _test_function(Fi .+ Fj)
        end
        @test MOI.Utilities.operate(+, Int, fi, fj) ≈ fk
        @test MOI.Utilities.operate!(+, Int, fi, fj) ≈ fk
    end
    for i in 7:12, j in 7:12
        fi, fj = _test_function(F[i]), _test_function(F[j])
        if i == 12 || j == 12
            args = Any[]
            for (fi_, fj_) in zip(F[i], F[j])
                push!(
                    args,
                    MOI.Utilities.operate(
                        +,
                        Int,
                        _test_function(fi_),
                        _test_function(fj_),
                    ),
                )
            end
            fk = MOI.VectorNonlinearFunction(args)
        else
            k = map(zip(F[i], F[j])) do (x, y)
                return get(special_cases, x, x) .+ get(special_cases, y, y)
            end
            fk = _test_function(k)
        end
        @test MOI.Utilities.operate(+, Int, fi, fj) ≈ fk
        @test MOI.Utilities.operate!(+, Int, fi, fj) ≈ fk
    end
    return
end

function test_operate_1b_scalarnonlinearfunction_specialcase()
    x = MOI.VariableIndex(1)
    f = MOI.ScalarNonlinearFunction(:+, Any[x])
    g = MOI.Utilities.operate!(+, Float64, f, x)
    @test g === f
    @test g ≈ MOI.ScalarNonlinearFunction(:+, Any[x, x])
    return
end

function test_operate_1c()
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
        @test MOI.Utilities.operate(+, Int, f, f, f) ≈
              MOI.Utilities.operate(*, Int, f, 3)
        g = _test_function(coef)
        @test MOI.Utilities.operate!(+, Int, g, f, f) ≈
              MOI.Utilities.operate(*, Int, f, 3)
    end
    return
end

function test_operate_1c_many_arguments()
    x = [i + i * MOI.VariableIndex(i) for i in 1:1_000]
    y = MOI.Utilities.operate(+, Int, x...)
    f = MOI.ScalarAffineFunction(
        [MOI.ScalarAffineTerm(i, MOI.VariableIndex(i)) for i in 1:1_000],
        sum(i for i in 1:1_000),
    )
    @test ≈(y, f)
    z = zero(MOI.ScalarAffineFunction{Int})
    z = MOI.Utilities.operate!(+, Int, z, x...)
    @test ≈(z, f)
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
        (:log => (0, 0, 0)) => (:- => (:log => (0, 0, 0))),
        [(0, 0, 0)] => [(0, -1, 0)],
        [(1, 0, 0)] => [(-1, 0, 0)],
        [(0, 1, 0)] => [(0, -1, 0)],
        [(0, 0, 1)] => [(0, 0, -1)],
        [(1, 1, 1)] => [(-1, -1, -1)],
        [(:log => (0, 0, 0))] => [(:- => (:log => (0, 0, 0)))],
    )
        @test MOI.Utilities.operate(-, T, _test_function(f)) ≈ _test_function(g)
        @test MOI.Utilities.operate!(-, T, _test_function(f)) ≈
              _test_function(g)
    end
    return
end

function test_operate_2a_scalarnonlinearfunction_specialcase()
    x = MOI.VariableIndex(1)
    f = MOI.ScalarNonlinearFunction(:log, Any[x])
    g = MOI.ScalarNonlinearFunction(:-, Any[f])
    h = MOI.Utilities.operate!(-, Float64, g)
    @test h === f
    @test h ≈ MOI.ScalarNonlinearFunction(:log, Any[x])
    return
end

function test_operate_2b()
    F = (
        (0, 0, 0),
        (1, 0, 0),
        (0, 1, 0),
        (0, 0, 1),
        (1, 1, 1),
        :log => (0, 0, 0),
        [(0, 0, 0)],
        [(1, 0, 0)],
        [(0, 1, 0)],
        [(0, 0, 1)],
        [(1, 1, 1)],
        [:log => (0, 0, 0)],
    )
    special_cases = Dict((0, 0, 0) => (0, 1, 0))
    for i in 1:6, j in 1:6
        if i == 6 || j == 6
            fi = _test_function(F[i])
            fj = _test_function(F[j])
            fk = MOI.ScalarNonlinearFunction(:-, Any[fi, fj])
        else
            fi = _test_function(2 .* F[i])
            fj = _test_function(F[j])
            Fi = get(special_cases, 2 .* F[i], 2 .* F[i])
            Fj = get(special_cases, F[j], F[j])
            Fk = Fi .- Fj
            fk = _test_function(get(special_cases, Fk, Fk))
            if (i, j) in ((1, 1), (1, 3))
                fk = zero(MOI.ScalarAffineFunction{Int})
            end
        end
        @test MOI.Utilities.operate(-, Int, fi, fj) ≈ fk
        @test MOI.Utilities.operate!(-, Int, fi, fj) ≈ fk
    end
    for i in 7:12, j in 7:12
        fi, fj = _test_function(F[i]), _test_function(F[j])
        if i == 12 || j == 12
            args = Any[]
            for (fi_, fj_) in zip(F[i], F[j])
                push!(
                    args,
                    MOI.Utilities.operate(
                        -,
                        Int,
                        _test_function(fi_),
                        _test_function(fj_),
                    ),
                )
            end
            fk = MOI.VectorNonlinearFunction(args)
        else
            F2 = [2 .* fi for fi in F[i]]
            fi, fj = _test_function(F2), _test_function(F[j])
            k = map(zip(F2, F[j])) do (x, y)
                return get(special_cases, x, x) .- get(special_cases, y, y)
            end
            fk = _test_function(k)
            if (i, j) in ((7, 7), (7, 9))
                fk = MOI.VectorAffineFunction(MOI.VectorAffineTerm{Int}[], [0])
            end
        end
        @test MOI.Utilities.operate(-, Int, fi, fj) ≈ fk
        @test MOI.Utilities.operate!(-, Int, fi, fj) ≈ fk
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
        (:log => (0, 0, 0)) => (:* => [(3, 0, 0), (:log => (0, 0, 0))]),
        [(0, 0, 0)] => [(0, 3, 0)],
        [(1, 0, 0)] => [(3, 0, 0)],
        [(0, 1, 0)] => [(0, 3, 0)],
        [(0, 0, 1)] => [(0, 0, 3)],
        [(1, 1, 1)] => [(3, 3, 3)],
        [(:log => (0, 0, 0))] => [(:* => [(3, 0, 0), (:log => (0, 0, 0))])],
    )
        f = _test_function(f)
        @test MOI.Utilities.operate(*, T, 3, f) ≈ _test_function(g)
        @test MOI.Utilities.operate!(*, T, 3, f) ≈ _test_function(g)
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
        (:log => (0, 0, 0)) => (:* => [(:log => (0, 0, 0)), (3, 0, 0)]),
        [(0, 0, 0)] => [(0, 3, 0)],
        [(1, 0, 0)] => [(3, 0, 0)],
        [(0, 1, 0)] => [(0, 3, 0)],
        [(0, 0, 1)] => [(0, 0, 3)],
        [(1, 1, 1)] => [(3, 3, 3)],
        [(:log => (0, 0, 0))] => [(:* => [(:log => (0, 0, 0)), (3, 0, 0)])],
    )
        f = _test_function(f)
        @test MOI.Utilities.operate(*, T, f, 3) ≈ _test_function(g)
        @test MOI.Utilities.operate!(*, T, f, 3) ≈ _test_function(g)
    end
    return
end

function test_operate_3b_scalarnonlinearfunction_specialcase()
    x = MOI.VariableIndex(1)
    f = MOI.ScalarNonlinearFunction(:*, Any[x])
    g = MOI.Utilities.operate!(*, Float64, f, 3.0)
    @test g === f
    @test g ≈ MOI.ScalarNonlinearFunction(:*, Any[x, 3.0])
    return
end

function test_operate_3c()
    x = MOI.VariableIndex(1)
    f = 1.0 * x + 2.0
    g = MOI.Utilities.operate(*, Float64, f, x)
    @test g ≈ 1.0 * x * x + 2.0 * x
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
        (:log => (0, 0, 0)) => (:/ => [(:log => (0, 0, 0)), (2.0, 0.0, 0.0)]),
        [(0.0, 0.0, 0.0)] => [(0.0, 0.5, 0.0)],
        [(1.0, 0.0, 0.0)] => [(0.5, 0.0, 0.0)],
        [(0.0, 1.0, 0.0)] => [(0.0, 0.5, 0.0)],
        [(0.0, 0.0, 1.0)] => [(0.0, 0.0, 0.5)],
        [(1.0, 1.0, 1.0)] => [(0.5, 0.5, 0.5)],
        [(:log => (0, 0, 0))] =>
            [(:/ => [(:log => (0, 0, 0)), (2.0, 0.0, 0.0)])],
    )
        f = _test_function(f)
        @test MOI.Utilities.operate(/, T, f, 2.0) ≈ _test_function(g)
        @test MOI.Utilities.operate!(/, T, f, 2.0) ≈ _test_function(g)
    end
    return
end

function test_operate_5a()
    T = Float64
    F = (
        (0.0, 0.0, 0.0),
        (1.0, 0.0, 0.0),
        (0.0, 1.0, 0.0),
        (0.0, 0.0, 1.0),
        (1.0, 1.0, 1.0),
        (:log => (0, 0, 0)),
        [(0.0, 0.0, 0.0)],
        [(1.0, 0.0, 0.0)],
        [(0.0, 1.0, 0.0)],
        [(0.0, 0.0, 1.0)],
        [(1.0, 1.0, 1.0)],
        [:log => (0, 0, 0)],
    )
    for f in F, g in F
        h = vcat(f, g)
        args = (_test_function(f), _test_function(g))
        @test MOI.Utilities.operate(vcat, T, args...) ≈ _test_function(h)
        args = (_test_function(f), _test_function(g))
        @test MOI.Utilities.operate!(vcat, T, args...) ≈ _test_function(h)
    end
    @test MOI.Utilities.operate(vcat, T) == T[]
    return
end

function test_operate_5a_VectorNonlinearFunction()
    x = MOI.ScalarNonlinearFunction(:+, Any[])
    f = MOI.VectorNonlinearFunction([x])
    g = MOI.Utilities.operate(vcat, Float64, f, f, x)
    rows = [MOI.ScalarNonlinearFunction(:+, Any[]) for _ in 1:3]
    h = MOI.VectorNonlinearFunction(rows)
    @test g ≈ h
    push!(x.args, 0.0)
    @test g ≈ h
    @test g.rows[1] !== g.rows[2]
    @test g.rows[1] !== g.rows[3]
    @test g.rows[2] !== g.rows[3]
    rows = [MOI.ScalarNonlinearFunction(:+, Any[0.0])]
    f_new = MOI.VectorNonlinearFunction(rows)
    @test f ≈ f_new
    return
end

function test_operate_6a()
    T = Float64
    @test MOI.Utilities.operate(imag, T, _test_function((0.0, 0.0, 0.0))) ≈
          zero(MOI.ScalarAffineFunction{T})
    @test MOI.Utilities.operate(imag, T, _test_function([(0.0, 0.0, 0.0)])) ≈
          MOI.VectorAffineFunction(MOI.VectorAffineTerm{T}[], [0.0])
    @test MOI.Utilities.operate!(imag, T, _test_function((0.0, 0.0, 0.0))) ≈
          zero(MOI.ScalarAffineFunction{T})
    @test MOI.Utilities.operate!(imag, T, _test_function([(0.0, 0.0, 0.0)])) ≈
          MOI.VectorAffineFunction(MOI.VectorAffineTerm{T}[], [0.0])
    return
end

function test_operate_term_1a()
    x = MOI.VariableIndex(1)
    for f in (
        MOI.ScalarAffineTerm(1.0, x),
        MOI.ScalarQuadraticTerm(1.0, x, x),
        MOI.VectorAffineTerm(3, MOI.ScalarAffineTerm(1.0, x)),
        MOI.VectorQuadraticTerm(3, MOI.ScalarQuadraticTerm(1.0, x, x)),
    )
        @test MOI.Utilities.operate_term(+, f) == f
    end
    return
end

function test_operate_term_2a()
    x = MOI.VariableIndex(1)
    for (f, g) in (
        MOI.ScalarAffineTerm(1.0, x) => MOI.ScalarAffineTerm(-1.0, x),
        MOI.ScalarQuadraticTerm(1.0, x, x) =>
            MOI.ScalarQuadraticTerm(-1.0, x, x),
        MOI.VectorAffineTerm(3, MOI.ScalarAffineTerm(1.0, x)) =>
            MOI.VectorAffineTerm(3, MOI.ScalarAffineTerm(-1.0, x)),
        MOI.VectorQuadraticTerm(3, MOI.ScalarQuadraticTerm(1.0, x, x)) =>
            MOI.VectorQuadraticTerm(3, MOI.ScalarQuadraticTerm(-1.0, x, x)),
    )
        @test MOI.Utilities.operate_term(-, f) == g
    end
    return
end

function test_operate_term_3a()
    x = MOI.VariableIndex(1)
    for (f, g) in (
        MOI.ScalarAffineTerm(2.0, x) => MOI.ScalarAffineTerm(-4.0, x),
        MOI.ScalarQuadraticTerm(2.0, x, x) =>
            MOI.ScalarQuadraticTerm(-4.0, x, x),
        MOI.VectorAffineTerm(3, MOI.ScalarAffineTerm(2.0, x)) =>
            MOI.VectorAffineTerm(3, MOI.ScalarAffineTerm(-4.0, x)),
        MOI.VectorQuadraticTerm(3, MOI.ScalarQuadraticTerm(2.0, x, x)) =>
            MOI.VectorQuadraticTerm(3, MOI.ScalarQuadraticTerm(-4.0, x, x)),
    )
        @test MOI.Utilities.operate_term(*, -2.0, f) == g
    end
    return
end

function test_operate_term_3b()
    x = MOI.VariableIndex(1)
    for (f, g) in (
        MOI.ScalarAffineTerm(2.0, x) => MOI.ScalarAffineTerm(-4.0, x),
        MOI.ScalarQuadraticTerm(2.0, x, x) =>
            MOI.ScalarQuadraticTerm(-4.0, x, x),
        MOI.VectorAffineTerm(3, MOI.ScalarAffineTerm(2.0, x)) =>
            MOI.VectorAffineTerm(3, MOI.ScalarAffineTerm(-4.0, x)),
        MOI.VectorQuadraticTerm(3, MOI.ScalarQuadraticTerm(2.0, x, x)) =>
            MOI.VectorQuadraticTerm(3, MOI.ScalarQuadraticTerm(-4.0, x, x)),
    )
        @test MOI.Utilities.operate_term(*, f, -2.0) == g
    end
    return
end

function test_operate_term_3c()
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    f = MOI.ScalarAffineTerm(2.0, x)
    g = MOI.ScalarAffineTerm(2.0, y)
    @test MOI.Utilities.operate_term(*, f, f) ==
          MOI.ScalarQuadraticTerm(8.0, x, x)
    @test MOI.Utilities.operate_term(*, f, g) ==
          MOI.ScalarQuadraticTerm(4.0, x, y)
    h = MOI.VectorAffineTerm(1, f)
    i = MOI.VectorAffineTerm(1, g)
    @test MOI.Utilities.operate_term(*, h, h) ==
          MOI.VectorQuadraticTerm(1, MOI.ScalarQuadraticTerm(8.0, x, x))
    @test MOI.Utilities.operate_term(*, h, i) ==
          MOI.VectorQuadraticTerm(1, MOI.ScalarQuadraticTerm(4.0, x, y))
    return
end

function test_operate_term_3d()
    x = MOI.VariableIndex(1)
    f = MOI.ScalarAffineTerm(2.0, x)
    @test MOI.Utilities.operate_term(*, 0.5, f, 0.4) ==
          MOI.ScalarAffineTerm(0.5 * 2.0 * 0.4, x)
    g = MOI.ScalarQuadraticTerm(8.0, x, x)
    @test MOI.Utilities.operate_term(*, 0.5, g, 0.4) ==
          MOI.ScalarQuadraticTerm(0.5 * 8.0 * 0.4, x, x)
    return
end

function test_operate_term_3e()
    D = LinearAlgebra.Diagonal([0.5, 0.6])
    x = MOI.VariableIndex(1)
    f = MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(2.0, x))
    @test MOI.Utilities.operate_term(*, D, f) ==
          MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(0.5 * 2.0, x))
    g = MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(2.0, x))
    @test MOI.Utilities.operate_term(*, D, g) ==
          MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(0.6 * 2.0, x))
    f = MOI.VectorQuadraticTerm(1, MOI.ScalarQuadraticTerm(2.0, x, x))
    @test MOI.Utilities.operate_term(*, D, f) ==
          MOI.VectorQuadraticTerm(1, MOI.ScalarQuadraticTerm(0.5 * 2.0, x, x))
    g = MOI.VectorQuadraticTerm(2, MOI.ScalarQuadraticTerm(2.0, x, x))
    @test MOI.Utilities.operate_term(*, D, g) ==
          MOI.VectorQuadraticTerm(2, MOI.ScalarQuadraticTerm(0.6 * 2.0, x, x))
    return
end

function test_operate_term_4a()
    x = MOI.VariableIndex(1)
    for (f, g) in (
        MOI.ScalarAffineTerm(2.0, x) => MOI.ScalarAffineTerm(-4.0, x),
        MOI.ScalarQuadraticTerm(2.0, x, x) =>
            MOI.ScalarQuadraticTerm(-4.0, x, x),
        MOI.VectorAffineTerm(3, MOI.ScalarAffineTerm(2.0, x)) =>
            MOI.VectorAffineTerm(3, MOI.ScalarAffineTerm(-4.0, x)),
        MOI.VectorQuadraticTerm(3, MOI.ScalarQuadraticTerm(2.0, x, x)) =>
            MOI.VectorQuadraticTerm(3, MOI.ScalarQuadraticTerm(-4.0, x, x)),
    )
        @test MOI.Utilities.operate_term(/, f, -0.5) == g
    end
    return
end

function test_operate_term_5a()
    x = MOI.VariableIndex(1)
    for f in (
        MOI.ScalarAffineTerm(1.0, x),
        MOI.ScalarQuadraticTerm(1.0, x, x),
        MOI.VectorAffineTerm(3, MOI.ScalarAffineTerm(1.0, x)),
        MOI.VectorQuadraticTerm(3, MOI.ScalarQuadraticTerm(1.0, x, x)),
    )
        @test MOI.Utilities.operate_term(t -> -(t), f) ==
              MOI.Utilities.operate_term(-, f)
    end
    return
end

function test_operate_terms_1a()
    x = MOI.VariableIndex(1)
    for term in (
        MOI.ScalarAffineTerm(2.0, x),
        MOI.ScalarQuadraticTerm(2.0, x, x),
        MOI.VectorAffineTerm(3, MOI.ScalarAffineTerm(2.0, x)),
        MOI.VectorQuadraticTerm(3, MOI.ScalarQuadraticTerm(2.0, x, x)),
    )
        @test MOI.Utilities.operate_terms(+, [term]) == [term]
    end
    return
end

function test_operate_terms_2a()
    x = MOI.VariableIndex(1)
    for term in (
        MOI.ScalarAffineTerm(2.0, x),
        MOI.ScalarQuadraticTerm(2.0, x, x),
        MOI.VectorAffineTerm(3, MOI.ScalarAffineTerm(2.0, x)),
        MOI.VectorQuadraticTerm(3, MOI.ScalarQuadraticTerm(2.0, x, x)),
    )
        @test MOI.Utilities.operate_terms(-, [term]) ==
              [MOI.Utilities.operate_term(-, term)]
    end
    return
end

function test_operate_terms_3a()
    D = LinearAlgebra.Diagonal([0.5, 0.6])
    x = MOI.VariableIndex(1)
    terms = [
        MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(2.0, x)),
        MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(2.0, x)),
    ]
    @test MOI.Utilities.operate_terms(*, D, terms) == [
        MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(0.5 * 2.0, x)),
        MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(0.6 * 2.0, x)),
    ]
    terms = [
        MOI.VectorQuadraticTerm(1, MOI.ScalarQuadraticTerm(2.0, x, x)),
        MOI.VectorQuadraticTerm(2, MOI.ScalarQuadraticTerm(2.0, x, x)),
    ]
    @test MOI.Utilities.operate_terms(*, D, terms) == [
        MOI.VectorQuadraticTerm(1, MOI.ScalarQuadraticTerm(0.5 * 2.0, x, x)),
        MOI.VectorQuadraticTerm(2, MOI.ScalarQuadraticTerm(0.6 * 2.0, x, x)),
    ]
    return
end

function test_operate_terms!_1a()
    x = MOI.VariableIndex(1)
    terms = [MOI.ScalarAffineTerm(2.0, x)]
    ret = MOI.Utilities.operate_terms!(-, terms)
    @test ret === terms
    @test ret == [MOI.ScalarAffineTerm(-2.0, x)]
    terms = [MOI.ScalarQuadraticTerm(2.0, x, x)]
    ret = MOI.Utilities.operate_terms!(-, terms)
    @test ret === terms
    @test ret == [MOI.ScalarQuadraticTerm(-2.0, x, x)]
    return
end

function test_operate_output_index_1a()
    T = Float64
    F = (
        (0.0, 0.0, 0.0),
        (1.0, 0.0, 0.0),
        (0.0, 1.0, 0.0),
        (0.0, 0.0, 1.0),
        (1.0, 1.0, 1.0),
        :log => (0, 0, 0),
    )
    for i in 2:6
        for j in 1:i
            if (i, j) == (2, 1)
                continue
            end
            f = _test_function(vcat(F[i], F[i]))
            fi, fj = _test_function(F[i]), _test_function(F[j])
            fij = MOI.Utilities.operate(+, T, fi, fj)
            g = MOI.Utilities.operate(vcat, T, fi, fij)
            @test MOI.Utilities.operate_output_index!(+, T, 2, f, fj) ≈ g
        end
    end
    return
end

function test_operate_coefficient()
    x = MOI.VariableIndex(1)
    for f in (
        MOI.ScalarAffineTerm(1.0, x),
        MOI.ScalarQuadraticTerm(1.0, x, x),
        MOI.VectorAffineTerm(3, MOI.ScalarAffineTerm(1.0, x)),
        MOI.VectorQuadraticTerm(3, MOI.ScalarQuadraticTerm(1.0, x, x)),
    )
        @test MOI.Utilities.operate_coefficient(t -> -(t), f) ==
              MOI.Utilities.operate_term(-, f)
    end
    return
end

function test_operate_coefficients_term()
    x = MOI.VariableIndex(1)
    for f in (
        MOI.ScalarAffineTerm(1.0, x),
        MOI.ScalarQuadraticTerm(1.0, x, x),
        MOI.VectorAffineTerm(3, MOI.ScalarAffineTerm(1.0, x)),
        MOI.VectorQuadraticTerm(3, MOI.ScalarQuadraticTerm(1.0, x, x)),
    )
        @test MOI.Utilities.operate_coefficients(t -> -(t), [f]) ==
              MOI.Utilities.operate_terms(-, [f])
    end
    return
end

function test_operate_coefficients_functions()
    T = Float64
    F = ((0.0, 1.0, 0.0), (0.0, 0.0, 1.0), (1.0, 1.0, 1.0))
    op(x) = -x
    for coef in F
        f, g = _test_function(coef), _test_function(op.(coef))
        @test MOI.Utilities.operate_coefficients(op, f) ≈ g
        f, g = _test_function([coef]), _test_function([op.(coef)])
        @test MOI.Utilities.operate_coefficients(op, f) ≈ g
    end
    return
end

function test_operate_int32()
    x = MOI.VariableIndex(1)
    y = one(Int32) * x
    @test y isa MOI.ScalarAffineFunction{Int32}
    q1 = MOI.Utilities.operate(*, Int32, x, x)
    q2 = MOI.Utilities.operate(*, Int32, y, x)
    @test q1 isa MOI.ScalarQuadraticFunction{Int32}
    @test isapprox(q1, q2)
    return
end

end  # module

TestOperate.runtests()
