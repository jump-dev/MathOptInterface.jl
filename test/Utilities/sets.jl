# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestSets

using SparseArrays
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

function test_diagonal_element()
    k = 0
    for j in 1:10
        for i in 1:j
            k += 1
            @test MOIU.is_diagonal_vectorized_index(k) == (i == j)
        end
    end
    for side_dim in 1:10
        set = MOI.PositiveSemidefiniteConeTriangle(side_dim)
        vec_dim = MOI.dimension(set)
        @test MOIU.side_dimension_for_vectorized_dimension(vec_dim) == side_dim
    end
    return
end

function test_side_dimension()
    for side_dim in 1:10
        set = MOI.PositiveSemidefiniteConeTriangle(side_dim)
        vec_dim = MOI.dimension(set)
        @test MOIU.side_dimension_for_vectorized_dimension(vec_dim) == side_dim
    end
    return
end

function test_shifts()
    @test MOIU.supports_shift_constant(MOI.EqualTo{Int})
    @test MOIU.shift_constant(MOI.EqualTo(3), 1) == MOI.EqualTo(4)
    @test MOIU.shift_constant(MOI.EqualTo(3), im) == MOI.EqualTo(3 + im)
    @test MOIU.supports_shift_constant(MOI.GreaterThan{Int})
    @test MOIU.shift_constant(MOI.GreaterThan(6), -1) == MOI.GreaterThan(5)
    @test MOIU.shift_constant(MOI.GreaterThan(6), -1.0) == MOI.GreaterThan(5.0)
    @test MOIU.supports_shift_constant(MOI.LessThan{Int})
    @test MOIU.shift_constant(MOI.LessThan(2), 2) == MOI.LessThan(4)
    @test MOIU.shift_constant(MOI.LessThan(2), 2.0) == MOI.LessThan(4.0)
    @test MOIU.supports_shift_constant(MOI.Interval{Int})
    @test MOIU.shift_constant(MOI.Interval(-2, 3), 1) == MOI.Interval(-1, 4)
    @test MOIU.supports_shift_constant(MOI.ZeroOne) == false
    @test_throws MethodError MOIU.shift_constant(MOI.ZeroOne(), 1.0)
    @test MOIU.supports_shift_constant(MOI.Parameter{Int})
    @test MOIU.shift_constant(MOI.Parameter(3), 1) == MOI.Parameter(4)
    return
end

function test_set_dot()
    vec = zeros(6)
    @test MOIU.set_dot(vec, vec, MOI.SecondOrderCone(6)) == 0
    @test MOIU.set_dot(vec, vec, MOI.PositiveSemidefiniteConeTriangle(3)) == 0
    @test MOIU.set_dot(vec, vec, MOI.PositiveSemidefiniteConeSquare(3)) == 0
    vec[5] = 1
    @test MOIU.set_dot(vec, vec, MOI.SecondOrderCone(6)) == 1
    @test MOIU.set_dot(vec, vec, MOI.PositiveSemidefiniteConeTriangle(3)) == 2
    @test MOIU.set_dot(vec, vec, MOI.PositiveSemidefiniteConeSquare(3)) == 1
    vec[5] = 0
    vec[3] = 1
    @test MOIU.set_dot(vec, vec, MOI.SecondOrderCone(6)) == 1
    @test MOIU.set_dot(vec, vec, MOI.PositiveSemidefiniteConeTriangle(3)) == 1
    @test MOIU.set_dot(vec, vec, MOI.PositiveSemidefiniteConeSquare(3)) == 1

    vec = zeros(7)
    @test MOIU.set_dot(vec, vec, MOI.RootDetConeTriangle(3)) == 0
    vec[5] = 1
    @test MOIU.set_dot(vec, vec, MOI.RootDetConeTriangle(3)) == 2
    vec = zeros(8)
    @test MOIU.set_dot(vec, vec, MOI.LogDetConeTriangle(3)) == 0
    vec[5] = 1
    @test MOIU.set_dot(vec, vec, MOI.LogDetConeTriangle(3)) == 1

    sp_vec = spzeros(6)
    @test MOIU.set_dot(sp_vec, sp_vec, MOI.SecondOrderCone(6)) == 0
    @test MOIU.set_dot(
        sp_vec,
        sp_vec,
        MOI.PositiveSemidefiniteConeTriangle(3),
    ) == 0
    @test MOIU.set_dot(sp_vec, sp_vec, MOI.PositiveSemidefiniteConeSquare(3)) ==
          0
    sp_vec[5] = 1
    @test MOIU.set_dot(sp_vec, sp_vec, MOI.SecondOrderCone(6)) == 1
    @test MOIU.set_dot(
        sp_vec,
        sp_vec,
        MOI.PositiveSemidefiniteConeTriangle(3),
    ) == 2
    @test MOIU.set_dot(sp_vec, sp_vec, MOI.PositiveSemidefiniteConeSquare(3)) ==
          1
    sp_vec[5] = 0
    sp_vec[3] = 1
    @test MOIU.set_dot(sp_vec, sp_vec, MOI.SecondOrderCone(6)) == 1
    @test MOIU.set_dot(
        sp_vec,
        sp_vec,
        MOI.PositiveSemidefiniteConeTriangle(3),
    ) == 1
    @test MOIU.set_dot(sp_vec, sp_vec, MOI.PositiveSemidefiniteConeSquare(3)) ==
          1

    sp_vec = spzeros(7)
    @test MOIU.set_dot(sp_vec, sp_vec, MOI.RootDetConeTriangle(3)) == 0
    sp_vec[5] = 1
    @test MOIU.set_dot(sp_vec, sp_vec, MOI.RootDetConeTriangle(3)) == 2
    sp_vec = spzeros(8)
    @test MOIU.set_dot(sp_vec, sp_vec, MOI.LogDetConeTriangle(3)) == 0
    sp_vec[5] = 1
    @test MOIU.set_dot(sp_vec, sp_vec, MOI.LogDetConeTriangle(3)) == 1
end

function test_dot_coefficients()
    vec = zeros(6)
    @test MOIU.dot_coefficients(vec, MOI.SecondOrderCone(6)) == vec
    @test MOIU.dot_coefficients(vec, MOI.PositiveSemidefiniteConeTriangle(3)) ==
          vec
    vec[5] = 1
    @test MOIU.dot_coefficients(vec, MOI.SecondOrderCone(6)) == vec
    @test MOIU.dot_coefficients(vec, MOI.PositiveSemidefiniteConeTriangle(3)) ==
          vec ./ 2
    vec[5] = 0
    vec[3] = 1
    @test MOIU.dot_coefficients(vec, MOI.SecondOrderCone(6)) == vec
    @test MOIU.dot_coefficients(vec, MOI.PositiveSemidefiniteConeTriangle(3)) ==
          vec

    vec = zeros(7)
    @test MOIU.dot_coefficients(vec, MOI.RootDetConeTriangle(3)) == vec
    vec[5] = 1
    @test MOIU.dot_coefficients(vec, MOI.RootDetConeTriangle(3)) == vec ./ 2
    vec = zeros(8)
    @test MOIU.dot_coefficients(vec, MOI.LogDetConeTriangle(3)) == vec
    vec[5] = 1
    @test MOIU.dot_coefficients(vec, MOI.LogDetConeTriangle(3)) == vec

    sp_vec = spzeros(6)
    @test MOIU.dot_coefficients(sp_vec, MOI.SecondOrderCone(6)) == sp_vec
    @test MOIU.dot_coefficients(
        sp_vec,
        MOI.PositiveSemidefiniteConeTriangle(3),
    ) == sp_vec
    sp_vec[5] = 1
    @test MOIU.dot_coefficients(sp_vec, MOI.SecondOrderCone(6)) == sp_vec
    @test MOIU.dot_coefficients(
        sp_vec,
        MOI.PositiveSemidefiniteConeTriangle(3),
    ) == sp_vec ./ 2
    sp_vec[5] = 0
    sp_vec[3] = 1
    @test MOIU.dot_coefficients(sp_vec, MOI.SecondOrderCone(6)) == sp_vec
    @test MOIU.dot_coefficients(
        sp_vec,
        MOI.PositiveSemidefiniteConeTriangle(3),
    ) == sp_vec

    sp_vec = spzeros(7)
    @test MOIU.dot_coefficients(sp_vec, MOI.RootDetConeTriangle(3)) == sp_vec
    sp_vec[5] = 1
    @test MOIU.dot_coefficients(sp_vec, MOI.RootDetConeTriangle(3)) ==
          sp_vec ./ 2
    sp_vec = spzeros(8)
    @test MOIU.dot_coefficients(sp_vec, MOI.LogDetConeTriangle(3)) == sp_vec
    sp_vec[5] = 1
    @test MOIU.dot_coefficients(sp_vec, MOI.LogDetConeTriangle(3)) == sp_vec
end

function test_trimap()
    @test MOIU.trimap(1, 1) == 1
    @test MOIU.trimap(1, 2) == 2
    @test MOIU.trimap(2, 1) == 2
    @test MOIU.trimap(2, 2) == 3
    @test MOIU.trimap(3, 1) == 4
    @test MOIU.trimap(1, 3) == 4
    @test MOIU.trimap(3, 2) == 5
    @test MOIU.trimap(2, 3) == 5
    @test MOIU.trimap(3, 3) == 6
    return
end

function test_symmetric_matrix_scaling()
    n = 10
    v = MOI.Utilities.SymmetricMatrixScalingVector{Float64}(1.5, 0.5)
    @test sprint(show, v) == "SymmetricMatrixScalingVector{Float64}(1.5, 0.5)"
    s = MOI.Utilities.symmetric_matrix_scaling_vector(Float64)
    @test sprint(show, s) == "SymmetricMatrixScalingVector{Float64}(√2)"
    s32 = MOI.Utilities.symmetric_matrix_scaling_vector(Float32)
    @test sprint(show, s32) == "SymmetricMatrixScalingVector{Float32}(√Float32(2))"
    is = MOI.Utilities.symmetric_matrix_inverse_scaling_vector(Float64)
    @test sprint(show, is) == "SymmetricMatrixScalingVector{Float64}(inv(√2))"
    is32 = MOI.Utilities.symmetric_matrix_inverse_scaling_vector(Float32)
    @test sprint(show, is32) == "SymmetricMatrixScalingVector{Float32}(inv(√Float32(2)))"
    k = 0
    for j in 1:n
        for i in 1:(j-1)
            k += 1
            @test v[k] == 1.5
            @test s[k] == √2
            @test s32[k] == √Float32(2)
            @test is[k] == inv(√2)
            @test is32[k] == inv(√Float32(2))
        end
        k += 1
        @test v[k] == 0.5
        @test isone(s[k])
        @test isone(s32[k])
        @test isone(is[k])
        @test isone(is32[k])
    end
end

end  # module

TestSets.runtests()
