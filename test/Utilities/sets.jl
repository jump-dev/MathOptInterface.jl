module TestSets

using SparseArrays
using Test
using MathOptInterface

const MOI = MathOptInterface
const MOIU = MOI.Utilities

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
    @test MOIU.supports_shift_constant(MOI.GreaterThan{Int})
    @test MOIU.shift_constant(MOI.GreaterThan(6), -1) == MOI.GreaterThan(5)
    @test MOIU.supports_shift_constant(MOI.LessThan{Int})
    @test MOIU.shift_constant(MOI.LessThan(2), 2) == MOI.LessThan(4)
    @test MOIU.supports_shift_constant(MOI.Interval{Int})
    @test MOIU.shift_constant(MOI.Interval(-2, 3), 1) == MOI.Interval(-1, 4)
    @test MOIU.supports_shift_constant(MOI.ZeroOne) == false
    @test_throws MethodError MOIU.shift_constant(MOI.ZeroOne(), 1.0)
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

end  # module

TestSets.runtests()
