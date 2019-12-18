using Test

import MutableArithmetics
const MA = MutableArithmetics

using MathOptInterface
const MOI = MathOptInterface

@testset "promote_operation with $T" for T in [Float64, Float32]
    @test MA.promote_operation(*, MOI.SingleVariable, T) == MOI.ScalarAffineFunction{T}
    @test MA.promote_operation(*, T, MOI.SingleVariable) == MOI.ScalarAffineFunction{T}
    @test MA.promote_operation(*, MOI.ScalarAffineFunction{T}, T) == MOI.ScalarAffineFunction{T}
    @test MA.promote_operation(*, T, MOI.ScalarAffineFunction{T}) == MOI.ScalarAffineFunction{T}
    @test MA.promote_operation(*, MOI.ScalarQuadraticFunction{T}, T) == MOI.ScalarQuadraticFunction{T}
    @test MA.promote_operation(*, T, MOI.ScalarQuadraticFunction{T}) == MOI.ScalarQuadraticFunction{T}
end

@testset "scaling with $T" for T in [Float64, Float32]
    x = MOI.VariableIndex(1)
    fx = MOI.SingleVariable(x)
    @test T(3) == MA.scaling(T(0)fx + T(3))
    f = T(2)fx + T(3)
    err = InexactError(:convert, T, f)
    @test_throws err MA.scaling(f)
end

@testset "Unary `-` with $T" for T in [Float64, Float32]
    x = MOI.VariableIndex(1)
    fx = MOI.SingleVariable(x)
    for f in [T(2)fx + T(3), T(4)*fx*fx + T(2)fx + T(3)]
        g = -f
        @test g ≈ MA.operate!(-, f)
        @test g ≈ f
        @test -g ≈ MA.operate!(-, f)
        @test -g ≈ f
    end
end

function all_tests(T::Type, a, b, c, d, e, f, g)
    exclude = ["scalar_uniform_scaling", "cube"]
    @testset "Scalar" begin
        MA.Test.scalar_test(a, exclude = exclude)
        MA.Test.scalar_test(b, exclude = exclude)
        MA.Test.scalar_test(c, exclude = exclude)
        MA.Test.scalar_test(d, exclude = exclude)
        MA.Test.scalar_test(e, exclude = exclude)
        MA.Test.scalar_test(f, exclude = exclude)
        MA.Test.scalar_test(g, exclude = exclude)
    end
    if !(a isa MOI.ScalarQuadraticFunction)
        exclude = String[]
        if T <: Integer
            push!(exclude, "quadratic_division")
        end
        @testset "Quadratic" begin
            MA.Test.quadratic_test(a, b, c, d, exclude = exclude)
            MA.Test.quadratic_test(b, c, d, e, exclude = exclude)
            MA.Test.quadratic_test(c, d, e, f, exclude = exclude)
            MA.Test.quadratic_test(d, e, f, g, exclude = exclude)
            MA.Test.quadratic_test(e, f, g, a, exclude = exclude)
            MA.Test.quadratic_test(f, g, a, b, exclude = exclude)
            MA.Test.quadratic_test(g, a, b, c, exclude = exclude)
        end
    end
    exclude = String[]
    if a isa MOI.ScalarQuadraticFunction
        push!(exclude, "sparse_quadratic")
    end
    @testset "Sparse" begin
        MA.Test.sparse_test(a, b, [a b c; b c a; a b a], exclude = exclude)
    end
    exclude = String[]
    if a isa MOI.ScalarQuadraticFunction
        push!(exclude, "matrix_vector")
        push!(exclude, "dot")
    end
    @testset "Vector" begin
        MA.Test.array_test([a, b, c], exclude = exclude)
        MA.Test.array_test([b, c, d], exclude = exclude)
        MA.Test.array_test([c, d, e], exclude = exclude)
        MA.Test.array_test([d, e, a], exclude = exclude)
        MA.Test.array_test([e, a, b], exclude = exclude)
    end
    exclude = ["broadcast_division", "matrix_uniform_scaling", "symmetric_matrix_uniform_scaling", "matrix_vector_division"]
    if a isa MOI.ScalarQuadraticFunction
        push!(exclude, "dot")
        push!(exclude, "broadcast_multiplication")
        push!(exclude, "sum_multiplication")
    end
    @testset "Matrix" begin
        MA.Test.array_test([a b; c d], exclude = exclude)
        MA.Test.array_test([c e; e d], exclude = exclude)
        MA.Test.array_test([a b c; b c a; a b a], exclude = exclude)
        MA.Test.array_test([d b c; d c e; e b a], exclude = exclude)
    end
end

x = MOI.VariableIndex(1)
y = MOI.VariableIndex(2)
fx = MOI.SingleVariable(x)
fy = MOI.SingleVariable(y)

@testset "Affine in $T" for T in [Int]
    @testset "Int" begin
        MA.Test.int_test(MOI.ScalarAffineFunction{T}, exclude = ["int_mul", "int_add_mul"])
    end
    a = T(2) * fx + T(1)
    b = T(4) * fy + T(2)
    c = T(3) * fx - T(2) * fy - T(3)
    d = T(3) * fx - T(2) * fy + T(4) * fx
    e = T(5) * fx - T(5)
    f = T(1) * fy - T(2) * fx + T(2)
    g = T(2) * fx + T(3) * fx
    all_tests(T, a, b, c, d, e, f, g)
end

@testset "MutableArithmetics with polynomials in $T" for T in [Int]
    @testset "Int" begin
        MA.Test.int_test(MOI.ScalarQuadraticFunction{T}, exclude = ["int_mul", "int_add_mul"])
    end
    a = T(2) * fx + T(1) + T(4) * fx * fy
    b = T(4) * fy + T(3) * fy * fy - T(3) * fy * fx + T(2)
    c = T(2) * fx * fx + T(3) * fx - T(2) * fy - T(3)
    d = T(2) * fx * fx + T(3) * fx - T(2) * fy + T(4) * fx - T(1) * fy * fy
    e = T(5) * fx - T(5) - T(4) * fx * fy
    f = T(1) * fy + T(2) * fy * fy - T(2) * fx + T(2)
    g = T(2) * fx + T(3) * fx + T(3) * fx * fy
    all_tests(T, a, b, c, d, e, f, g)
end
