module TestMutableArithmetics

using Test

import MutableArithmetics
const MA = MutableArithmetics

using MathOptInterface
const MOI = MathOptInterface

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

_zero(::Type{MOI.VariableIndex}, T::Type) = zero(MOI.ScalarAffineFunction{T})
_zero(x, ::Type) = zero(x)

function _promote_operation_test(op::Function, T, x::Type, y::Type)
    f() = MA.promote_operation(op, x, y)
    @test typeof(op(_zero(x, T), _zero(y, T))) == f()
    @test 0 == @allocated f()
end

function _test_promote_operation_allocation(T)
    AffType = MOI.ScalarAffineFunction{T}
    QuadType = MOI.ScalarQuadraticFunction{T}
    for op in [+, -, *]
        _promote_operation_test(op, T, T, MOI.VariableIndex)
        _promote_operation_test(op, T, MOI.VariableIndex, T)
        _promote_operation_test(op, T, T, AffType)
        _promote_operation_test(op, T, AffType, T)
        _promote_operation_test(op, T, T, QuadType)
        _promote_operation_test(op, T, QuadType, T)
        _promote_operation_test(op, T, MOI.VariableIndex, AffType)
        _promote_operation_test(op, T, AffType, MOI.VariableIndex)
        if op != *
            _promote_operation_test(op, T, MOI.VariableIndex, QuadType)
            _promote_operation_test(op, T, QuadType, MOI.VariableIndex)
            _promote_operation_test(op, T, AffType, QuadType)
            _promote_operation_test(op, T, QuadType, AffType)
        end
    end
    return
end

function test_promote_operation_allocation_Int()
    return _test_promote_operation_allocation(Int)
end
function test_promote_operation_allocation_Float64()
    return _test_promote_operation_allocation(Float64)
end
function test_promote_operation_allocation_Float32()
    return _test_promote_operation_allocation(Float32)
end

function _test_promote_operation(T)
    @test MA.promote_operation(*, MOI.VariableIndex, T) ==
          MOI.ScalarAffineFunction{T}
    @test MA.promote_operation(*, T, MOI.VariableIndex) ==
          MOI.ScalarAffineFunction{T}
    @test MA.promote_operation(*, MOI.ScalarAffineFunction{T}, T) ==
          MOI.ScalarAffineFunction{T}
    @test MA.promote_operation(*, T, MOI.ScalarAffineFunction{T}) ==
          MOI.ScalarAffineFunction{T}
    @test MA.promote_operation(*, MOI.ScalarQuadraticFunction{T}, T) ==
          MOI.ScalarQuadraticFunction{T}
    @test MA.promote_operation(*, T, MOI.ScalarQuadraticFunction{T}) ==
          MOI.ScalarQuadraticFunction{T}
    return
end

test_promote_operation_Int() = _test_promote_operation(Int)
test_promote_operation_Float64() = _test_promote_operation(Float64)
test_promote_operation_Float32() = _test_promote_operation(Float32)

function _test_scaling(T)
    x = MOI.VariableIndex(1)
    @test T(3) == MA.scaling(T(0)x + T(3))
    f = T(2)x + T(3)
    err = InexactError(:convert, T, f)
    @test_throws err MA.scaling(f)
end

test_scaling_Float64() = _test_scaling(Float64)
test_scaling_Float32() = _test_scaling(Float32)

function test_unary_minus()
    for T in [Float64, Float32]
        x = MOI.VariableIndex(1)
        for f in [T(2)x + T(3), T(4) * x * x + T(2)x + T(3)]
            g = -f
            @test g ≈ MA.operate!(-, f)
            @test g ≈ f
            @test -g ≈ MA.operate!(-, f)
            @test -g ≈ f
        end
    end
    return
end

function _run_all_tests(T::Type, a, b, c, d, e, f, g)
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
    exclude = [
        "broadcast_division",
        "matrix_uniform_scaling",
        "symmetric_matrix_uniform_scaling",
        "matrix_vector_division",
    ]
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
    return
end

function test_VariableIndex()
    x = MOI.VariableIndex(1)
    a = 2x
    MA.Test.@test_rewrite(a + x)
    MA.Test.@test_rewrite(a + 3 * x)
    MA.Test.@test_rewrite(a + x * 4)
    MA.Test.@test_rewrite(a + 3 * x * 4)
    MA.Test.@test_rewrite(x + a)
    MA.Test.@test_rewrite(a - x)
    MA.Test.@test_rewrite(a - 3 * x)
    MA.Test.@test_rewrite(a - x * 4)
    MA.Test.@test_rewrite(a - 3 * x * 4)
    MA.Test.@test_rewrite(x - a)
end

function test_ScalarAffineFunction()
    T = Int
    MA.Test.int_test(
        MOI.ScalarAffineFunction{T},
        exclude = ["int_mul", "int_add_mul"],
    )
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    a = T(2) * x + T(1)
    b = T(4) * y + T(2)
    c = T(3) * x - T(2) * y - T(3)
    d = T(3) * x - T(2) * y + T(4) * x
    e = T(5) * x - T(5)
    f = T(1) * y - T(2) * x + T(2)
    g = T(2) * x + T(3) * x
    _run_all_tests(T, a, b, c, d, e, f, g)
    return
end

function test_ScalarQuadraticFunction()
    T = Int
    MA.Test.int_test(
        MOI.ScalarQuadraticFunction{T},
        exclude = ["int_mul", "int_add_mul"],
    )
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    a = T(2) * x + T(1) + T(4) * x * y
    b = T(4) * y + T(3) * y * y - T(3) * y * x + T(2)
    c = T(2) * x * x + T(3) * x - T(2) * y - T(3)
    d = T(2) * x * x + T(3) * x - T(2) * y + T(4) * x - T(1) * y * y
    e = T(5) * x - T(5) - T(4) * x * y
    f = T(1) * y + T(2) * y * y - T(2) * x + T(2)
    g = T(2) * x + T(3) * x + T(3) * x * y
    _run_all_tests(T, a, b, c, d, e, f, g)
    return
end

end  # module

TestMutableArithmetics.runtests()
