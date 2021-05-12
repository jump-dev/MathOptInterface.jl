using Test
using MathOptInterface
const MOI = MathOptInterface

function constant_not_zero_test(::Type{T}) where {T}
    S = MOI.EqualTo{T}
    x = MOI.VariableIndex(1)
    fx = MOI.SingleVariable(x)
    @test nothing === MOI.throw_if_scalar_and_constant_not_zero(fx, S)
    func1 = one(T) * fx + one(T)
    @test_throws MOI.ScalarFunctionConstantNotZero begin
        MOI.throw_if_scalar_and_constant_not_zero(func1, S)
    end
    func2 = one(T) * fx
    @test nothing === MOI.throw_if_scalar_and_constant_not_zero(func2, S)
    func = MOI.Utilities.operate(vcat, T, func1, func2)
    @test nothing === MOI.throw_if_scalar_and_constant_not_zero(func, MOI.Zeros)
end

@testset "Constant not zero" begin
    constant_not_zero_test(Int)
    constant_not_zero_test(Float64)
end
