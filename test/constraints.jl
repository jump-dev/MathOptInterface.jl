module TestConstraints

using Test
using MathOptInterface
const MOI = MathOptInterface

function _constant_not_zero_test(::Type{T}) where {T}
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
    @test nothing === MOI.throw_if_scalar_and_constant_not_zero(func, MOI.ZeroCone)
end

function test_constraints_ConstantNotZero()
    _constant_not_zero_test(Int)
    _constant_not_zero_test(Float64)
    return
end

function runtests()
    for name in names(@__MODULE__; all = true)
        if startswith("$name", "test_")
            @testset "$(name)" begin
                getfield(@__MODULE__, name)()
            end
        end
    end
end

end

TestConstraints.runtests()
