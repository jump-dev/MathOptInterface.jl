module TestConstraints

using Test

using MathOptInterface
const MOI = MathOptInterface

function runtests()
    for name in names(@__MODULE__; all = true)
        if startswith("$name", "test_")
            @testset "$(name)" begin
                getfield(@__MODULE__, name)()
            end
        end
    end
end

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
    @test nothing === MOI.throw_if_scalar_and_constant_not_zero(func, MOI.Zeros)
end

function test_constraints_ConstantNotZero()
    _constant_not_zero_test(Int)
    _constant_not_zero_test(Float64)
    return
end

function test_LowerBoundAlreadySet_error()
    x = MOI.VariableIndex(1)
    S1 = MOI.LessThan{Int}
    S2 = MOI.Interval{Int}
    err = MOI.LowerBoundAlreadySet{S1,S2}(x)
    @test sprint(showerror, err) ==
          "$(typeof(err)): Cannot add `SingleVariable`-in-`$(S2)` constraint " *
          "for variable $(x) as a `SingleVariable`-in-`$(S1)` constraint was " *
          "already set for this variable and both constraints set a lower bound."
    return
end

function test_UpperBoundAlreadySet_error()
    x = MOI.VariableIndex(1)
    S1 = MOI.GreaterThan{Int}
    S2 = MOI.Interval{Int}
    err = MOI.UpperBoundAlreadySet{S1,S2}(x)
    @test sprint(showerror, err) ==
          "$(typeof(err)): Cannot add `SingleVariable`-in-`$(S2)` constraint " *
          "for variable $(x) as a `SingleVariable`-in-`$(S1)` constraint was " *
          "already set for this variable and both constraints set an upper bound."
    return
end

function test_ScalarFunctionConstantNotZero_error()
    x = MOI.VariableIndex(1)
    T = Int
    F = MOI.ScalarAffineFunction{T}
    S = MOI.EqualTo{T}
    err = MOI.ScalarFunctionConstantNotZero{T,F,S}(one(T))
    @test sprint(showerror, err) ==
          "In `$F`-in-`$S` constraint: Constant $(one(T)) of the function is " *
          "not zero. The function constant should be moved to the set. You can " *
          "use `MOI.Utilities.normalize_and_add_constraint` which does this " *
          "automatically."
    return
end

end

TestConstraints.runtests()
