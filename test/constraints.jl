# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConstraints

using Test

import MathOptInterface as MOI

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
    @test nothing === MOI.throw_if_scalar_and_constant_not_zero(x, S)
    func1 = one(T) * x + one(T)
    @test_throws MOI.ScalarFunctionConstantNotZero begin
        MOI.throw_if_scalar_and_constant_not_zero(func1, S)
    end
    func2 = one(T) * x
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
          "$(typeof(err)): Cannot add `VariableIndex`-in-`$(S2)` constraint " *
          "for variable $(x) as a `VariableIndex`-in-`$(S1)` constraint was " *
          "already set for this variable and both constraints set a lower bound."
    return
end

function test_UpperBoundAlreadySet_error()
    x = MOI.VariableIndex(1)
    S1 = MOI.GreaterThan{Int}
    S2 = MOI.Interval{Int}
    err = MOI.UpperBoundAlreadySet{S1,S2}(x)
    @test sprint(showerror, err) ==
          "$(typeof(err)): Cannot add `VariableIndex`-in-`$(S2)` constraint " *
          "for variable $(x) as a `VariableIndex`-in-`$(S1)` constraint was " *
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

function test_scalar_nonlinear_function_add_constraint()
    model = MOI.Utilities.Model{Float64}()
    x = MOI.add_variable(model)
    f = MOI.ScalarNonlinearFunction{Float64}(
        :+,
        Any[x, MOI.ScalarNonlinearFunction{Float64}(:sin, Any[x])],
    )
    c = MOI.add_constraint(model, f, MOI.EqualTo(0.0))
    @test isapprox(MOI.get(model, MOI.ConstraintFunction(), c), f)
    return
end

end

TestConstraints.runtests()
