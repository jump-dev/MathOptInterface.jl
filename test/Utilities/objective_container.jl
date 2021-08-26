module TestObjectiveContainer

using Test

import MathOptInterface

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

function test_ObjectiveSense()
    o = MOI.Utilities.ObjectiveContainer{Float16}()
    @test MOI.get(o, MOI.ListOfModelAttributesSet()) == []
    @test MOI.supports(o, MOI.ObjectiveSense())
    for val in (MOI.MIN_SENSE, MOI.MAX_SENSE, MOI.FEASIBILITY_SENSE)
        MOI.set(o, MOI.ObjectiveSense(), val)
        @test MOI.get(o, MOI.ObjectiveSense()) == val
    end
    return
end

function test_FEASIBILITY_SENSE_clears_objective()
    o = MOI.Utilities.ObjectiveContainer{Float16}()
    x = MOI.VariableIndex(1234)
    f = x
    MOI.set(o, MOI.ObjectiveFunction{MOI.VariableIndex}(), f)
    @test MOI.get(o, MOI.ObjectiveFunction{MOI.VariableIndex}()) ≈ f
    MOI.set(o, MOI.ObjectiveSense(), MOI.FEASIBILITY_SENSE)
    @test !MOI.is_empty(o)
    @test o.is_function_set == false
    return
end

function _test_basic_objective(F, T)
    o = MOI.Utilities.ObjectiveContainer{T}()
    @test MOI.is_empty(o)
    @test MOI.supports(o, MOI.ObjectiveFunction{F}())
    x = MOI.VariableIndex(1234)
    f = convert(F, x)
    MOI.set(o, MOI.ObjectiveFunction{F}(), f)
    @test MOI.get(o, MOI.ObjectiveFunctionType()) == F
    @test !MOI.is_empty(o)
    @test MOI.get(o, MOI.ListOfModelAttributesSet()) ==
          Any[MOI.ObjectiveFunction{F}()]
    MOI.empty!(o)
    @test MOI.is_empty(o)
    return
end

function test_basic_objective()
    _test_basic_objective(MOI.VariableIndex, Float32)
    _test_basic_objective(MOI.ScalarAffineFunction{Float32}, Float32)
    _test_basic_objective(MOI.ScalarQuadraticFunction{Float32}, Float32)
    return
end

function test_delete_VariableIndex()
    o = MOI.Utilities.ObjectiveContainer{Float16}()
    x = MOI.VariableIndex(1234)
    f = x
    MOI.set(o, MOI.ObjectiveFunction{MOI.VariableIndex}(), f)
    MOI.delete(o, x)
    @test MOI.is_empty(o)
    return
end

function test_delete_ScalarAffineFunction()
    o = MOI.Utilities.ObjectiveContainer{Float16}()
    x = MOI.VariableIndex(1234)
    f = convert(MOI.ScalarAffineFunction{Float16}, x)
    MOI.set(o, MOI.ObjectiveFunction{typeof(f)}(), f)
    MOI.delete(o, x)
    @test MOI.get(o, MOI.ObjectiveFunctionType()) == typeof(f)
    @test !(MOI.get(o, MOI.ObjectiveFunction{typeof(f)}()) ≈ f)
    return
end

function test_delete_ScalarQuadraticFunction()
    o = MOI.Utilities.ObjectiveContainer{Float16}()
    x = MOI.VariableIndex(1234)
    f = convert(MOI.ScalarQuadraticFunction{Float16}, x)
    MOI.set(o, MOI.ObjectiveFunction{typeof(f)}(), f)
    MOI.delete(o, x)
    @test MOI.get(o, MOI.ObjectiveFunctionType()) == typeof(f)
    @test !(MOI.get(o, MOI.ObjectiveFunction{typeof(f)}()) ≈ f)
    return
end

function test_delete_VariableIndex_plural()
    o = MOI.Utilities.ObjectiveContainer{Float16}()
    x = MOI.VariableIndex(1234)
    f = x
    MOI.set(o, MOI.ObjectiveFunction{MOI.VariableIndex}(), f)
    MOI.delete(o, [x])
    @test MOI.is_empty(o)
    return
end

function test_delete_ScalarAffineFunction_plural()
    o = MOI.Utilities.ObjectiveContainer{Float16}()
    x = MOI.VariableIndex(1234)
    f = convert(MOI.ScalarAffineFunction{Float16}, x)
    MOI.set(o, MOI.ObjectiveFunction{typeof(f)}(), f)
    MOI.delete(o, [x])
    @test MOI.get(o, MOI.ObjectiveFunctionType()) == typeof(f)
    @test !(MOI.get(o, MOI.ObjectiveFunction{typeof(f)}()) ≈ f)
    return
end

function test_delete_ScalarQuadraticFunction_plural()
    o = MOI.Utilities.ObjectiveContainer{Float16}()
    x = MOI.VariableIndex(1234)
    f = convert(MOI.ScalarQuadraticFunction{Float16}, x)
    MOI.set(o, MOI.ObjectiveFunction{typeof(f)}(), f)
    MOI.delete(o, [x])
    @test MOI.get(o, MOI.ObjectiveFunctionType()) == typeof(f)
    @test !(MOI.get(o, MOI.ObjectiveFunction{typeof(f)}()) ≈ f)
    return
end

end  # module

TestObjectiveContainer.runtests()
