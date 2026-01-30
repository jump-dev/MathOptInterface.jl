# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestVariableParameter

using Test

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

function test_runtests()
    MOI.Bridges.runtests(
        MOI.Bridges.Variable.ParameterToEqualToBridge,
        """
        constrainedvariable: x in Parameter(2.0)
        minobjective: 1.0 * x + 2.0
        """,
        """
        constrainedvariable: x in EqualTo(2.0)
        minobjective: 1.0 * x + 2.0
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Variable.ParameterToEqualToBridge,
        """
        constrainedvariable: x in Parameter(-2.0)
        minobjective: 1.0 * x + 2.0
        """,
        """
        constrainedvariable: x in EqualTo(-2.0)
        minobjective: 1.0 * x + 2.0
        """,
    )
    return
end

function test_delete()
    inner = MOI.Utilities.Model{Float64}()
    model = MOI.Bridges.Variable.ParameterToEqualTo{Float64}(inner)
    x, ci = MOI.add_constrained_variable(model, MOI.Parameter(2.0))
    @test !MOI.is_empty(inner)
    MOI.delete(model, x)
    @test MOI.is_empty(inner)
    @test MOI.is_empty(model)
    x, ci = MOI.add_constrained_variable(model, MOI.Parameter(2.0))
    @test_throws MOI.DeleteNotAllowed MOI.delete(model, ci)
    return
end

function test_constraint_function()
    inner = MOI.Utilities.Model{Float64}()
    model = MOI.Bridges.Variable.ParameterToEqualTo{Float64}(inner)
    x, ci = MOI.add_constrained_variable(model, MOI.Parameter(2.0))
    y, bridge = first(model.map)
    @test y == x
    x_inner = MOI.get(model, MOI.ConstraintFunction(), bridge)
    @test MOI.get(inner, MOI.ListOfVariableIndices()) == [x_inner]
    return
end

function test_list_of_constraint_indices()
    inner = MOI.Utilities.Model{Float64}()
    model = MOI.Bridges.Variable.ParameterToEqualTo{Float64}(inner)
    x, ci = MOI.add_constrained_variable(model, MOI.Parameter(2.0))
    F, S = MOI.VariableIndex, MOI.EqualTo{Float64}
    @test isempty(MOI.get(model, MOI.ListOfConstraintIndices{F,S}()))
    return
end

function test_VariablePrimalStart()
    inner = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    model = MOI.Bridges.Variable.ParameterToEqualTo{Float64}(inner)
    x, ci = MOI.add_constrained_variable(model, MOI.Parameter(2.0))
    @test MOI.supports(model, MOI.VariablePrimalStart(), MOI.VariableIndex)
    MOI.set(model, MOI.VariablePrimalStart(), x, 2.0)
    @test MOI.get(model, MOI.VariablePrimalStart(), x) == 2.0
    MOI.set(model, MOI.VariablePrimalStart(), x, nothing)
    @test MOI.get(model, MOI.VariablePrimalStart(), x) === nothing
    return
end

function test_ConstraintPrimalStart()
    inner = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    model = MOI.Bridges.Variable.ParameterToEqualTo{Float64}(inner)
    x, ci = MOI.add_constrained_variable(model, MOI.Parameter(2.0))
    @test MOI.supports(model, MOI.ConstraintPrimalStart(), typeof(ci))
    MOI.set(model, MOI.ConstraintPrimalStart(), ci, 2.0)
    @test MOI.get(model, MOI.ConstraintPrimalStart(), ci) == 2.0
    MOI.set(model, MOI.ConstraintPrimalStart(), ci, nothing)
    @test MOI.get(model, MOI.ConstraintPrimalStart(), ci) === nothing
    return
end

function test_ConstraintDualStart()
    inner = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    model = MOI.Bridges.Variable.ParameterToEqualTo{Float64}(inner)
    x, ci = MOI.add_constrained_variable(model, MOI.Parameter(2.0))
    @test MOI.supports(model, MOI.ConstraintDualStart(), typeof(ci))
    MOI.set(model, MOI.ConstraintDualStart(), ci, 2.0)
    @test MOI.get(model, MOI.ConstraintDualStart(), ci) == 2.0
    MOI.set(model, MOI.ConstraintDualStart(), ci, nothing)
    @test MOI.get(model, MOI.ConstraintDualStart(), ci) === nothing
    return
end

end  # module

TestVariableParameter.runtests()
