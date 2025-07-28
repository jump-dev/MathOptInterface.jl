# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestMockOptimizer

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

# function test_optimizer_solve_no_result()
#     optimizer = MOIU.MockOptimizer(MOIU.Model{Float64}())

#     v1 = MOI.add_variable(optimizer)

#     # Load fake solution
#     MOI.set(optimizer, MOI.TerminationStatus(), MOI.INFEASIBLE)
#     MOI.optimize!(optimizer)
#     @test MOI.get(optimizer, MOI.TerminationStatus()) == MOI.INFEASIBLE

#     MOI.set(optimizer, MOI.ConflictStatus(), MOI.CONFLICT_FOUND)
#     MOI.compute_conflict!(optimizer)
#     @test MOI.get(optimizer, MOI.ConflictStatus()) == MOI.CONFLICT_FOUND
# end

function test_optimizer_solve_with_result()
    optimizer = MOIU.MockOptimizer(
        MOIU.Model{Float64}(),
        eval_objective_value = false,
        eval_variable_constraint_dual = false,
    )

    v = MOI.add_variables(optimizer, 2)
    c1 = MOI.add_constraint(optimizer, v[1], MOI.GreaterThan(1.0))
    soc = MOI.add_constraint(
        optimizer,
        MOI.VectorOfVariables(v),
        MOI.SecondOrderCone(2),
    )
    MOI.set(optimizer, MOI.ObjectiveFunction{MOI.VariableIndex}(), v[1])
    MOI.set(optimizer, MOI.ResultCount(), 1)
    @test_throws(
        ErrorException("No mock primal is set for variable `$(v[1])`."),
        MOI.get(optimizer, MOI.VariablePrimal(), v[1])
    )
    @test_throws(
        MOI.InvalidIndex(MOI.VariableIndex(-1)),
        MOI.get(optimizer, MOI.VariablePrimal(), MOI.VariableIndex(-1))
    )
    MOI.set(optimizer, MOI.ResultCount(), 0)
    err = MOI.ResultIndexBoundsError(MOI.VariablePrimal(1), 0)
    @test_throws err MOI.get(optimizer, MOI.VariablePrimal(), v[1])
    err = MOI.ResultIndexBoundsError(MOI.ConstraintDual(1), 0)
    @test_throws err MOI.get(optimizer, MOI.ConstraintDual(), c1)

    # Load fake solution
    # TODO: Provide a more compact API for this.
    MOI.set(optimizer, MOI.TerminationStatus(), MOI.OPTIMAL)
    MOI.set(optimizer, MOI.ObjectiveValue(), 1.0)
    MOI.set(optimizer, MOI.DualObjectiveValue(2), 2.0)
    MOI.set(optimizer, MOI.ResultCount(), 2)
    MOI.set(optimizer, MOI.PrimalStatus(), MOI.FEASIBLE_POINT)
    MOI.set(optimizer, MOI.DualStatus(2), MOI.FEASIBLE_POINT)
    MOI.set(optimizer, MOI.VariablePrimal(), v, [1.0, 2.0])
    MOI.set(optimizer, MOI.VariablePrimal(), v[1], 3.0)
    MOI.set(optimizer, MOI.ConstraintDual(2), c1, 5.9)
    MOI.set(optimizer, MOI.ConstraintDual(2), soc, [1.0, 2.0])

    MOI.optimize!(optimizer)
    @test MOI.get(optimizer, MOI.TerminationStatus()) == MOI.OPTIMAL
    @test MOI.get(optimizer, MOI.ResultCount()) == 2
    @test MOI.get(optimizer, MOI.ObjectiveValue()) == 1.0
    @test isnan(MOI.get(optimizer, MOI.ObjectiveValue(2)))
    optimizer.eval_objective_value = true
    @test MOI.get(optimizer, MOI.ObjectiveValue()) == 3.0
    @test_throws(
        ErrorException(
            "No mock primal is set for variable `$(v[1])` at result index `2`.",
        ),
        MOI.get(optimizer, MOI.ObjectiveValue(2))
    )
    @test_throws(
        ErrorException(
            "No mock dual is set for constraint `$c1` at result index `1`.",
        ),
        MOI.get(optimizer, MOI.DualObjectiveValue())
    )
    @test MOI.get(optimizer, MOI.DualObjectiveValue(2)) == 5.9
    @test MOI.get(optimizer, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
    @test MOI.get(optimizer, MOI.DualStatus(2)) == MOI.FEASIBLE_POINT
    @test MOI.get(optimizer, MOI.VariablePrimal(), v) == [3.0, 2.0]
    @test MOI.get(optimizer, MOI.VariablePrimal(), v[1]) == 3.0
    @test_throws(
        ErrorException(
            "No mock primal is set for variable `$(v[1])` at result index `2`.",
        ),
        MOI.get(optimizer, MOI.VariablePrimal(2), v[1])
    )
    @test MOI.get(optimizer, MOI.ConstraintPrimal(), c1) == 3.0
    @test MOI.get(optimizer, MOI.ConstraintPrimal(), soc) == [3.0, 2.0]
    @test_throws(
        ErrorException(
            "No mock primal is set for variable `$(v[1])` at result index `2`.",
        ),
        MOI.get(optimizer, MOI.ConstraintPrimal(2), c1),
    )
    @test_throws(
        ErrorException(
            "No mock primal is set for variable `$(v[1])` at result index `2`.",
        ),
        MOI.get(optimizer, MOI.ConstraintPrimal(2), soc)
    )
    @test MOI.get(optimizer, MOI.ConstraintDual(2), c1) == 5.9
    @test MOI.get(optimizer, MOI.ConstraintDual(2), soc) == [1.0, 2.0]
    @test_throws(
        ErrorException(
            "No mock dual is set for constraint `$c1` at result index `1`.",
        ),
        MOI.get(optimizer, MOI.ConstraintDual(1), c1)
    )
end

function test_CanonicalConstraintFunction()
    mock = MOIU.MockOptimizer(MOIU.Model{Int}())
    fx, fy = MOI.add_variables(mock, 2)
    cx = MOI.add_constraint(mock, fx, MOI.LessThan(0))
    c = MOI.add_constraint(mock, 1fx + fy, MOI.LessThan(1))
    @test MOIU.is_canonical(MOI.get(mock, MOI.ConstraintFunction(), cx))
    @test MOIU.is_canonical(
        MOI.get(mock, MOI.CanonicalConstraintFunction(), cx),
    )
    @test !MOIU.is_canonical(MOI.get(mock, MOI.ConstraintFunction(), c))
    func = MOI.get(mock, MOI.CanonicalConstraintFunction(), c)
    @test MOIU.is_canonical(func)
    # Check that indices have been xored
    for term in func.terms
        @test MOI.is_valid(mock, term.variable)
    end
end

function test_conflict_access()
    mock = MOIU.MockOptimizer(MOIU.Model{Int}())
    fx, fy = MOI.add_variables(mock, 2)
    cx = MOI.add_constraint(mock, fx, MOI.LessThan(0))
    c = MOI.add_constraint(mock, 1fx + fy, MOI.LessThan(1))
    MOI.set(mock, MOI.ConstraintConflictStatus(), cx, MOI.NOT_IN_CONFLICT)
    MOI.set(mock, MOI.ConstraintConflictStatus(), c, MOI.IN_CONFLICT)
    MOI.set(mock, MOI.ConflictCount(), 1)
    MOI.compute_conflict!(mock)
    @test MOI.get(mock, MOI.ConstraintConflictStatus(), cx) ==
          MOI.NOT_IN_CONFLICT
    @test MOI.get(mock, MOI.ConstraintConflictStatus(), c) == MOI.IN_CONFLICT
end

function test_MockVariableAttribute()
    mock = MOIU.MockOptimizer(MOIU.Model{Int}())
    x = MOI.add_variable(mock)
    MOI.set(mock, MOI.Utilities.MockVariableAttribute(), x, 1)
    @test MOI.get(mock, MOI.Utilities.MockVariableAttribute(), x) == 1
    return
end

function test_MockConstraintAttribute()
    mock = MOIU.MockOptimizer(MOIU.Model{Int}())
    x = MOI.add_variable(mock)
    c = MOI.add_constraint(mock, x, MOI.LessThan(0))
    @test MOI.supports(mock, MOI.Utilities.MockConstraintAttribute(), typeof(c))
    MOI.set(mock, MOI.Utilities.MockConstraintAttribute(), c, 1)
    @test MOI.get(mock, MOI.Utilities.MockConstraintAttribute(), c) == 1
    return
end

function test_DualObjectiveValue()
    mock =
        MOIU.MockOptimizer(MOIU.Model{Int}(); eval_dual_objective_value = false)
    @test isnan(MOI.get(mock, MOI.DualObjectiveValue()))
    return
end

function test_add_con_allowed_false()
    model = MOI.Utilities.MockOptimizer(
        MOI.Utilities.Model{Float64}();
        add_con_allowed = false,
    )
    x = MOI.add_variable(model)
    @test_throws(
        MOI.AddConstraintNotAllowed,
        MOI.add_constraint(model, x, MOI.ZeroOne()),
    )
    @test_throws(
        MOI.AddConstraintNotAllowed,
        MOI.add_constraint(model, 1.0 * x, MOI.EqualTo(1.0)),
    )
    return
end

function test_supports_names_false()
    model = MOI.Utilities.MockOptimizer(
        MOI.Utilities.Model{Float64}();
        supports_names = false,
    )
    @test_throws(
        MOI.UnsupportedAttribute{MOI.Name},
        MOI.set(model, MOI.Name(), "model"),
    )
    x = MOI.add_variable(model)
    @test_throws(
        MOI.UnsupportedAttribute{MOI.VariableName},
        MOI.set(model, MOI.VariableName(), x, "x"),
    )
    c = MOI.add_constraint(model, 1.0 * x, MOI.EqualTo(1.0))
    @test_throws(
        MOI.UnsupportedAttribute{MOI.ConstraintName},
        MOI.set(model, MOI.ConstraintName(), c, "c"),
    )
    return
end

function test_delete_allowed_false()
    model = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    model.delete_allowed = false
    x = MOI.add_variable(model)
    @test_throws(MOI.DeleteNotAllowed{MOI.VariableIndex}, MOI.delete(model, x))
    @test_throws(
        MOI.DeleteNotAllowed{MOI.VariableIndex},
        MOI.delete(model, [x]),
    )
    return
end

function test_modify_not_allowed()
    model = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    config = MOI.Test.Config(
        exclude = Any[MOI.ScalarCoefficientChange, MOI.VectorConstantChange],
    )
    MOI.Test.runtests(
        model,
        config,
        include = [
            r"^test_linear_integration$",
            r"^test_linear_integration_modification$",
            r"^test_linear_VectorAffineFunction$",
        ],
    )
    return
end

function test_get_fallback_constraint_dual()
    model = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    x = MOI.add_variables(model, 2)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    f = MOI.VectorOfVariables(x)
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    c = MOI.add_constraint(model, f, MOI.Nonnegatives(2))
    @test_throws(
        ErrorException(
            "Fallback getter for variable constraint dual does not support objective function of type $(MOI.VectorOfVariables). Please report this issue to the solver wrapper package.",
        ),
        MOI.Utilities.get_fallback(model, MOI.ConstraintDual(), c),
    )
    return
end

struct SetByOptimizeAttribute <: MOI.AbstractOptimizerAttribute end

MOI.is_set_by_optimize(::SetByOptimizeAttribute) = true

function test_is_set_by_optimize_optimizer_attribute()
    model = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    MOI.set(model, SetByOptimizeAttribute(), 1.23)
    @test MOI.get(model, SetByOptimizeAttribute()) == 1.23
    return
end

function test_empty_constructor()
    mock = MOI.Utilities.MockOptimizer(Int; supports_names = false)
    @test mock.supports_names == false
    @test isa(
        mock,
        MOI.Utilities.MockOptimizer{
            MOI.Utilities.UniversalFallback{MOI.Utilities.Model{Int}},
            Int,
        },
    )
    mock = MOI.Utilities.MockOptimizer()
    @test mock.supports_names
    @test isa(
        mock,
        MOI.Utilities.MockOptimizer{
            MOI.Utilities.UniversalFallback{MOI.Utilities.Model{Float64}},
            Float64,
        },
    )
    return
end

end  # module

TestMockOptimizer.runtests()
