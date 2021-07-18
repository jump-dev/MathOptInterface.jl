module TestMockOptimizer

using Test
import MathOptInterface
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
    c1 = MOI.add_constraint(
        optimizer,
        MOI.SingleVariable(v[1]),
        MOI.GreaterThan(1.0),
    )
    soc = MOI.add_constraint(
        optimizer,
        MOI.VectorOfVariables(v),
        MOI.SecondOrderCone(2),
    )
    MOI.set(
        optimizer,
        MOI.ObjectiveFunction{MOI.SingleVariable}(),
        MOI.SingleVariable(v[1]),
    )
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
        @show MOI.get(optimizer, MOI.ConstraintPrimal(2), c1)
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
    fx, fy = MOI.SingleVariable.(MOI.add_variables(mock, 2))
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
    fx, fy = MOI.SingleVariable.(MOI.add_variables(mock, 2))
    cx = MOI.add_constraint(mock, fx, MOI.LessThan(0))
    c = MOI.add_constraint(mock, 1fx + fy, MOI.LessThan(1))
    MOI.set(mock, MOI.ConstraintConflictStatus(), cx, MOI.NOT_IN_CONFLICT)
    MOI.set(mock, MOI.ConstraintConflictStatus(), c, MOI.IN_CONFLICT)
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
    c = MOI.add_constraint(mock, MOI.SingleVariable(x), MOI.LessThan(0))
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

function test_mock_deprecated()
    mock = MOIU.MockOptimizer(MOIU.Model{Float64}())
    x = MOI.add_variable(mock)
    c = MOI.add_constraint(
        mock,
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0),
        MOI.EqualTo(2.0),
    )
    MOIU.set_mock_optimize!(
        mock,
        m -> MOI.Utilities.mock_optimize!(
            m,
            MOI.OPTIMAL,
            MOI.FEASIBLE_POINT,
            MOI.NO_SOLUTION,
            var_basis = [MOI.BASIC],
            con_basis = [
                (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                    [MOI.BASIC],
            ],
        ),
    )
    @test_logs (:warn,) (:warn,) MOI.optimize!(mock)
    return
end

end  # module

TestMockOptimizer.runtests()
