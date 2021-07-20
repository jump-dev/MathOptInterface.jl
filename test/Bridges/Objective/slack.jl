module TestObjectiveMap

using Test

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

function test_SlackBridge_ObjectiveSense_error()
    inner = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    model = MOI.Bridges.Objective.Slack{Float64}(inner)
    x = MOI.add_variable(model)
    f = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.1, x)], 1.2)
    @test_throws(
        ErrorException(
            "Set `MOI.ObjectiveSense` before `MOI.ObjectiveFunction` when" *
            " using `MOI.Bridges.Objective.SlackBridge`.",
        ),
        MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f),
    )
    return
end

function test_SlackBridge_ObjectiveSense_modify()
    inner = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    model = MOI.Bridges.Objective.Slack{Float64}(inner)
    x = MOI.add_variable(model)
    f = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.1, x)], -1.2)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    @test_throws(
        ArgumentError,
        MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.FEASIBILITY_SENSE)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    @test MOI.get(model, MOI.ObjectiveFunction{typeof(f)}()) ≈ f
    return
end

function test_SlackBridge_ObjectiveFunction_modify()
    inner = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    model = MOI.Bridges.Objective.Slack{Float64}(inner)
    x = MOI.add_variable(model)
    f = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.1, x)], -1.2)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    @test MOI.get(model, MOI.ObjectiveFunction{typeof(f)}()) ≈ f
    g = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(-1.1, x)], 1.2)
    MOI.set(model, MOI.ObjectiveFunction{typeof(g)}(), g)
    @test MOI.get(model, MOI.ObjectiveFunction{typeof(g)}()) ≈ g
    return
end

function test_SlackBridge_get_ObjectiveFunction_MIN()
    inner = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    model = MOI.Bridges.Objective.Slack{Float64}(inner)
    x = MOI.add_variable(model)
    f = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.1, x)], -1.2)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    @test MOI.get(model, MOI.ObjectiveFunction{typeof(f)}()) ≈ f
    return
end

function test_SlackBridge_get_ObjectiveFunction_MAX()
    inner = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    model = MOI.Bridges.Objective.Slack{Float64}(inner)
    x = MOI.add_variable(model)
    f = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.1, x)], -1.2)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    @test MOI.get(model, MOI.ObjectiveFunction{typeof(f)}()) ≈ f
    return
end

function test_SlackBridge_get_ObjectiveSense()
    inner = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    model = MOI.Bridges.Objective.Slack{Float64}(inner)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MIN_SENSE
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MAX_SENSE
    return
end

function test_SlackBridge_NumberOfVariables()
    inner = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    model = MOI.Bridges.Objective.Slack{Float64}(inner)
    x = MOI.add_variable(model)
    f = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.1, x)], 1.2)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    @test MOI.get(model, MOI.NumberOfVariables()) == 1
    @test MOI.get(inner, MOI.NumberOfVariables()) == 2
    MOI.set(model, MOI.ObjectiveSense(), MOI.FEASIBILITY_SENSE)
    @test MOI.get(model, MOI.NumberOfVariables()) == 1
    @test MOI.get(inner, MOI.NumberOfVariables()) == 1
    return
end

function test_SlackBridge_ListOfModelAttributesSet()
    inner = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    model = MOI.Bridges.Objective.Slack{Float64}(inner)
    x = MOI.add_variable(model)
    f = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.1, x)], 1.2)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    attr = MOI.get(model, MOI.ListOfModelAttributesSet())
    @test length(attr) == 2
    @test MOI.ObjectiveSense() in attr
    @test MOI.ObjectiveFunction{typeof(f)}() in attr
    attr = MOI.get(inner, MOI.ListOfModelAttributesSet())
    @test length(attr) == 2
    @test MOI.ObjectiveSense() in attr
    @test MOI.ObjectiveFunction{MOI.SingleVariable}() in attr
    return
end

function test_SlackBridge_ListOfVariableIndices()
    inner = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    model = MOI.Bridges.Objective.Slack{Float64}(inner)
    x = MOI.add_variable(model)
    f = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.1, x)], 1.2)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    @test MOI.get(model, MOI.ListOfVariableIndices()) == [x]
    @test length(MOI.get(inner, MOI.ListOfVariableIndices())) == 2
    MOI.set(model, MOI.ObjectiveSense(), MOI.FEASIBILITY_SENSE)
    @test MOI.get(model, MOI.ListOfVariableIndices()) == [x]
    @test MOI.get(inner, MOI.ListOfVariableIndices()) == [x]
    return
end

function test_SlackBridge_ListOfConstraintTypesPresent()
    inner = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    model = MOI.Bridges.Objective.Slack{Float64}(inner)
    x = MOI.add_variable(model)
    f = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.1, x)], 1.2)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    @test MOI.get(model, MOI.ListOfConstraintTypesPresent()) == []
    @test MOI.get(inner, MOI.ListOfConstraintTypesPresent()) ==
          [(MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64})]
    MOI.set(model, MOI.ObjectiveSense(), MOI.FEASIBILITY_SENSE)
    @test MOI.get(model, MOI.ListOfConstraintTypesPresent()) == []
    @test MOI.get(inner, MOI.ListOfConstraintTypesPresent()) == []
    return
end

function test_SlackBridge_ListOfConstraintIndices()
    inner = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    model = MOI.Bridges.Objective.Slack{Float64}(inner)
    x = MOI.add_variable(model)
    f = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.1, x)], 1.2)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    attr = MOI.ListOfConstraintIndices{
        MOI.ScalarAffineFunction{Float64},
        MOI.GreaterThan{Float64},
    }()
    @test MOI.get(model, attr) == []
    @test length(MOI.get(inner, attr)) == 1
    attr = MOI.ListOfConstraintIndices{
        MOI.ScalarAffineFunction{Float64},
        MOI.LessThan{Float64},
    }()
    @test MOI.get(model, attr) == []
    @test length(MOI.get(inner, attr)) == 0
    MOI.set(model, MOI.ObjectiveSense(), MOI.FEASIBILITY_SENSE)
    @test MOI.get(model, attr) == []
    @test length(MOI.get(inner, attr)) == 0
    return
end

function test_SlackBridge_ObjectiveFunctionValue()
    inner = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    model = MOI.Bridges.Objective.Slack{Float64}(inner)
    x = MOI.add_variable(model)
    MOI.add_constraint(model, MOI.SingleVariable(x), MOI.GreaterThan(2.0))
    f = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.1, x)], -1.2)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    MOI.Utilities.set_mock_optimize!(
        inner,
        mock -> MOI.Utilities.mock_optimize!(mock, [2.0, 1.0]),
    )
    MOI.optimize!(model)
    @test MOI.get(model, MOI.ObjectiveValue()) ≈ 1.0
    return
end

end  # module

TestObjectiveMap.runtests()
