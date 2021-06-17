module TestAttributes

using Test
using MathOptInterface
const MOI = MathOptInterface

include("dummy.jl")

function test_attributes_is_set_by_optimize()
    @test MOI.is_set_by_optimize(MOI.TerminationStatus())
    @test !MOI.is_set_by_optimize(MOI.ConstraintSet())
    @test !MOI.is_set_by_optimize(MOI.ObjectiveSense())
    @test MOI.is_set_by_optimize(MOI.CallbackNodeStatus(1))
end

function test_attributes_is_copyable()
    @test !MOI.is_copyable(MOI.TerminationStatus())
    @test !MOI.is_copyable(MOI.ConstraintSet())
    @test MOI.is_copyable(MOI.ObjectiveSense())
end

function test_attributes_supports()
    model = DummyModel()
    @test_throws ArgumentError MOI.supports(model, MOI.TerminationStatus())
    @test_throws ArgumentError begin
        MOI.supports(
            model,
            MOI.ConstraintSet(),
            MOI.ConstraintIndex{MOI.SingleVariable,MOI.EqualTo{Float64}},
        )
    end
    @test MOI.supports(model, MOI.ObjectiveSense())
end

function test_attributes_set_vector()
    attr = MOI.VariablePrimalStart()
    err = DimensionMismatch(
        "Number of indices (1) does not match the " *
        "number of values (2) set to `$attr`.",
    )
    model = DummyModel()
    x = MOI.VariableIndex(1)
    @test_throws err MOI.set(model, MOI.VariablePrimalStart(), [x], ones(2))
end

function test_attributes_integration_compute_conflict_1()
    optimizer = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    model = MOI.Utilities.CachingOptimizer(
        MOI.Utilities.Model{Float64}(),
        MOI.Bridges.full_bridge_optimizer(optimizer, Float64),
    )
    x = MOI.add_variable(model)
    c1 = MOI.add_constraint(model, MOI.SingleVariable(x), MOI.LessThan(0.0))
    c2 = MOI.add_constraint(model, MOI.SingleVariable(x), MOI.GreaterThan(1.0))
    MOI.optimize!(model)
    @test MOI.get(optimizer, MOI.ConflictStatus()) ==
          MOI.COMPUTE_CONFLICT_NOT_CALLED
    MOI.set(optimizer, MOI.ConflictStatus(), MOI.CONFLICT_FOUND)
    MOI.set(
        optimizer,
        MOI.ConstraintConflictStatus(),
        MOI.get(
            optimizer,
            MOI.ListOfConstraintIndices{
                MOI.SingleVariable,
                MOI.LessThan{Float64},
            }(),
        )[1],
        MOI.NOT_IN_CONFLICT,
    )
    MOI.set(
        optimizer,
        MOI.ConstraintConflictStatus(),
        MOI.get(
            optimizer,
            MOI.ListOfConstraintIndices{
                MOI.SingleVariable,
                MOI.GreaterThan{Float64},
            }(),
        )[1],
        MOI.IN_CONFLICT,
    )
    MOI.compute_conflict!(model)
    @test MOI.get(model, MOI.ConflictStatus()) == MOI.CONFLICT_FOUND
    @test MOI.get(model, MOI.ConstraintConflictStatus(), c1) ==
          MOI.NOT_IN_CONFLICT
    @test MOI.get(model, MOI.ConstraintConflictStatus(), c2) == MOI.IN_CONFLICT
end

MOI.Utilities.@model(
    OnlyScalarConstraints,
    (),
    (MOI.GreaterThan, MOI.LessThan),
    (),
    (),
    (),
    (MOI.ScalarAffineFunction,),
    (),
    ()
)

function test_attributes_integration_compute_conflict_2()
    optimizer = MOI.Utilities.MockOptimizer(OnlyScalarConstraints{Float64}())
    model = MOI.Bridges.full_bridge_optimizer(optimizer, Float64)
    x = MOI.add_variable(model)
    c = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0),
        MOI.Interval(0.0, 1.0),
    )
    MOI.optimize!(model)
    @test MOI.get(optimizer, MOI.ConflictStatus()) ==
          MOI.COMPUTE_CONFLICT_NOT_CALLED
    MOI.set(optimizer, MOI.ConflictStatus(), MOI.CONFLICT_FOUND)
    MOI.compute_conflict!(model)
    @test MOI.get(model, MOI.ConflictStatus()) == MOI.CONFLICT_FOUND
    @test_throws ArgumentError MOI.get(model, MOI.ConstraintConflictStatus(), c)
end

struct _NoConstraintName <: MOI.AbstractOptimizer end

function test_no_constraint_name()
    model = _NoConstraintName()
    @test_throws(
        MOI.SingleVariableConstraintNameError(),
        MOI.supports(
            model,
            MOI.ConstraintName(),
            MOI.ConstraintIndex{MOI.SingleVariable,MOI.ZeroOne},
        ),
    )
    @test_throws(
        MOI.SingleVariableConstraintNameError(),
        MOI.set(
            model,
            MOI.ConstraintName(),
            MOI.ConstraintIndex{MOI.SingleVariable,MOI.ZeroOne}(1),
            "name",
        ),
    )
end

function test_ConstraintBasisStatus_fallback()
    model = DummyModelWithAdd()
    c = MOI.ConstraintIndex{MOI.SingleVariable,MOI.EqualTo{Float64}}(1)
    @test_throws(
        ErrorException(
            "Querying the basis status of a `SingleVariable` constraint is " *
            "not supported. Use [`VariableBasisStatus`](@ref) instead.",
        ),
        MOI.get(model, MOI.ConstraintBasisStatus(), c),
    )
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

TestAttributes.runtests()
