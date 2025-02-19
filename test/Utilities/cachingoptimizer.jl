# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestCachingOptimizer

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

###
### Helper structs
###

struct DummyModelAttribute <: MOI.AbstractModelAttribute end

struct DummyEvaluator <: MOI.AbstractNLPEvaluator end

struct DummyVariableAttribute <: MOI.AbstractVariableAttribute end

struct DummyConstraintAttribute <: MOI.AbstractConstraintAttribute end

mutable struct NoFreeVariables <: MOI.AbstractOptimizer
    inner::MOIU.Model{Float64}
    NoFreeVariables() = new(MOIU.Model{Float64}())
end

MOI.is_empty(model::NoFreeVariables) = MOI.is_empty(model.inner)

MOI.empty!(model::NoFreeVariables) = MOI.empty!(model.inner)

function MOI.get(model::NoFreeVariables, attr::MOI.AnyAttribute, idx::Vector)
    return MOI.get(model.inner, attr, idx)
end

function MOI.get(model::NoFreeVariables, attr::MOI.AnyAttribute, args...)
    return MOI.get(model.inner, attr, args...)
end

function MOI.supports_add_constrained_variables(
    ::NoFreeVariables,
    ::Type{MOI.Reals},
)
    return false
end

function MOI.supports_add_constrained_variable(
    ::NoFreeVariables,
    ::Type{<:MOI.AbstractScalarSet},
)
    return true
end

function MOI.add_constrained_variable(
    model::NoFreeVariables,
    set::MOI.AbstractScalarSet,
)
    return MOI.add_constrained_variable(model.inner, set)
end

function MOI.supports_add_constrained_variables(
    ::NoFreeVariables,
    ::Type{<:MOI.AbstractVectorSet},
)
    return true
end

function MOI.add_constrained_variables(
    model::NoFreeVariables,
    set::MOI.AbstractVectorSet,
)
    return MOI.add_constrained_variables(model.inner, set)
end

MOI.supports_incremental_interface(::NoFreeVariables) = true

function MOI.copy_to(dest::NoFreeVariables, src::MOI.ModelLike; kwargs...)
    return MOI.Utilities.default_copy_to(dest, src; kwargs...)
end

###
### The tests
###

function test_MOI_Test()
    # It seems like these loops might take a while. But the first one takes
    # _forever_, and then the rest are really quick (like 140s vs 0.4s).
    for state in
        (MOIU.NO_OPTIMIZER, MOIU.EMPTY_OPTIMIZER, MOIU.ATTACHED_OPTIMIZER)
        for mode in (MOIU.MANUAL, MOIU.AUTOMATIC)
            model = MOIU.CachingOptimizer(MOIU.Model{Float64}(), mode)
            if state != MOIU.NO_OPTIMIZER
                optimizer = MOIU.MockOptimizer(
                    MOIU.Model{Float64}(),
                    supports_names = false,
                )
                MOIU.reset_optimizer(model, optimizer)
                if state == MOIU.ATTACHED_OPTIMIZER
                    MOIU.attach_optimizer(model)
                end
            end
            @test MOIU.state(model) == state
            @test MOIU.mode(model) == mode
            MOI.Test.runtests(
                model,
                MOI.Test.Config(exclude = Any[MOI.optimize!]),
                exclude = [
                    "test_attribute_SolverName",
                    "test_attribute_SolverVersion",
                ],
            )
        end
    end
    return
end

# !!! warning
#     This is some type piracy. To enable CachingOptimizer to pass some MOI.Test
#     functions with MockOptimizer, we overload `setup_test` to setup the inner
#     mock optimizer.
#
#     This is pretty fragile. It requires the inner optimizer to be attached,
#     amongst other things. It's used by `test_compute_conflict` below, but not
#     by test_MOI_Test above (test_MOI_Test has `exclude = Any[MOI.optimize!]`).
function MOI.Test.setup_test(
    f::Any,
    model::MOI.Utilities.CachingOptimizer{
        MOI.Utilities.MockOptimizer{
            MOI.Utilities.UniversalFallback{MOI.Utilities.Model{Float64}},
            Float64,
        },
        MOI.Utilities.Model{Float64},
    },
    config::MOI.Test.Config,
)
    MOI.Test.setup_test(f, model.optimizer, config)
    return
end

function test_compute_conflict()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    model = MOI.Utilities.CachingOptimizer(MOI.Utilities.Model{Float64}(), mock)
    MOI.Test.runtests(
        model,
        MOI.Test.Config(),
        include = ["test_solve_conflict"],
    )
    return
end

"""
Without an optimizer attached (that is, `MOI.state(model) == NO_OPTIMIZER`) we
need throw nice errors for attributes that are based on the optimizer. For
`AbstractModelAttribute`s that `is_set_by_optimize` returns `true` for, we
overload `TerminationStatus`, `PrimalStatus`, or `DualStatus` to return
sane default values. Otherwise we throw a nice error.
"""
function test_default_attributes()
    model = MOIU.CachingOptimizer(MOIU.Model{Float64}(), MOIU.MANUAL)
    @test MOIU.state(model) == MOIU.NO_OPTIMIZER

    @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
    @test MOI.get(model, MOI.PrimalStatus()) == MOI.NO_SOLUTION
    @test MOI.get(model, MOI.DualStatus()) == MOI.NO_SOLUTION
    x = MOI.add_variables(model, 2)
    @test_throws(
        MOI.GetAttributeNotAllowed{MOI.VariablePrimal},
        MOI.get(model, MOI.VariablePrimal(), x[1]),
    )
    @test_throws(
        MOI.GetAttributeNotAllowed{MOI.VariablePrimal},
        MOI.get(model, MOI.VariablePrimal(), x),
    )
    for attr in (
        MOI.SolverName(),
        MOI.Silent(),
        MOI.TimeLimitSec(),
        MOI.ObjectiveLimit(),
        MOI.NumberOfThreads(),
        MOI.ResultCount(),
    )
        @test_throws(
            MOI.GetAttributeNotAllowed{typeof(attr)},
            MOI.get(model, attr),
        )
    end
    return
end

function test_copyable_solver_attributes()
    cache = MOIU.UniversalFallback(MOIU.Model{Float64}())
    cached = MOIU.CachingOptimizer(cache, MOIU.MANUAL)
    MOI.set(cached, MOI.Silent(), true)
    MOI.set(cached, MOI.TimeLimitSec(), 0.0)
    MOI.set(cached, MOI.ObjectiveLimit(), 42.0)
    MOI.set(cached, MOI.NumberOfThreads(), 1)
    mock = MOIU.MockOptimizer(MOIU.UniversalFallback(MOIU.Model{Float64}()))
    MOIU.reset_optimizer(cached, mock)
    @test MOI.get(mock, MOI.Silent())
    @test MOI.get(cached, MOI.Silent())
    @test MOI.get(mock, MOI.TimeLimitSec()) == 0.0
    @test MOI.get(cached, MOI.TimeLimitSec()) == 0.0
    @test MOI.get(mock, MOI.ObjectiveLimit()) == 42.0
    @test MOI.get(cached, MOI.ObjectiveLimit()) == 42.0
    @test MOI.get(mock, MOI.NumberOfThreads()) == 1
    @test MOI.get(cached, MOI.NumberOfThreads()) == 1
    MOI.set(cached, MOI.Silent(), false)
    MOI.set(cached, MOI.TimeLimitSec(), 1.0)
    MOI.set(cached, MOI.ObjectiveLimit(), 1.0)
    MOI.set(cached, MOI.NumberOfThreads(), 2)
    @test !MOI.get(mock, MOI.Silent())
    @test !MOI.get(cached, MOI.Silent())
    @test MOI.get(mock, MOI.TimeLimitSec()) ≈ 1.0
    @test MOI.get(mock, MOI.ObjectiveLimit()) ≈ 1.0
    @test MOI.get(cached, MOI.TimeLimitSec()) ≈ 1.0
    @test MOI.get(cached, MOI.ObjectiveLimit()) ≈ 1.0
    @test MOI.get(mock, MOI.NumberOfThreads()) == 2
    @test MOI.get(cached, MOI.NumberOfThreads()) == 2
    mock = MOIU.MockOptimizer(MOIU.UniversalFallback(MOIU.Model{Float64}()))
    MOIU.reset_optimizer(cached, mock)
    @test !MOI.get(mock, MOI.Silent())
    @test !MOI.get(cached, MOI.Silent())
    @test MOI.get(mock, MOI.TimeLimitSec()) ≈ 1.0
    @test MOI.get(cached, MOI.TimeLimitSec()) ≈ 1.0
    @test MOI.get(mock, MOI.ObjectiveLimit()) ≈ 1.0
    @test MOI.get(cached, MOI.ObjectiveLimit()) ≈ 1.0
    @test MOI.get(mock, MOI.NumberOfThreads()) == 2
    @test MOI.get(cached, MOI.NumberOfThreads()) == 2
    MOI.set(cached, MOI.Silent(), true)
    MOI.set(cached, MOI.TimeLimitSec(), 0.0)
    MOI.set(cached, MOI.NumberOfThreads(), 1)
    @test MOI.get(mock, MOI.Silent())
    @test MOI.get(cached, MOI.Silent())
    @test MOI.get(mock, MOI.TimeLimitSec()) == 0.0
    @test MOI.get(cached, MOI.TimeLimitSec()) == 0.0
    @test MOI.get(mock, MOI.NumberOfThreads()) == 1
    @test MOI.get(cached, MOI.NumberOfThreads()) == 1
    mock = MOIU.MockOptimizer(MOIU.UniversalFallback(MOIU.Model{Float64}()))
    MOIU.reset_optimizer(cached, mock)
    @test MOI.get(mock, MOI.Silent())
    @test MOI.get(cached, MOI.Silent())
    @test MOI.get(mock, MOI.TimeLimitSec()) == 0.0
    @test MOI.get(cached, MOI.TimeLimitSec()) == 0.0
    @test MOI.get(mock, MOI.NumberOfThreads()) == 1
    @test MOI.get(cached, MOI.NumberOfThreads()) == 1
    return
end

function test_mapping_of_variables()
    mock = MOIU.MockOptimizer(MOIU.UniversalFallback(MOIU.Model{Float64}()))
    model = MOIU.CachingOptimizer(
        MOIU.UniversalFallback(MOIU.Model{Float64}()),
        mock,
    )
    MOIU.attach_optimizer(model)
    x = MOI.add_variable(model)
    y = first(MOI.get(mock, MOI.ListOfVariableIndices()))
    @test x != y # Otherwise, these tests will trivially pass
    @test !MOI.is_valid(model, y)
    @test !MOI.is_valid(mock, x)
    cx = MOI.add_constraint(model, x, MOI.GreaterThan(1.0))
    cy = first(
        MOI.get(
            mock,
            MOI.ListOfConstraintIndices{
                MOI.VariableIndex,
                MOI.GreaterThan{Float64},
            }(),
        ),
    )
    @test !MOI.is_valid(model, cy)
    @test !MOI.is_valid(mock, cx)
    @test MOI.get(mock, MOI.ConstraintFunction(), cy) == y

    c2x = MOI.add_constraint(model, 2.0x, MOI.GreaterThan(1.0))
    c2y = first(
        MOI.get(
            mock,
            MOI.ListOfConstraintIndices{
                MOI.ScalarAffineFunction{Float64},
                MOI.GreaterThan{Float64},
            }(),
        ),
    )
    @test !MOI.is_valid(model, c2y)
    @test !MOI.is_valid(mock, c2x)
    @test MOI.get(model, MOI.ConstraintFunction(), c2x) ≈ 2.0x
    @test MOI.get(model, MOI.ConstraintSet(), c2x) == MOI.GreaterThan(1.0)
    @test MOI.get(mock, MOI.ConstraintFunction(), c2y) ≈ 2.0y
    @test MOI.get(mock, MOI.ConstraintSet(), c2y) == MOI.GreaterThan(1.0)

    MOI.set(model, MOI.ConstraintSet(), c2x, MOI.GreaterThan(2.0))
    @test MOI.get(model, MOI.ConstraintFunction(), c2x) ≈ 2.0x
    @test MOI.get(model, MOI.ConstraintSet(), c2x) == MOI.GreaterThan(2.0)
    @test MOI.get(mock, MOI.ConstraintFunction(), c2y) ≈ 2.0y
    @test MOI.get(mock, MOI.ConstraintSet(), c2y) == MOI.GreaterThan(2.0)

    MOI.set(model, MOI.ConstraintFunction(), c2x, 3.0x)
    @test MOI.get(model, MOI.ConstraintFunction(), c2x) ≈ 3.0x
    @test MOI.get(model, MOI.ConstraintSet(), c2x) == MOI.GreaterThan(2.0)
    @test MOI.get(mock, MOI.ConstraintFunction(), c2y) ≈ 3.0y
    @test MOI.get(mock, MOI.ConstraintSet(), c2y) == MOI.GreaterThan(2.0)

    MOI.set(model, MOI.ConstraintSet(), c2x, MOI.GreaterThan(4.0))
    @test MOI.get(model, MOI.ConstraintFunction(), c2x) ≈ 3.0x
    @test MOI.get(model, MOI.ConstraintSet(), c2x) == MOI.GreaterThan(4.0)
    @test MOI.get(mock, MOI.ConstraintFunction(), c2y) ≈ 3.0y
    @test MOI.get(mock, MOI.ConstraintSet(), c2y) == MOI.GreaterThan(4.0)

    MOI.set(model, MOI.ObjectiveFunction{typeof(x)}(), x)
    @test MOI.get(mock, MOI.ObjectiveFunction{typeof(y)}()) == y

    MOI.set(model, MOI.ObjectiveFunction{typeof(2.0x)}(), 2.0x)
    @test MOI.get(mock, MOI.ObjectiveFunction{typeof(2.0y)}()) ≈ 2.0y

    MOI.set(model, DummyModelAttribute(), 2.0x + 1.0)
    @test MOI.get(model, DummyModelAttribute()) ≈ 2.0x + 1.0
    @test MOI.get(mock, DummyModelAttribute()) ≈ 2.0y + 1.0

    for (attr, cache_index, optimizer_index) in
        [(DummyVariableAttribute(), x, y), (DummyConstraintAttribute(), cx, cy)]
        MOI.set(model, attr, cache_index, 1.0x)
        @test MOI.get(model, attr, cache_index) ≈ 1.0x
        @test MOI.get(mock, attr, optimizer_index) ≈ 1.0y

        MOI.set(model, attr, [cache_index], [3.0x])
        @test MOI.get(model, attr, [cache_index])[1] ≈ 3.0x
        @test MOI.get(mock, attr, [optimizer_index])[1] ≈ 3.0y
    end

    @testset "RawSolver" begin
        MOI.get(model, MOI.RawSolver()) === mock
    end

    @testset "HeuristicCallback" begin
        attr = MOI.HeuristicCallback()
        f(callback_data) = nothing
        MOI.set(model, attr, f)
        @test MOI.get(model, attr) === f
    end

    @testset "CallbackVariablePrimal" begin
        attr = MOI.CallbackVariablePrimal(nothing)
        err =
            ErrorException("No mock callback primal is set for variable `$y`.")
        @test_throws err MOI.get(model, attr, x)
        MOI.set(mock, attr, y, 1.0)
        @test_throws MOI.InvalidIndex(x) MOI.get(mock, attr, x)
        @test MOI.get(mock, attr, y) == 1.0
        @test MOI.get(model, attr, x) == 1.0
    end

    @testset "LazyConstraint" begin
        sub = MOI.LazyConstraint(nothing)
        @test MOI.supports(model, sub)
        MOI.submit(model, sub, 2.0x, MOI.GreaterThan(1.0))
        @test mock.submitted[sub][1][1] ≈ 2.0y
        @test mock.submitted[sub][1][2] == MOI.GreaterThan(1.0)
    end
    @testset "HeuristicSolution" begin
        sub = MOI.HeuristicSolution(nothing)
        @test MOI.supports(model, sub)
        MOI.submit(model, sub, [x], [1.0])
        @test mock.submitted[sub][1][1] == [y]
        @test mock.submitted[sub][1][2] == [1.0]
    end

    nlp_data =
        MOI.NLPBlockData([MOI.NLPBoundsPair(1.0, 2.0)], DummyEvaluator(), false)
    MOI.set(model, MOI.NLPBlock(), nlp_data)
    for nlp_data in
        [MOI.get(model, MOI.NLPBlock()), MOI.get(mock, MOI.NLPBlock())]
        @test nlp_data.constraint_bounds == [MOI.NLPBoundsPair(1.0, 2.0)]
        @test nlp_data.evaluator isa DummyEvaluator
        @test nlp_data.has_objective == false
    end
    return
end

function test_CachingOptimizer_MANUAL_mode()
    m = MOIU.CachingOptimizer(MOIU.Model{Float64}(), MOIU.MANUAL)
    @test MOIU.state(m) == MOIU.NO_OPTIMIZER

    s = MOIU.MockOptimizer(MOIU.Model{Float64}(), supports_names = false)
    @test MOI.is_empty(s)
    MOIU.reset_optimizer(m, s)
    @test MOIU.state(m) == MOIU.EMPTY_OPTIMIZER

    v = MOI.add_variable(m)
    x = MOI.add_variables(m, 2)
    saf = MOI.ScalarAffineFunction(
        MOI.ScalarAffineTerm.([1.0, 2.0, 3.0], [v; x]),
        0.0,
    )
    @test MOI.supports(m, MOI.ObjectiveFunction{typeof(saf)}())
    MOI.set(m, MOI.ObjectiveFunction{typeof(saf)}(), saf)
    @test MOI.get(
        m,
        MOIU.AttributeFromModelCache(MOI.ObjectiveFunction{typeof(saf)}()),
    ) ≈ saf
    @test MOI.get(m, MOI.ObjectiveFunction{typeof(saf)}()) ≈ saf

    @test_throws AssertionError MOI.optimize!(m)

    MOIU.attach_optimizer(m)
    @test MOIU.state(m) == MOIU.ATTACHED_OPTIMIZER
    @test MOI.get(
        m,
        MOIU.AttributeFromOptimizer(MOI.ObjectiveFunction{typeof(saf)}()),
    ) ≈ saf

    @test MOI.supports(m, MOI.ObjectiveSense())
    MOI.set(m, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    @test MOI.get(m, MOI.ObjectiveSense()) == MOI.MAX_SENSE
    @test MOI.get(m, MOIU.AttributeFromModelCache(MOI.ObjectiveSense())) ==
          MOI.MAX_SENSE
    @test MOI.get(m, MOIU.AttributeFromOptimizer(MOI.ObjectiveSense())) ==
          MOI.MAX_SENSE

    @test MOI.supports(
        m,
        MOIU.AttributeFromOptimizer(MOIU.MockModelAttribute()),
    )
    MOI.set(m, MOIU.AttributeFromOptimizer(MOIU.MockModelAttribute()), 10)
    @test MOI.get(m, MOIU.AttributeFromOptimizer(MOIU.MockModelAttribute())) ==
          10

    MOI.set(m, MOIU.AttributeFromOptimizer(MOI.ResultCount()), 1)
    @test MOI.supports(
        m,
        MOIU.AttributeFromOptimizer(MOI.VariablePrimal()),
        typeof(v),
    )
    MOI.set(m, MOIU.AttributeFromOptimizer(MOI.VariablePrimal()), v, 3.0)
    MOI.set(s, MOI.TerminationStatus(), MOI.OPTIMAL)
    MOI.optimize!(m)

    @test MOI.get(m, MOI.VariablePrimal(), v) == 3.0
    @test MOI.get(m, MOI.VariablePrimal(), [v]) == [3.0]
    @test MOI.get(m, MOIU.AttributeFromOptimizer(MOI.VariablePrimal()), v) ==
          3.0

    @test MOI.supports_constraint(
        m.model_cache,
        MOI.VariableIndex,
        MOI.LessThan{Float64},
    )
    @test MOI.supports_constraint(
        m.optimizer.inner_model,
        MOI.VariableIndex,
        MOI.LessThan{Float64},
    )
    @test MOI.supports_constraint(
        m.optimizer,
        MOI.VariableIndex,
        MOI.LessThan{Float64},
    )
    @test MOI.supports_constraint(m, MOI.VariableIndex, MOI.LessThan{Float64})
    lb = MOI.add_constraint(m, v, MOI.LessThan(10.0))
    MOI.set(m, MOI.ConstraintSet(), lb, MOI.LessThan(11.0))
    @test MOI.get(m, MOI.ConstraintSet(), lb) == MOI.LessThan(11.0)
    @test MOI.get(m, MOI.ConstraintFunction(), lb) == v

    MOIU.drop_optimizer(m)
    @test MOIU.state(m) == MOIU.NO_OPTIMIZER

    MOI.set(m, MOI.ConstraintSet(), lb, MOI.LessThan(12.0))
    @test MOI.get(m, MOI.ConstraintSet(), lb) == MOI.LessThan(12.0)

    MOI.delete(m, x[2])
    @test_throws MOI.InvalidIndex{typeof(x[2])} MOI.delete(m, x[2])
    @test !MOI.is_valid(m, x[2])

    # TODO: test more constraint modifications

    show_str = sprint(show, m)
    @test occursin("NO_OPTIMIZER", show_str)
    @test occursin("MANUAL", show_str)

    MOI.empty!(s)
    MOIU.reset_optimizer(m, s)
    @test MOIU.state(m) == MOIU.EMPTY_OPTIMIZER
    MOIU.attach_optimizer(m)
    @test MOIU.state(m) == MOIU.ATTACHED_OPTIMIZER
    MOI.empty!(m)
    @test MOIU.state(m) == MOIU.EMPTY_OPTIMIZER
end

function test_CachingOptimizer_AUTOMATIC_mode()
    m = MOIU.CachingOptimizer(MOIU.Model{Float64}(), MOIU.AUTOMATIC)
    @test MOIU.state(m) == MOIU.NO_OPTIMIZER

    v = MOI.add_variable(m)
    @test MOI.supports(m, MOI.VariableName(), typeof(v))
    MOI.set(m, MOI.VariableName(), v, "v")
    @test MOI.get(m, MOI.VariableName(), v) == "v"

    s = MOIU.MockOptimizer(MOIU.Model{Float64}(), supports_names = false)
    @test MOI.is_empty(s)
    MOIU.reset_optimizer(m, s)
    @test MOIU.state(m) == MOIU.EMPTY_OPTIMIZER

    saf = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, v)], 0.0)
    @test MOI.supports(m, MOI.ObjectiveFunction{typeof(saf)}())
    MOI.set(m, MOI.ObjectiveFunction{typeof(saf)}(), saf)
    @test MOI.get(
        m,
        MOIU.AttributeFromModelCache(MOI.ObjectiveFunction{typeof(saf)}()),
    ) ≈ saf

    MOI.optimize!(m)
    @test MOIU.state(m) == MOIU.ATTACHED_OPTIMIZER
    @test MOI.get(m, MOIU.AttributeFromOptimizer(MOI.TerminationStatus())) ==
          MOI.OPTIMIZE_NOT_CALLED

    @test MOI.get(m, MOI.VariableName(), v) == "v"
    @test MOI.get(m, MOIU.AttributeFromModelCache(MOI.VariableName()), v) == "v"
    # The caching optimizer does not copy names
    @test MOI.get(m, MOIU.AttributeFromOptimizer(MOI.VariableName()), v) == ""

    @test MOI.supports(m, MOI.ObjectiveSense())
    MOI.set(m, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    @test MOI.get(m, MOIU.AttributeFromModelCache(MOI.ObjectiveSense())) ==
          MOI.MAX_SENSE
    @test MOI.get(m, MOIU.AttributeFromOptimizer(MOI.ObjectiveSense())) ==
          MOI.MAX_SENSE

    @test MOI.supports(
        m,
        MOIU.AttributeFromOptimizer(MOIU.MockModelAttribute()),
    )
    MOI.set(m, MOIU.AttributeFromOptimizer(MOIU.MockModelAttribute()), 10)
    @test MOI.get(m, MOIU.AttributeFromOptimizer(MOIU.MockModelAttribute())) ==
          10

    MOI.set(m, MOIU.AttributeFromOptimizer(MOI.ResultCount()), 1)
    @test MOI.supports(
        m,
        MOIU.AttributeFromOptimizer(MOI.VariablePrimal()),
        typeof(v),
    )
    MOI.set(m, MOIU.AttributeFromOptimizer(MOI.VariablePrimal()), v, 3.0)

    MOI.optimize!(m)

    @test MOI.get(m, MOIU.AttributeFromOptimizer(MOI.VariablePrimal()), v) ==
          3.0

    @testset "Modify not allowed" begin
        s.modify_allowed = false
        MOI.modify(
            m,
            MOI.ObjectiveFunction{typeof(saf)}(),
            MOI.ScalarConstantChange(1.0),
        )
        s.modify_allowed = true
        @test MOIU.state(m) == MOIU.EMPTY_OPTIMIZER
        MOIU.attach_optimizer(m)
        @test MOIU.state(m) == MOIU.ATTACHED_OPTIMIZER
        ci = MOI.add_constraint(m, saf, MOI.EqualTo(0.0))
        s.modify_allowed = false
        MOI.modify(m, ci, MOI.ScalarCoefficientChange(v, 1.0))
        s.modify_allowed = true
        @test MOIU.state(m) == MOIU.EMPTY_OPTIMIZER
        MOIU.attach_optimizer(m)
        @test MOIU.state(m) == MOIU.ATTACHED_OPTIMIZER
    end

    # Simulate that constraints cannot be added
    @testset "Add constraint not allowed" begin
        s.add_con_allowed = false
        MOI.add_constraint(
            m,
            MOI.VectorOfVariables([v]),
            MOI.SecondOrderCone(1),
        )
        s.add_con_allowed = true
        @test MOIU.state(m) == MOIU.EMPTY_OPTIMIZER
        MOI.empty!(m)
        @test MOIU.state(m) == MOIU.EMPTY_OPTIMIZER
    end

    @testset "Add variable not allowed" begin
        s.add_var_allowed = false # Simulate optimizer that cannot add variables incrementally
        MOI.add_variable(m)
        @test MOIU.state(m) == MOIU.EMPTY_OPTIMIZER
        @test_throws MOI.AddVariableNotAllowed MOIU.attach_optimizer(m)
        @test MOIU.state(m) == MOIU.EMPTY_OPTIMIZER

        s.add_var_allowed = true
        MOIU.attach_optimizer(m)
        @test MOIU.state(m) == MOIU.ATTACHED_OPTIMIZER
        s.add_var_allowed = false
        MOI.add_variables(m, 2)
        s.add_var_allowed = true
        @test MOIU.state(m) == MOIU.EMPTY_OPTIMIZER
    end

    @testset "Delete not allowed" begin
        vi = MOI.add_variable(m)
        s.delete_allowed = false # Simulate optimizer that cannot delete variable
        MOI.delete(m, vi)
        s.delete_allowed = true
        @test MOIU.state(m) == MOIU.EMPTY_OPTIMIZER
        MOIU.attach_optimizer(m)
        @test MOIU.state(m) == MOIU.ATTACHED_OPTIMIZER

        vi = MOI.add_variable(m)
        ci = MOI.add_constraint(m, vi, MOI.EqualTo(0.0))
        s.delete_allowed = false # Simulate optimizer that cannot delete constraint
        MOI.delete(m, ci)
        s.delete_allowed = true
        @test MOIU.state(m) == MOIU.EMPTY_OPTIMIZER
        MOIU.attach_optimizer(m)
        @test MOIU.state(m) == MOIU.ATTACHED_OPTIMIZER
    end
    @test occursin("AUTOMATIC", sprint(show, m))
    MOI.empty!(m)
    @test MOIU.state(m) == MOIU.EMPTY_OPTIMIZER
    return
end

function test_empty_model_and_optimizer()
    s = MOIU.MockOptimizer(MOIU.Model{Float64}(), supports_names = false)
    model = MOIU.Model{Float64}()
    m = MOIU.CachingOptimizer(model, s)
    @test m isa MOIU.CachingOptimizer{typeof(s),typeof(model)}
    @test MOI.is_empty(m)
    @test MOIU.state(m) == MOIU.EMPTY_OPTIMIZER
    @test MOIU.mode(m) == MOIU.AUTOMATIC
    @test MOI.get(m, MOI.SolverName()) == "Mock"
    @test occursin("EMPTY_OPTIMIZER", sprint(show, m))
    return
end

function test_empty_model_nonempty_optimizer()
    s = MOIU.MockOptimizer(MOIU.Model{Float64}(), supports_names = false)
    MOI.add_variable(s)
    model = MOIU.Model{Float64}()
    @test MOI.is_empty(model)
    @test !MOI.is_empty(s)
    @test_throws AssertionError MOIU.CachingOptimizer(model, s)
    return
end

function test_nonempty_model()
    s = MOIU.MockOptimizer(MOIU.Model{Float64}(), supports_names = false)
    model = MOIU.Model{Float64}()
    MOI.add_variable(model)
    @test !MOI.is_empty(model)
    @test MOI.is_empty(s)
    @test MOIU.CachingOptimizer(model, s) isa MOIU.CachingOptimizer
    return
end

function _constrained_variables_test(model)
    @test !MOI.supports_add_constrained_variables(model, MOI.Reals)
    @test MOI.supports_add_constrained_variable(model, MOI.ZeroOne)
    @test !MOI.supports_constraint(model, MOI.VariableIndex, MOI.ZeroOne)
    @test MOI.supports_add_constrained_variables(model, MOI.Nonnegatives)
    @test !MOI.supports_constraint(
        model,
        MOI.VectorOfVariables,
        MOI.Nonnegatives,
    )
    scalar_set = MOI.ZeroOne()
    x, cx = MOI.add_constrained_variable(model, scalar_set)
    vector_set = MOI.Nonnegatives(2)
    y, cy = MOI.add_constrained_variables(model, vector_set)
    constraint_types = Set([
        (MOI.VariableIndex, MOI.ZeroOne),
        (MOI.VectorOfVariables, MOI.Nonnegatives),
    ])
    @test Set(MOI.get(model.model_cache, MOI.ListOfConstraintTypesPresent())) ==
          constraint_types
    if MOIU.state(model) == MOIU.EMPTY_OPTIMIZER
        MOIU.attach_optimizer(model)
    end
    @test Set(MOI.get(model.optimizer, MOI.ListOfConstraintTypesPresent())) ==
          constraint_types
    return
end

function test_constrained_variables()
    cache = NoFreeVariables()
    optimizer = NoFreeVariables()
    model = MOIU.CachingOptimizer(cache, optimizer)
    _constrained_variables_test(model)
    return
end

function test_constrained_variables_AUTOMATIC()
    cache = NoFreeVariables()
    optimizer = NoFreeVariables()
    model = MOIU.CachingOptimizer(cache, MOIU.AUTOMATIC)
    MOIU.reset_optimizer(model, optimizer)
    _constrained_variables_test(model)
    return
end

struct Issue1220 <: MOI.AbstractOptimizer
    optimizer_attributes::Dict{Any,Any}
    Issue1220() = new(Dict{Any,Any}())
end

MOI.is_empty(model::Issue1220) = isempty(model.optimizer_attributes)

function MOI.get(model::Issue1220, ::MOI.ListOfOptimizerAttributesSet)
    return collect(keys(model.optimizer_attributes))
end

MOI.supports(::Issue1220, ::MOI.AbstractOptimizerAttribute) = true

MOI.supports(::Issue1220, ::MOI.NumberOfThreads) = false

function MOI.get(model::Issue1220, attr::MOI.AbstractOptimizerAttribute)
    return model.optimizer_attributes[attr]
end

function MOI.set(model::Issue1220, attr::MOI.AbstractOptimizerAttribute, value)
    model.optimizer_attributes[attr] = value
    return
end

function test_issue1220_dont_pass_raw_parameter()
    model = MOIU.CachingOptimizer(Issue1220(), Issue1220())
    MOI.set(model, MOI.Silent(), true)
    MOI.set(model, MOI.RawOptimizerAttribute("foo"), "bar")
    MOI.set(model, MOI.NumberOfThreads(), 1)
    MOIU.reset_optimizer(model, Issue1220())
    @test MOI.get(model, MOI.Silent()) == true
    @test_throws KeyError MOI.get(model, MOI.RawOptimizerAttribute("foo"))
    @test_throws KeyError MOI.get(model, MOI.NumberOfThreads())
    return
end

function test_status_codes()
    optimizer = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    model = MOI.Utilities.CachingOptimizer(
        MOI.Utilities.Model{Float64}(),
        optimizer,
    )
    MOI.Utilities.attach_optimizer(model)
    MOI.set(optimizer, MOI.TerminationStatus(), MOI.OPTIMAL)
    MOI.set(optimizer, MOI.PrimalStatus(), MOI.FEASIBLE_POINT)
    @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMAL
    @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
    return
end

mutable struct CopyToAndOptimizer <: MOI.AbstractOptimizer
    is_dirty::Bool
end

function MOI.empty!(x::CopyToAndOptimizer)
    x.is_dirty = false
    return
end

MOI.is_empty(x::CopyToAndOptimizer) = !x.is_dirty

function MOI.optimize!(x::CopyToAndOptimizer, ::MOI.ModelLike)
    x.is_dirty = true
    return MOI.Utilities.IndexMap(), false
end

function test_copy_to_and_optimize!()
    optimizer = CopyToAndOptimizer(false)
    model = MOI.Utilities.CachingOptimizer(
        MOI.Utilities.Model{Float64}(),
        optimizer,
    )
    MOI.optimize!(model)
    @test MOI.Utilities.state(model) == MOI.Utilities.EMPTY_OPTIMIZER
    @test !MOI.is_empty(optimizer)
    MOI.empty!(model)
    @test MOI.is_empty(optimizer)
    @test MOI.Utilities.state(model) == MOI.Utilities.EMPTY_OPTIMIZER
    return
end

###
### Test get_fallback in CachingOptimizer.
###
### It's a bit complicated because we need an optimizer with the minimal set of
### methods needed to test the fallbacks.
###

struct _GetFallbackModel1310 <: MOI.AbstractOptimizer end

MOI.is_empty(::_GetFallbackModel1310) = true

MOI.get(::_GetFallbackModel1310, ::MOI.ListOfModelAttributesSet) = []

MOI.get(::_GetFallbackModel1310, ::MOI.PrimalStatus) = MOI.FEASIBLE_POINT

MOI.get(::_GetFallbackModel1310, ::MOI.DualStatus) = MOI.FEASIBLE_POINT

MOI.get(::_GetFallbackModel1310, ::MOI.TerminationStatus) = MOI.OTHER_ERROR

MOI.get(::_GetFallbackModel1310, ::MOI.ResultCount) = 1

function MOI.optimize!(::_GetFallbackModel1310, model::MOI.ModelLike)
    index_map = MOI.IndexMap()
    for x in MOI.get(model, MOI.ListOfVariableIndices())
        index_map[x] = x
    end
    for (F, S) in MOI.get(model, MOI.ListOfConstraintTypesPresent())
        for ci in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
            index_map[ci] = ci
        end
    end
    return index_map, false
end

function MOI.get(
    ::_GetFallbackModel1310,
    ::MOI.VariablePrimal,
    ::MOI.VariableIndex,
)
    return 1.2
end

function test_ConstraintPrimal_fallback()
    model = MOI.Utilities.CachingOptimizer(
        MOI.Utilities.Model{Float64}(),
        _GetFallbackModel1310(),
    )
    x = MOI.add_variable(model)
    c = MOI.add_constraint(model, x, MOI.GreaterThan(1.0))
    MOI.optimize!(model)
    @test MOI.get(model, MOI.ConstraintPrimal(), c) == 1.2
    @test MOI.get(model, MOI.ConstraintPrimal(), [c]) == [1.2]
    @test_throws(
        MOI.ResultIndexBoundsError(MOI.ConstraintPrimal(2), 1),
        MOI.get(model, MOI.ConstraintPrimal(2), c),
    )
    @test_throws(
        MOI.ResultIndexBoundsError(MOI.ConstraintPrimal(2), 1),
        MOI.get(model, MOI.ConstraintPrimal(2), [c]),
    )
    return
end

function test_ConstraintDual_variable_fallback()
    model = MOI.Utilities.CachingOptimizer(
        MOI.Utilities.Model{Float64}(),
        _GetFallbackModel1310(),
    )
    x = MOI.add_variable(model)
    cx = MOI.add_constraint(model, x, MOI.GreaterThan(1.0))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{MOI.VariableIndex}(), x)
    MOI.optimize!(model)
    @test MOI.get(model, MOI.ConstraintDual(), cx) == 1.0
    @test_throws(
        MOI.ResultIndexBoundsError(MOI.ConstraintDual(2), 1),
        MOI.get(model, MOI.ConstraintDual(2), cx),
    )
    @test_throws(
        MOI.ResultIndexBoundsError(MOI.ConstraintDual(2), 1),
        MOI.get(model, MOI.ConstraintDual(2), [cx]),
    )
    return
end

function test_ConstraintDual_nonvariable_nofallback()
    model = MOI.Utilities.CachingOptimizer(
        MOI.Utilities.Model{Float64}(),
        _GetFallbackModel1310(),
    )
    x = MOI.add_variable(model)
    cx = MOI.add_constraint(model, x + 1.0, MOI.GreaterThan(1.0))
    MOI.optimize!(model)
    @test_throws(
        MOI.GetAttributeNotAllowed,
        MOI.get(model, MOI.ConstraintDual(), cx),
    )
    return
end

function test_ConstraintPrimal_fallback_error()
    model = MOI.Utilities.CachingOptimizer(
        MOI.Utilities.Model{Float64}(),
        MOI.Utilities.AUTOMATIC,
    )
    x = MOI.add_variable(model)
    c = MOI.add_constraint(model, x, MOI.GreaterThan(1.0))
    @test_throws(
        MOI.GetAttributeNotAllowed{MOI.ConstraintPrimal},
        MOI.get(model, MOI.ConstraintPrimal(), c),
    )
    @test_throws(
        MOI.GetAttributeNotAllowed{MOI.ConstraintPrimal},
        MOI.get(model, MOI.ConstraintPrimal(), [c]),
    )
    return
end

function test_ObjectiveValue_fallback()
    model = MOI.Utilities.CachingOptimizer(
        MOI.Utilities.Model{Float64}(),
        _GetFallbackModel1310(),
    )
    x = MOI.add_variable(model)
    MOI.add_constraint(model, x, MOI.GreaterThan(1.0))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{typeof(x)}(), x)
    MOI.optimize!(model)
    @test MOI.get(model, MOI.ObjectiveValue()) == 1.2
    @test_throws(
        MOI.ResultIndexBoundsError(MOI.ObjectiveValue(2), 1),
        MOI.get(model, MOI.ObjectiveValue(2)),
    )
    return
end

function test_DualObjectiveValue_fallback()
    model = MOI.Utilities.CachingOptimizer(
        MOI.Utilities.Model{Float64}(),
        _GetFallbackModel1310(),
    )
    x = MOI.add_variable(model)
    MOI.add_constraint(model, x, MOI.GreaterThan(1.0))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{typeof(x)}(), x)
    MOI.optimize!(model)
    @test MOI.get(model, MOI.DualObjectiveValue()) == 1.0
    @test_throws(
        MOI.ResultIndexBoundsError(MOI.DualObjectiveValue(2), 1),
        MOI.get(model, MOI.DualObjectiveValue(2)),
    )
    return
end

struct _OptimizerAttributeValue1670 end

function test_map_indices_issue_1670()
    optimizer = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    model = MOI.Utilities.CachingOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
        optimizer,
    )
    MOI.Utilities.attach_optimizer(model)
    MOI.set(
        model,
        MOI.RawOptimizerAttribute("1670"),
        _OptimizerAttributeValue1670(),
    )
    @test MOI.get(optimizer, MOI.RawOptimizerAttribute("1670")) ==
          _OptimizerAttributeValue1670()
    return
end

function test_copy_optimizer_attributes_2887()
    cache() = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    optimizer() = MOI.Utilities.MockOptimizer(cache())
    # Test without calling Silent
    model = cache()
    opt = optimizer()
    model = MOI.Utilities.CachingOptimizer(model, opt)
    @test MOI.get(opt, MOI.Silent()) === nothing
    # Test with Silent => true
    model = cache()
    MOI.set(model, MOI.Silent(), true)
    opt = optimizer()
    model = MOI.Utilities.CachingOptimizer(model, opt)
    @test MOI.get(opt, MOI.Silent()) == true
    # Test with Silent => false
    model = cache()
    MOI.set(model, MOI.Silent(), false)
    opt = optimizer()
    model = MOI.Utilities.CachingOptimizer(model, opt)
    @test MOI.get(opt, MOI.Silent()) == false
    return
end

mutable struct FinalTouchDetector <: MOI.ModelLike
    index_map::Any
end

function MOI.Utilities.final_touch(model::FinalTouchDetector, index_map)
    model.index_map = index_map
    return
end

function MOI.copy_to(::MOI.Utilities.MockOptimizer, ::FinalTouchDetector)
    return MOI.Utilities.IndexMap()
end

function MOI.get(::FinalTouchDetector, ::MOI.ListOfOptimizerAttributesSet)
    return MOI.AbstractOptimizerAttribute[]
end

function MOI.get(::FinalTouchDetector, ::MOI.ListOfModelAttributesSet)
    return MOI.AbstractModelAttribute[]
end

function test_final_touch_optimize()
    model = MOI.Utilities.CachingOptimizer(
        FinalTouchDetector(missing),
        MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}()),
    )
    MOI.Utilities.attach_optimizer(model)
    @test model.model_cache.index_map === nothing
    MOI.Utilities.final_touch(model, missing)
    @test model.model_cache.index_map === missing
    MOI.Utilities.reset_optimizer(model)
    MOI.optimize!(model)
    @test model.model_cache.index_map === nothing
end

function test_multiple_modifications()
    m = MOIU.CachingOptimizer(MOIU.Model{Float64}(), MOIU.AUTOMATIC)

    x = MOI.add_variables(m, 3)

    saf = MOI.ScalarAffineFunction(
        [
            MOI.ScalarAffineTerm(1.0, x[1]),
            MOI.ScalarAffineTerm(1.0, x[2]),
            MOI.ScalarAffineTerm(1.0, x[3]),
        ],
        0.0,
    )
    ci1 = MOI.add_constraint(m, saf, MOI.LessThan(1.0))
    ci2 = MOI.add_constraint(m, saf, MOI.LessThan(2.0))

    MOI.set(m, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), saf)

    fc1 = MOI.get(m, MOI.ConstraintFunction(), ci1)
    @test MOI.coefficient.(fc1.terms) == [1.0, 1.0, 1.0]
    fc2 = MOI.get(m, MOI.ConstraintFunction(), ci2)
    @test MOI.coefficient.(fc2.terms) == [1.0, 1.0, 1.0]
    obj = MOI.get(m, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    @test MOI.coefficient.(obj.terms) == [1.0, 1.0, 1.0]

    changes_cis = [
        MOI.ScalarCoefficientChange(MOI.VariableIndex(1), 4.0)
        MOI.ScalarCoefficientChange(MOI.VariableIndex(1), 0.5)
        MOI.ScalarCoefficientChange(MOI.VariableIndex(3), 2.0)
    ]
    MOI.modify(m, [ci1, ci2, ci2], changes_cis)

    fc1 = MOI.get(m, MOI.ConstraintFunction(), ci1)
    @test MOI.coefficient.(fc1.terms) == [4.0, 1.0, 1.0]
    fc2 = MOI.get(m, MOI.ConstraintFunction(), ci2)
    @test MOI.coefficient.(fc2.terms) == [0.5, 1.0, 2.0]

    changes_obj = [
        MOI.ScalarCoefficientChange(MOI.VariableIndex(1), 4.0)
        MOI.ScalarCoefficientChange(MOI.VariableIndex(2), 10.0)
        MOI.ScalarCoefficientChange(MOI.VariableIndex(3), 2.0)
    ]
    MOI.modify(
        m,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        changes_obj,
    )

    obj = MOI.get(m, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    @test MOI.coefficient.(obj.terms) == [4.0, 10.0, 2.0]
end

function test_show()
    model = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(
        model,
        """
        variables: x, y
        minobjective: 2.0 * x
        c1: x >= 0.0
        c2: 1.0 * x + 2.0 * y <= 3.0
        c2: 1.0 * x + 3.0 * y <= 2.0
        """,
    )
    cache = MOI.Utilities.CachingOptimizer(model, MOI.Utilities.MANUAL)
    output = """
    MOIU.CachingOptimizer
    ├ state: NO_OPTIMIZER
    ├ mode: MANUAL
    ├ model_cache: MOIU.Model{Float64}
    │ ├ ObjectiveSense: MIN_SENSE
    │ ├ ObjectiveFunctionType: MOI.ScalarAffineFunction{Float64}
    │ ├ NumberOfVariables: 2
    │ └ NumberOfConstraints: 3
    │   ├ MOI.ScalarAffineFunction{Float64} in MOI.LessThan{Float64}: 2
    │   └ MOI.VariableIndex in MOI.GreaterThan{Float64}: 1
    └ optimizer: nothing"""
    @test sprint(show, cache) == output
    return
end

function test_add_variable()
    for mode in (MOI.Utilities.AUTOMATIC, MOI.Utilities.MANUAL)
        cache = MOI.Utilities.Model{Float64}()
        optimizer = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
        model = MOI.Utilities.CachingOptimizer(cache, mode)
        MOI.Utilities.reset_optimizer(model, optimizer)
        MOI.Utilities.attach_optimizer(model)
        x = MOI.add_variable(model)
        @test MOI.is_valid(optimizer, model.model_to_optimizer_map[x])
    end
    return
end

function test_add_variables()
    for mode in (MOI.Utilities.AUTOMATIC, MOI.Utilities.MANUAL)
        cache = MOI.Utilities.Model{Float64}()
        optimizer = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
        model = MOI.Utilities.CachingOptimizer(cache, mode)
        MOI.Utilities.reset_optimizer(model, optimizer)
        MOI.Utilities.attach_optimizer(model)
        x = MOI.add_variables(model, 2)
        for xi in x
            @test MOI.is_valid(optimizer, model.model_to_optimizer_map[xi])
        end
    end
    return
end

function test_add_constrained_variable()
    for mode in (MOI.Utilities.AUTOMATIC, MOI.Utilities.MANUAL)
        cache = MOI.Utilities.Model{Float64}()
        optimizer = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
        model = MOI.Utilities.CachingOptimizer(cache, mode)
        MOI.Utilities.reset_optimizer(model, optimizer)
        MOI.Utilities.attach_optimizer(model)
        x, c = MOI.add_constrained_variable(model, MOI.ZeroOne())
        @test MOI.is_valid(optimizer, model.model_to_optimizer_map[x])
        @test MOI.is_valid(optimizer, model.model_to_optimizer_map[c])
    end
    return
end

function test_add_constrained_variables()
    for mode in (MOI.Utilities.AUTOMATIC, MOI.Utilities.MANUAL)
        cache = MOI.Utilities.Model{Float64}()
        optimizer = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
        model = MOI.Utilities.CachingOptimizer(cache, mode)
        MOI.Utilities.reset_optimizer(model, optimizer)
        MOI.Utilities.attach_optimizer(model)
        x, c = MOI.add_constrained_variables(model, MOI.Zeros(2))
        for xi in x
            @test MOI.is_valid(optimizer, model.model_to_optimizer_map[xi])
        end
        @test MOI.is_valid(optimizer, model.model_to_optimizer_map[c])
    end
    return
end

function test_modify_constraint()
    for mode in (MOI.Utilities.AUTOMATIC, MOI.Utilities.MANUAL)
        cache = MOI.Utilities.Model{Float64}()
        optimizer = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
        model = MOI.Utilities.CachingOptimizer(cache, mode)
        MOI.Utilities.reset_optimizer(model, optimizer)
        MOI.Utilities.attach_optimizer(model)
        x = MOI.add_variable(model)
        c = MOI.add_constraint(model, 1.0 * x, MOI.EqualTo(1.0))
        c_opt = model.model_to_optimizer_map[c]
        f = MOI.get(optimizer, MOI.ConstraintFunction(), c_opt)
        @test f ≈ 1.0 * model.model_to_optimizer_map[x]
        MOI.modify(model, c, MOI.ScalarCoefficientChange(x, 2.0))
        c_opt = model.model_to_optimizer_map[c]
        f = MOI.get(optimizer, MOI.ConstraintFunction(), c_opt)
        @test f ≈ 2.0 * model.model_to_optimizer_map[x]
    end
    return
end

function test_modify_objective()
    for mode in (MOI.Utilities.AUTOMATIC, MOI.Utilities.MANUAL)
        cache = MOI.Utilities.Model{Float64}()
        optimizer = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
        model = MOI.Utilities.CachingOptimizer(cache, mode)
        MOI.Utilities.reset_optimizer(model, optimizer)
        MOI.Utilities.attach_optimizer(model)
        x = MOI.add_variable(model)
        f = 1.0 * x
        attr = MOI.ObjectiveFunction{typeof(f)}()
        MOI.set(model, attr, f)
        @test MOI.get(optimizer, attr) ≈ 1.0 * model.model_to_optimizer_map[x]
        MOI.modify(model, attr, MOI.ScalarCoefficientChange(x, 2.0))
        @test MOI.get(optimizer, attr) ≈ 2.0 * model.model_to_optimizer_map[x]
    end
    return
end

function test_delete_invalid_index()
    for mode in (MOI.Utilities.AUTOMATIC, MOI.Utilities.MANUAL)
        cache = MOI.Utilities.Model{Float64}()
        optimizer = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
        model = MOI.Utilities.CachingOptimizer(cache, mode)
        MOI.Utilities.reset_optimizer(model, optimizer)
        MOI.Utilities.attach_optimizer(model)
        x = MOI.add_variable(model)
        @test MOI.is_valid(optimizer, model.model_to_optimizer_map[x])
        MOI.delete(model, x)
        @test_throws(KeyError, model.model_to_optimizer_map[x])
        @test_throws(MOI.InvalidIndex, MOI.delete(model, x))
    end
    return
end

function test_delete_variables()
    for mode in (MOI.Utilities.AUTOMATIC, MOI.Utilities.MANUAL)
        cache = MOI.Utilities.Model{Float64}()
        optimizer = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
        model = MOI.Utilities.CachingOptimizer(cache, mode)
        MOI.Utilities.reset_optimizer(model, optimizer)
        MOI.Utilities.attach_optimizer(model)
        x, y = MOI.add_variables(model, 2)
        @test MOI.is_valid(optimizer, model.model_to_optimizer_map[x])
        @test MOI.is_valid(optimizer, model.model_to_optimizer_map[y])
        MOI.delete(model, [x])
        @test_throws KeyError model.model_to_optimizer_map[x]
        @test MOI.is_valid(optimizer, model.model_to_optimizer_map[y])
        @test_throws(MOI.InvalidIndex, MOI.delete(model, x))
    end
    return
end

function test_set_attribute()
    for mode in (MOI.Utilities.AUTOMATIC, MOI.Utilities.MANUAL)
        cache = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
        optimizer = MOI.Utilities.MockOptimizer(
            MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
        )
        model = MOI.Utilities.CachingOptimizer(cache, mode)
        MOI.Utilities.reset_optimizer(model, optimizer)
        MOI.Utilities.attach_optimizer(model)
        x = MOI.add_variable(model)
        @test MOI.is_valid(optimizer, model.model_to_optimizer_map[x])
        attr = MOI.VariablePrimalStart()
        MOI.set(model, attr, x, 1.23)
        opt_attr = MOI.Utilities.AttributeFromOptimizer(attr)
        cache_attr = MOI.Utilities.AttributeFromModelCache(attr)
        @test MOI.get(model, opt_attr, x) == 1.23
        @test MOI.get(model, cache_attr, x) == 1.23
    end
    return
end

function test_get_AttributeFromOptimizer()
    cache = MOI.Utilities.Model{Float64}()
    optimizer = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    model = MOI.Utilities.CachingOptimizer(cache, optimizer)
    x = MOI.add_variable(model)
    MOI.Utilities.attach_optimizer(model)
    attr = MOI.VariableName()
    optimizer_attr = MOI.Utilities.AttributeFromOptimizer(attr)
    @test MOI.supports(model, optimizer_attr, MOI.VariableIndex)
    MOI.set(model, optimizer_attr, x, "x")
    @test MOI.get(model, attr, x) == ""
    @test MOI.get(model, optimizer_attr, x) == "x"
    @test MOI.get(model, optimizer_attr, [x]) == ["x"]
    return
end

function test_get_AttributeFromModelCache()
    cache = MOI.Utilities.Model{Float64}()
    optimizer = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    model = MOI.Utilities.CachingOptimizer(cache, optimizer)
    x = MOI.add_variable(model)
    MOI.Utilities.attach_optimizer(model)
    # VariableName
    attr = MOI.VariableName()
    cache_attr = MOI.Utilities.AttributeFromModelCache(attr)
    MOI.set(model, cache_attr, x, "x")
    @test MOI.supports(model, cache_attr, MOI.VariableIndex)
    @test MOI.get(model, attr, x) == "x"
    @test MOI.get(model, cache_attr, x) == "x"
    @test MOI.get(model, MOI.Utilities.AttributeFromOptimizer(attr), x) == ""
    # Name
    attr = MOI.Name()
    cache_attr = MOI.Utilities.AttributeFromModelCache(attr)
    MOI.set(model, cache_attr, "m")
    @test MOI.supports(model, cache_attr)
    @test MOI.get(model, attr) == "m"
    @test MOI.get(model, cache_attr) == "m"
    @test MOI.get(model, MOI.Utilities.AttributeFromOptimizer(attr)) == ""
    return
end

end  # module

TestCachingOptimizer.runtests()
