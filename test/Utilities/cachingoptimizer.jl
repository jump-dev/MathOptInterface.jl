using Test
import MathOptInterface
const MOI = MathOptInterface
const MOIT = MOI.Test
const MOIU = MOI.Utilities

@testset "Test default attributes" begin
    # Without an optimizer attached (i.e., `MOI.state(model) == NO_OPTIMIZER`) we
    # need throw nice errors for attributes that are based on the optimizer. For
    # `AbstractModelAttribute`s that `is_set_by_optimize` returns `true` for, we
    # overload `TerminationStatus`, `PrimalStatus`, or `DualStatus` to return
    # sane default values. Otherwise we throw a nice error.
    model = MOIU.CachingOptimizer(MOIU.Model{Float64}(), MOIU.MANUAL)
    @test MOIU.state(model) == MOIU.NO_OPTIMIZER

    @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
    @test MOI.get(model, MOI.PrimalStatus()) == MOI.NO_SOLUTION
    @test MOI.get(model, MOI.DualStatus()) == MOI.NO_SOLUTION
    x = MOI.add_variables(model, 2)
    attr = MOI.VariablePrimal()
    exception = ErrorException(
        "Cannot query $(attr) from caching optimizer because no optimizer" *
        " is attached.")
    @test_throws exception MOI.get(model, MOI.VariablePrimal(), x[1])
    @test_throws exception MOI.get(model, MOI.VariablePrimal(), x)

    attr = MOI.SolverName()
    exception = ErrorException(
        "Cannot query $(attr) from caching optimizer because no optimizer" *
        " is attached.")
    @test_throws exception MOI.get(model, attr)
    attr = MOI.Silent()
    exception = ErrorException(
        "Cannot query $(attr) from caching optimizer because no optimizer" *
        " is attached.")
    @test_throws exception MOI.get(model, attr)
    attr = MOI.TimeLimitSec()
    exception = ErrorException(
        "Cannot query $(attr) from caching optimizer because no optimizer" *
        " is attached.")
    @test_throws exception MOI.get(model, attr)
    attr = MOI.ResultCount()
    exception = ErrorException(
        "Cannot query $(attr) from caching optimizer because no optimizer" *
        " is attached.")
    @test_throws exception MOI.get(model, attr)
end

@testset "Copyable solver attributes" begin
    cache = MOIU.UniversalFallback(MOIU.Model{Float64}())
    cached = MOIU.CachingOptimizer(cache, MOIU.MANUAL)
    MOI.set(cached, MOI.Silent(), true)
    MOI.set(cached, MOI.TimeLimitSec(), 0.0)
    mock = MOIU.MockOptimizer(MOIU.UniversalFallback(MOIU.Model{Float64}()))
    MOIU.reset_optimizer(cached, mock)
    @test MOI.get(mock, MOI.Silent())
    @test MOI.get(cached, MOI.Silent())
    @test MOI.get(mock, MOI.TimeLimitSec()) == 0.0
    @test MOI.get(cached, MOI.TimeLimitSec()) == 0.0
    MOI.set(cached, MOI.Silent(), false)
    MOI.set(cached, MOI.TimeLimitSec(), 1.0)
    @test !MOI.get(mock, MOI.Silent())
    @test !MOI.get(cached, MOI.Silent())
    @test MOI.get(mock, MOI.TimeLimitSec()) ≈ 1.0
    @test MOI.get(cached, MOI.TimeLimitSec()) ≈ 1.0
    mock = MOIU.MockOptimizer(MOIU.UniversalFallback(MOIU.Model{Float64}()))
    MOIU.reset_optimizer(cached, mock)
    @test !MOI.get(mock, MOI.Silent())
    @test !MOI.get(cached, MOI.Silent())
    @test MOI.get(mock, MOI.TimeLimitSec()) ≈ 1.0
    @test MOI.get(cached, MOI.TimeLimitSec()) ≈ 1.0
    MOI.set(cached, MOI.Silent(), true)
    MOI.set(cached, MOI.TimeLimitSec(), 0.0)
    @test MOI.get(mock, MOI.Silent())
    @test MOI.get(cached, MOI.Silent())
    @test MOI.get(mock, MOI.TimeLimitSec()) == 0.0
    @test MOI.get(cached, MOI.TimeLimitSec()) == 0.0
    mock = MOIU.MockOptimizer(MOIU.UniversalFallback(MOIU.Model{Float64}()))
    MOIU.reset_optimizer(cached, mock)
    @test MOI.get(mock, MOI.Silent())
    @test MOI.get(cached, MOI.Silent())
    @test MOI.get(mock, MOI.TimeLimitSec()) == 0.0
    @test MOI.get(cached, MOI.TimeLimitSec()) == 0.0
end

struct DummyModelAttribute <: MOI.AbstractModelAttribute end
struct DummyEvaluator <: MOI.AbstractNLPEvaluator end
struct DummyVariableAttribute <: MOI.AbstractVariableAttribute end
struct DummyConstraintAttribute <: MOI.AbstractConstraintAttribute end

@testset "Mapping of variables" begin
    mock = MOIU.MockOptimizer(MOIU.UniversalFallback(MOIU.Model{Float64}()))
    model = MOIU.CachingOptimizer(MOIU.UniversalFallback(MOIU.Model{Float64}()), mock)
    x = MOI.add_variable(model)
    y = first(MOI.get(mock, MOI.ListOfVariableIndices()))
    @test !MOI.is_valid(model, y)
    @test !MOI.is_valid(mock, x)
    fx = MOI.SingleVariable(x)
    fy = MOI.SingleVariable(y)

    cfx = MOI.add_constraint(model, fx, MOI.GreaterThan(1.0))
    cfy = first(MOI.get(mock, MOI.ListOfConstraintIndices{
        MOI.SingleVariable, MOI.GreaterThan{Float64}}()))
    @test !MOI.is_valid(model, cfy)
    @test !MOI.is_valid(mock, cfx)
    @test MOI.get(mock, MOI.ConstraintFunction(), cfy) == fy

    c2fx = MOI.add_constraint(model, 2.0fx, MOI.GreaterThan(1.0))
    c2fy = first(MOI.get(mock, MOI.ListOfConstraintIndices{
        MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}}()))
    @test !MOI.is_valid(model, c2fy)
    @test !MOI.is_valid(mock, c2fx)
    @test MOI.get(model, MOI.ConstraintFunction(), c2fx) ≈ 2.0fx
    @test MOI.get(model, MOI.ConstraintSet(), c2fx) == MOI.GreaterThan(1.0)
    @test MOI.get(mock, MOI.ConstraintFunction(), c2fy) ≈ 2.0fy
    @test MOI.get(mock, MOI.ConstraintSet(), c2fy) == MOI.GreaterThan(1.0)

    MOI.set(model, MOI.ConstraintSet(), c2fx, MOI.GreaterThan(2.0))
    @test MOI.get(model, MOI.ConstraintFunction(), c2fx) ≈ 2.0fx
    @test MOI.get(model, MOI.ConstraintSet(), c2fx) == MOI.GreaterThan(2.0)
    @test MOI.get(mock, MOI.ConstraintFunction(), c2fy) ≈ 2.0fy
    @test MOI.get(mock, MOI.ConstraintSet(), c2fy) == MOI.GreaterThan(2.0)

    MOI.set(model, MOI.ConstraintFunction(), c2fx, 3.0fx)
    @test MOI.get(model, MOI.ConstraintFunction(), c2fx) ≈ 3.0fx
    @test MOI.get(model, MOI.ConstraintSet(), c2fx) == MOI.GreaterThan(2.0)
    @test MOI.get(mock, MOI.ConstraintFunction(), c2fy) ≈ 3.0fy
    @test MOI.get(mock, MOI.ConstraintSet(), c2fy) == MOI.GreaterThan(2.0)

    MOI.set(model, MOI.ConstraintSet(), c2fx, MOI.GreaterThan(4.0))
    @test MOI.get(model, MOI.ConstraintFunction(), c2fx) ≈ 3.0fx
    @test MOI.get(model, MOI.ConstraintSet(), c2fx) == MOI.GreaterThan(4.0)
    @test MOI.get(mock, MOI.ConstraintFunction(), c2fy) ≈ 3.0fy
    @test MOI.get(mock, MOI.ConstraintSet(), c2fy) == MOI.GreaterThan(4.0)

    MOI.set(model, MOI.ObjectiveFunction{typeof(fx)}(), fx)
    @test MOI.get(mock, MOI.ObjectiveFunction{typeof(fy)}()) == fy

    MOI.set(model, MOI.ObjectiveFunction{typeof(2.0fx)}(), 2.0fx)
    @test MOI.get(mock, MOI.ObjectiveFunction{typeof(2.0fy)}()) ≈ 2.0fy

    MOI.set(model, DummyModelAttribute(), 2.0fx + 1.0)
    @test MOI.get(model, DummyModelAttribute()) ≈ 2.0fx + 1.0
    @test MOI.get(mock, DummyModelAttribute()) ≈ 2.0fy + 1.0

    for (attr, cache_index, optimizer_index) in [
        (DummyVariableAttribute(), x, y),
        (DummyConstraintAttribute(), cfx, cfy)]
        MOI.set(model, attr, cache_index, 1.0fx)
        @test MOI.get(model, attr, cache_index) ≈ 1.0fx
        @test MOI.get(mock, attr, optimizer_index) ≈ 1.0fy

        MOI.set(model, attr, [cache_index], [3.0fx])
        @test MOI.get(model, attr, [cache_index])[1] ≈ 3.0fx
        @test MOI.get(mock, attr, [optimizer_index])[1] ≈ 3.0fy
    end

    nlp_data = MOI.NLPBlockData(
        [MOI.NLPBoundsPair(1.0, 2.0)],
        DummyEvaluator(), false)
    MOI.set(model, MOI.NLPBlock(), nlp_data)
    for nlp_data in [MOI.get(model, MOI.NLPBlock()), MOI.get(mock, MOI.NLPBlock())]
        @test nlp_data.constraint_bounds == [MOI.NLPBoundsPair(1.0, 2.0)]
        @test nlp_data.evaluator isa DummyEvaluator
        @test nlp_data.has_objective == false
    end
end

@testset "CachingOptimizer MANUAL mode" begin
    m = MOIU.CachingOptimizer(MOIU.Model{Float64}(), MOIU.MANUAL)
    @test MOIU.state(m) == MOIU.NO_OPTIMIZER

    s = MOIU.MockOptimizer(MOIU.Model{Float64}(), supports_names=false)
    @test MOI.is_empty(s)
    MOIU.reset_optimizer(m, s)
    @test MOIU.state(m) == MOIU.EMPTY_OPTIMIZER

    v = MOI.add_variable(m)
    x = MOI.add_variables(m, 2)
    saf = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0, 2.0, 3.0], [v; x]), 0.0)
    @test MOI.supports(m, MOI.ObjectiveFunction{typeof(saf)}())
    MOI.set(m, MOI.ObjectiveFunction{typeof(saf)}(), saf)
    @test MOI.get(m, MOIU.AttributeFromModelCache(MOI.ObjectiveFunction{typeof(saf)}())) ≈ saf
    @test MOI.get(m, MOI.ObjectiveFunction{typeof(saf)}()) ≈ saf

    @test_throws AssertionError MOI.optimize!(m)

    MOIU.attach_optimizer(m)
    @test MOIU.state(m) == MOIU.ATTACHED_OPTIMIZER
    @test MOI.get(m, MOIU.AttributeFromOptimizer(MOI.ObjectiveFunction{typeof(saf)}())) ≈ saf

    @test MOI.supports(m, MOI.ObjectiveSense())
    MOI.set(m, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    @test MOI.get(m, MOI.ObjectiveSense()) == MOI.MAX_SENSE
    @test MOI.get(m, MOIU.AttributeFromModelCache(MOI.ObjectiveSense())) == MOI.MAX_SENSE
    @test MOI.get(m, MOIU.AttributeFromOptimizer(MOI.ObjectiveSense())) == MOI.MAX_SENSE

    @test MOI.supports(m, MOIU.AttributeFromOptimizer(MOIU.MockModelAttribute()))
    MOI.set(m, MOIU.AttributeFromOptimizer(MOIU.MockModelAttribute()), 10)
    @test MOI.get(m, MOIU.AttributeFromOptimizer(MOIU.MockModelAttribute())) == 10

    MOI.set(m, MOIU.AttributeFromOptimizer(MOI.ResultCount()), 1)
    @test MOI.supports(m, MOIU.AttributeFromOptimizer(MOI.VariablePrimal()), typeof(v))
    MOI.set(m, MOIU.AttributeFromOptimizer(MOI.VariablePrimal()), v, 3.0)

    MOI.optimize!(m)

    @test MOI.get(m, MOI.VariablePrimal(), v) == 3.0
    @test MOI.get(m, MOI.VariablePrimal(), [v]) == [3.0]
    @test MOI.get(m, MOIU.AttributeFromOptimizer(MOI.VariablePrimal()), v) == 3.0

    @test MOI.supports_constraint(m.model_cache, MOI.SingleVariable, MOI.LessThan{Float64})
    @test MOI.supports_constraint(m.optimizer.inner_model, MOI.SingleVariable, MOI.LessThan{Float64})
    @test MOI.supports_constraint(m.optimizer, MOI.SingleVariable, MOI.LessThan{Float64})
    @test MOI.supports_constraint(m, MOI.SingleVariable, MOI.LessThan{Float64})
    lb = MOI.add_constraint(m, MOI.SingleVariable(v), MOI.LessThan(10.0))
    MOI.set(m, MOI.ConstraintSet(), lb, MOI.LessThan(11.0))
    @test MOI.get(m, MOI.ConstraintSet(), lb) == MOI.LessThan(11.0)
    @test MOI.get(m, MOI.ConstraintFunction(), lb) == MOI.SingleVariable(v)

    MOIU.drop_optimizer(m)
    @test MOIU.state(m) == MOIU.NO_OPTIMIZER

    MOI.set(m, MOI.ConstraintSet(), lb, MOI.LessThan(12.0))
    @test MOI.get(m, MOI.ConstraintSet(), lb) == MOI.LessThan(12.0)

    MOI.delete(m, x[2])
    @test_throws MOI.InvalidIndex{typeof(x[2])} MOI.delete(m, x[2])
    @test !MOI.is_valid(m, x[2])

    # TODO: test more constraint modifications


end

@testset "CachingOptimizer AUTOMATIC mode" begin
    m = MOIU.CachingOptimizer(MOIU.Model{Float64}(), MOIU.AUTOMATIC)
    @test MOIU.state(m) == MOIU.NO_OPTIMIZER

    v = MOI.add_variable(m)
    @test MOI.supports(m, MOI.VariableName(), typeof(v))
    MOI.set(m, MOI.VariableName(), v, "v")
    @test MOI.get(m, MOI.VariableName(), v) == "v"

    s = MOIU.MockOptimizer(MOIU.Model{Float64}(), supports_names=false)
    @test MOI.is_empty(s)
    MOIU.reset_optimizer(m, s)
    @test MOIU.state(m) == MOIU.EMPTY_OPTIMIZER

    saf = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, v)], 0.0)
    @test MOI.supports(m, MOI.ObjectiveFunction{typeof(saf)}())
    MOI.set(m, MOI.ObjectiveFunction{typeof(saf)}(), saf)
    @test MOI.get(m, MOIU.AttributeFromModelCache(MOI.ObjectiveFunction{typeof(saf)}())) ≈ saf

    MOI.optimize!(m)
    @test MOIU.state(m) == MOIU.ATTACHED_OPTIMIZER
    @test MOI.get(m, MOIU.AttributeFromOptimizer(MOI.TerminationStatus())) == MOI.OPTIMIZE_NOT_CALLED

    @test MOI.get(m, MOI.VariableName(), v) == "v"
    @test MOI.get(m, MOIU.AttributeFromModelCache(MOI.VariableName()), v) == "v"
    # The caching optimizer does not copy names
    @test MOI.get(m, MOIU.AttributeFromOptimizer(MOI.VariableName()), v) == ""

    @test MOI.supports(m, MOI.ObjectiveSense())
    MOI.set(m, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    @test MOI.get(m, MOIU.AttributeFromModelCache(MOI.ObjectiveSense())) == MOI.MAX_SENSE
    @test MOI.get(m, MOIU.AttributeFromOptimizer(MOI.ObjectiveSense())) == MOI.MAX_SENSE

    @test MOI.supports(m, MOIU.AttributeFromOptimizer(MOIU.MockModelAttribute()))
    MOI.set(m, MOIU.AttributeFromOptimizer(MOIU.MockModelAttribute()), 10)
    @test MOI.get(m, MOIU.AttributeFromOptimizer(MOIU.MockModelAttribute())) == 10

    MOI.set(m, MOIU.AttributeFromOptimizer(MOI.ResultCount()), 1)
    @test MOI.supports(m, MOIU.AttributeFromOptimizer(MOI.VariablePrimal()), typeof(v))
    MOI.set(m, MOIU.AttributeFromOptimizer(MOI.VariablePrimal()), v, 3.0)

    MOI.optimize!(m)

    @test MOI.get(m, MOIU.AttributeFromOptimizer(MOI.VariablePrimal()), v) == 3.0

    @testset "Modify not allowed" begin
        s.modify_allowed = false
        MOI.modify(m, MOI.ObjectiveFunction{typeof(saf)}(),
                    MOI.ScalarConstantChange(1.0))
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
        MOI.add_constraint(m, MOI.VectorOfVariables([v]), MOI.SecondOrderCone(1))
        s.add_con_allowed = true
        @test MOIU.state(m) == MOIU.EMPTY_OPTIMIZER
        MOI.empty!(m)
        @test MOIU.state(m) == MOIU.ATTACHED_OPTIMIZER
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
        ci = MOI.add_constraint(m, MOI.SingleVariable(vi), MOI.EqualTo(0.0))
        s.delete_allowed = false # Simulate optimizer that cannot delete constraint
        MOI.delete(m, ci)
        s.delete_allowed = true
        @test MOIU.state(m) == MOIU.EMPTY_OPTIMIZER
        MOIU.attach_optimizer(m)
        @test MOIU.state(m) == MOIU.ATTACHED_OPTIMIZER
    end

end

@testset "Constructor with optimizer" begin
    @testset "Empty model and optimizer" begin
        s = MOIU.MockOptimizer(MOIU.Model{Float64}(), supports_names=false)
        model = MOIU.Model{Float64}()
        m = MOIU.CachingOptimizer(model, s)
        @test m isa MOIU.CachingOptimizer{typeof(s), typeof(model)}
        @test MOI.is_empty(m)
        @test MOIU.state(m) == MOIU.ATTACHED_OPTIMIZER
        @test MOIU.mode(m) == MOIU.AUTOMATIC
        @test MOI.get(m, MOI.SolverName()) == "Mock"
    end
    @testset "Non-empty optimizer" begin
        s = MOIU.MockOptimizer(MOIU.Model{Float64}(), supports_names=false)
        MOI.add_variable(s)
        model = MOIU.Model{Float64}()
        @test MOI.is_empty(model)
        @test !MOI.is_empty(s)
        @test_throws AssertionError MOIU.CachingOptimizer(model, s)
    end
    @testset "Non-empty model" begin
        s = MOIU.MockOptimizer(MOIU.Model{Float64}(), supports_names=false)
        model = MOIU.Model{Float64}()
        MOI.add_variable(model)
        @test !MOI.is_empty(model)
        @test MOI.is_empty(s)
        @test_throws AssertionError MOIU.CachingOptimizer(model, s)
    end
end

for state in (MOIU.NO_OPTIMIZER, MOIU.EMPTY_OPTIMIZER, MOIU.ATTACHED_OPTIMIZER)
    @testset "Optimization tests in state $state and mode $mode" for mode in (MOIU.MANUAL, MOIU.AUTOMATIC)
        m = MOIU.CachingOptimizer(MOIU.Model{Float64}(), mode)
        if state != MOIU.NO_OPTIMIZER
            s = MOIU.MockOptimizer(MOIU.Model{Float64}(), supports_names=false)
            MOIU.reset_optimizer(m, s)
            if state == MOIU.ATTACHED_OPTIMIZER
                MOIU.attach_optimizer(m)
            end
        end
        @test MOIU.state(m) == state
        @test MOIU.mode(m) == mode

        @testset "Name test" begin
            MOIT.nametest(m)
        end

        @testset "Copy test" begin
            MOIT.failcopytestc(m)
            MOIT.failcopytestia(m)
            MOIT.failcopytestva(m)
            MOIT.failcopytestca(m)
            MOIT.copytest(m, MOIU.Model{Float64}())
        end

        config = MOIT.TestConfig(solve=false)
        @testset "Unit" begin
            MOIT.unittest(m, config)
        end
        @testset "Continuous Linear" begin
            exclude = ["partial_start"] # VariablePrimalStart not supported.
            MOIT.contlineartest(m, config, exclude)
        end
    end
end
