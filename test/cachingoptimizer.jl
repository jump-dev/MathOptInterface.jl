
@MOIU.model ModelForCachingOptimizer (ZeroOne, Integer) (EqualTo, GreaterThan, LessThan, Interval) (Zeros, Nonnegatives, Nonpositives, SecondOrderCone, RotatedSecondOrderCone, GeometricMeanCone, ExponentialCone, DualExponentialCone, PositiveSemidefiniteConeTriangle, RootDetConeTriangle, LogDetConeTriangle) () (SingleVariable,) (ScalarAffineFunction,ScalarQuadraticFunction) (VectorOfVariables,) (VectorAffineFunction,)

@testset "CachingOptimizer Manual mode" begin
    m = MOIU.CachingOptimizer(ModelForCachingOptimizer{Float64}(), MOIU.Manual)
    @test MOIU.state(m) == MOIU.NoOptimizer

    s = MOIU.MockOptimizer(ModelForMock{Float64}())
    @test MOI.isempty(s)
    MOIU.resetoptimizer!(m, s)
    @test MOIU.state(m) == MOIU.EmptyOptimizer

    v = MOI.addvariable!(m)
    x = MOI.addvariables!(m, 2)
    saf = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0, 2.0, 3.0], [v; x]), 0.0)
    @test MOI.canset(m, MOI.ObjectiveFunction{typeof(saf)}())
    MOI.set!(m, MOI.ObjectiveFunction{typeof(saf)}(), saf)
    @test MOI.get(m, MOIU.AttributeFromModelCache(MOI.ObjectiveFunction{typeof(saf)}())) ≈ saf
    @test MOI.get(m, MOI.ObjectiveFunction{typeof(saf)}()) ≈ saf
    @test !MOI.canget(m, MOIU.AttributeFromOptimizer(MOI.ObjectiveSense()))

    @test_throws AssertionError MOI.optimize!(m)

    MOIU.attachoptimizer!(m)
    @test MOIU.state(m) == MOIU.AttachedOptimizer
    @test MOI.get(m, MOIU.AttributeFromOptimizer(MOI.ObjectiveFunction{typeof(saf)}())) ≈ saf

    @test MOI.canset(m, MOI.ObjectiveSense())
    MOI.set!(m, MOI.ObjectiveSense(), MOI.MaxSense)
    @test MOI.canget(m, MOI.ObjectiveSense())
    @test MOI.canget(m, MOIU.AttributeFromModelCache(MOI.ObjectiveSense()))
    @test MOI.canget(m, MOIU.AttributeFromOptimizer(MOI.ObjectiveSense()))
    @test MOI.get(m, MOI.ObjectiveSense()) == MOI.MaxSense
    @test MOI.get(m, MOIU.AttributeFromModelCache(MOI.ObjectiveSense())) == MOI.MaxSense
    @test MOI.get(m, MOIU.AttributeFromOptimizer(MOI.ObjectiveSense())) == MOI.MaxSense

    @test !MOI.canset(m, MOI.NumberOfVariables())
    @test !MOI.canget(m, MOIU.AttributeFromModelCache(MOIU.MockModelAttribute()))
    @test MOI.canget(m, MOIU.AttributeFromOptimizer((MOIU.MockModelAttribute())))

    @test MOI.canset(m, MOIU.AttributeFromOptimizer(MOIU.MockModelAttribute()))
    MOI.set!(m, MOIU.AttributeFromOptimizer(MOIU.MockModelAttribute()), 10)
    @test MOI.get(m, MOIU.AttributeFromOptimizer(MOIU.MockModelAttribute())) == 10

    MOI.set!(m, MOIU.AttributeFromOptimizer(MOI.ResultCount()), 1)
    @test MOI.canset(m, MOIU.AttributeFromOptimizer(MOI.VariablePrimal()), typeof(v))
    MOI.set!(m, MOIU.AttributeFromOptimizer(MOI.VariablePrimal()), v, 3.0)

    MOI.optimize!(m)

    @test MOI.canget(m, MOI.VariablePrimal(), typeof(v))
    @test MOI.get(m, MOI.VariablePrimal(), v) == 3.0
    @test MOI.get(m, MOI.VariablePrimal(), [v]) == [3.0]
    @test MOI.canget(m, MOIU.AttributeFromOptimizer(MOI.VariablePrimal()), typeof(v))
    @test MOI.get(m, MOIU.AttributeFromOptimizer(MOI.VariablePrimal()), v) == 3.0

    # ModelForMock doesn't support RotatedSecondOrderCone
    @test !MOI.canaddconstraint(m, MOI.VectorOfVariables, MOI.RotatedSecondOrderCone)

    @test MOI.canaddconstraint(m.model_cache, MOI.SingleVariable, MOI.LessThan{Float64})
    @test MOI.canaddconstraint(m.optimizer.inner_model, MOI.SingleVariable, MOI.LessThan{Float64})
    @test MOI.canaddconstraint(m.optimizer, MOI.SingleVariable, MOI.LessThan{Float64})
    @test MOI.canaddconstraint(m, MOI.SingleVariable, MOI.LessThan{Float64})
    lb = MOI.addconstraint!(m, MOI.SingleVariable(v), MOI.LessThan(10.0))
    @test MOI.canset(m, MOI.ConstraintSet(), lb, MOI.LessThan{Float64})
    MOI.set!(m, MOI.ConstraintSet(), lb, MOI.LessThan(11.0))
    @test MOI.get(m, MOI.ConstraintSet(), lb) == MOI.LessThan(11.0)
    @test MOI.get(m, MOI.ConstraintFunction(), lb) == MOI.SingleVariable(v)

    MOIU.dropoptimizer!(m)
    @test MOIU.state(m) == MOIU.NoOptimizer

    @test MOI.canset(m, MOI.ConstraintSet(), lb, MOI.LessThan{Float64})
    MOI.set!(m, MOI.ConstraintSet(), lb, MOI.LessThan(12.0))
    @test MOI.get(m, MOI.ConstraintSet(), lb) == MOI.LessThan(12.0)

    @test MOI.candelete(m, x[2])
    MOI.delete!(m, x[2])
    @test !MOI.isvalid(m, x[2])

    # TODO: test more constraint modifications


end

@testset "CachingOptimizer Automatic mode" begin
    m = MOIU.CachingOptimizer(ModelForCachingOptimizer{Float64}(), MOIU.Automatic)
    @test MOIU.state(m) == MOIU.NoOptimizer

    v = MOI.addvariable!(m)
    @test MOI.canset(m, MOI.VariableName(), typeof(v))
    MOI.set!(m, MOI.VariableName(), v, "v")
    @test MOI.canget(m, MOI.VariableName(), typeof(v))
    @test MOI.get(m, MOI.VariableName(), v) == "v"

    s = MOIU.MockOptimizer(ModelForMock{Float64}())
    @test MOI.isempty(s)
    MOIU.resetoptimizer!(m, s)
    @test MOIU.state(m) == MOIU.EmptyOptimizer

    saf = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, v)], 0.0)
    @test MOI.canset(m, MOI.ObjectiveFunction{typeof(saf)}())
    MOI.set!(m, MOI.ObjectiveFunction{typeof(saf)}(), saf)
    @test MOI.get(m, MOIU.AttributeFromModelCache(MOI.ObjectiveFunction{typeof(saf)}())) ≈ saf

    MOI.optimize!(m)
    @test MOIU.state(m) == MOIU.AttachedOptimizer
    @test MOI.get(m, MOIU.AttributeFromOptimizer(MOI.ResultCount())) == 0

    @test MOI.canget(m, MOI.VariableName(), typeof(v))
    @test MOI.get(m, MOI.VariableName(), v) == "v"
    @test MOI.canget(m, MOIU.AttributeFromModelCache(MOI.VariableName()), typeof(v))
    @test MOI.get(m, MOIU.AttributeFromModelCache(MOI.VariableName()), v) == "v"
    # The caching optimizer does not copy names
    @test MOI.get(m, MOIU.AttributeFromOptimizer(MOI.VariableName()), v) == ""

    @test MOI.canset(m, MOI.ObjectiveSense())
    MOI.set!(m, MOI.ObjectiveSense(), MOI.MaxSense)
    @test MOI.canget(m, MOIU.AttributeFromModelCache(MOI.ObjectiveSense()))
    @test MOI.canget(m, MOIU.AttributeFromOptimizer(MOI.ObjectiveSense()))
    @test MOI.get(m, MOIU.AttributeFromModelCache(MOI.ObjectiveSense())) == MOI.MaxSense
    @test MOI.get(m, MOIU.AttributeFromOptimizer(MOI.ObjectiveSense())) == MOI.MaxSense

    @test !MOI.canget(m, MOIU.AttributeFromModelCache(MOIU.MockModelAttribute()))
    @test MOI.canget(m, MOIU.AttributeFromOptimizer((MOIU.MockModelAttribute())))

    @test MOI.canset(m, MOIU.AttributeFromOptimizer(MOIU.MockModelAttribute()))
    MOI.set!(m, MOIU.AttributeFromOptimizer(MOIU.MockModelAttribute()), 10)
    @test MOI.get(m, MOIU.AttributeFromOptimizer(MOIU.MockModelAttribute())) == 10

    MOI.set!(m, MOIU.AttributeFromOptimizer(MOI.ResultCount()), 1)
    @test MOI.canset(m, MOIU.AttributeFromOptimizer(MOI.VariablePrimal()), typeof(v))
    MOI.set!(m, MOIU.AttributeFromOptimizer(MOI.VariablePrimal()), v, 3.0)

    MOI.optimize!(m)

    @test MOI.canget(m, MOIU.AttributeFromOptimizer(MOI.VariablePrimal()), typeof(v))
    @test MOI.get(m, MOIU.AttributeFromOptimizer(MOI.VariablePrimal()), v) == 3.0

    # ModelForMock doesn't support RotatedSecondOrderCone
    MOI.addconstraint!(m, MOI.VectorOfVariables([v]), MOI.RotatedSecondOrderCone(1))
    @test MOIU.state(m) == MOIU.EmptyOptimizer

    # TODO: test modifyconstraint! with a change that forces the optimizer to be dropped

    MOI.empty!(m)
    @test MOIU.state(m) == MOIU.AttachedOptimizer

    m.optimizer.canaddvar = false # Simulate optimizer for which MOI.canaddvariable returns false
    MOI.addvariable!(m)
    @test MOIU.state(m) == MOIU.EmptyOptimizer
    res = MOIU.attachoptimizer!(m)
    @test res.status == MOI.CopyOtherError
    @test MOIU.state(m) == MOIU.EmptyOptimizer

    m.optimizer.canaddvar = true
    res = MOIU.attachoptimizer!(m)
    @test res.status == MOI.CopySuccess
    @test MOIU.state(m) == MOIU.AttachedOptimizer
    m.optimizer.canaddvar = false
    MOI.addvariables!(m, 2)
    @test MOIU.state(m) == MOIU.EmptyOptimizer
end

@testset "CachingOptimizer constructor with optimizer" begin
    @testset "Empty model and optimizer" begin
        s = MOIU.MockOptimizer(ModelForMock{Float64}())
        model = ModelForCachingOptimizer{Float64}()
        m = MOIU.CachingOptimizer(model, s)
        @test MOI.isempty(m)
        @test MOIU.state(m) == MOIU.AttachedOptimizer
        @test MOIU.mode(m) == MOIU.Automatic
    end
    @testset "Non-empty optimizer" begin
        s = MOIU.MockOptimizer(ModelForMock{Float64}())
        MOI.addvariable!(s)
        model = ModelForCachingOptimizer{Float64}()
        @test MOI.isempty(model)
        @test !MOI.isempty(s)
        @test_throws AssertionError MOIU.CachingOptimizer(model, s)
    end
    @testset "Non-empty model" begin
        s = MOIU.MockOptimizer(ModelForMock{Float64}())
        model = ModelForCachingOptimizer{Float64}()
        MOI.addvariable!(model)
        @test !MOI.isempty(model)
        @test MOI.isempty(s)
        @test_throws AssertionError MOIU.CachingOptimizer(model, s)
    end
end

for state in (MOIU.NoOptimizer, MOIU.EmptyOptimizer, MOIU.AttachedOptimizer)
    @testset "Optimization tests in state $state and mode $mode" for mode in (MOIU.Manual, MOIU.Automatic)
        m = MOIU.CachingOptimizer(ModelForCachingOptimizer{Float64}(), mode)
        if state != MOIU.NoOptimizer
            s = MOIU.MockOptimizer(ModelForMock{Float64}())
            MOIU.resetoptimizer!(m, s)
            if state == MOIU.AttachedOptimizer
                MOIU.attachoptimizer!(m)
            end
        end
        @test MOIU.state(m) == state
        @test MOIU.mode(m) == mode
        config = MOIT.TestConfig(solve=false)
        MOIT.contlineartest(m, config)
    end
end
