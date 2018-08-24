@MOIU.model ModelForCachingOptimizer (ZeroOne, Integer) (EqualTo, GreaterThan, LessThan, Interval) (Zeros, Nonnegatives, Nonpositives, SecondOrderCone, RotatedSecondOrderCone, GeometricMeanCone, ExponentialCone, DualExponentialCone, PositiveSemidefiniteConeTriangle, RootDetConeTriangle, LogDetConeTriangle) () (SingleVariable,) (ScalarAffineFunction,ScalarQuadraticFunction) (VectorOfVariables,) (VectorAffineFunction,)

@testset "CachingOptimizer Manual mode" begin
    m = MOIU.CachingOptimizer(ModelForCachingOptimizer{Float64}(), MOIU.Manual)
    @test MOIU.state(m) == MOIU.NoOptimizer

    s = MOIU.MockOptimizer(ModelForMock{Float64}())
    @test MOI.isempty(s)
    MOIU.resetoptimizer!(m, s)
    @test MOIU.state(m) == MOIU.EmptyOptimizer

    v = MOI.add_variable(m)
    x = MOI.add_variables(m, 2)
    saf = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0, 2.0, 3.0], [v; x]), 0.0)
    @test MOI.supports(m, MOI.ObjectiveFunction{typeof(saf)}())
    MOI.set(m, MOI.ObjectiveFunction{typeof(saf)}(), saf)
    @test MOI.get(m, MOIU.AttributeFromModelCache(MOI.ObjectiveFunction{typeof(saf)}())) ≈ saf
    @test MOI.get(m, MOI.ObjectiveFunction{typeof(saf)}()) ≈ saf

    @test_throws AssertionError MOI.optimize!(m)

    MOIU.attachoptimizer!(m)
    @test MOIU.state(m) == MOIU.AttachedOptimizer
    @test MOI.get(m, MOIU.AttributeFromOptimizer(MOI.ObjectiveFunction{typeof(saf)}())) ≈ saf

    @test MOI.supports(m, MOI.ObjectiveSense())
    MOI.set(m, MOI.ObjectiveSense(), MOI.MaxSense)
    @test MOI.get(m, MOI.ObjectiveSense()) == MOI.MaxSense
    @test MOI.get(m, MOIU.AttributeFromModelCache(MOI.ObjectiveSense())) == MOI.MaxSense
    @test MOI.get(m, MOIU.AttributeFromOptimizer(MOI.ObjectiveSense())) == MOI.MaxSense

    @test !MOI.supports(m, MOI.NumberOfVariables())

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

    # ModelForMock doesn't support RotatedSecondOrderCone
    @test !MOI.supportsconstraint(m, MOI.VectorOfVariables, MOI.RotatedSecondOrderCone)

    @test MOI.supportsconstraint(m.model_cache, MOI.SingleVariable, MOI.LessThan{Float64})
    @test MOI.supportsconstraint(m.optimizer.inner_model, MOI.SingleVariable, MOI.LessThan{Float64})
    @test MOI.supportsconstraint(m.optimizer, MOI.SingleVariable, MOI.LessThan{Float64})
    @test MOI.supportsconstraint(m, MOI.SingleVariable, MOI.LessThan{Float64})
    lb = MOI.add_constraint(m, MOI.SingleVariable(v), MOI.LessThan(10.0))
    @test MOI.supports(m, MOI.ConstraintSet(), typeof(lb))
    MOI.set(m, MOI.ConstraintSet(), lb, MOI.LessThan(11.0))
    @test MOI.get(m, MOI.ConstraintSet(), lb) == MOI.LessThan(11.0)
    @test MOI.get(m, MOI.ConstraintFunction(), lb) == MOI.SingleVariable(v)

    MOIU.dropoptimizer!(m)
    @test MOIU.state(m) == MOIU.NoOptimizer

    @test MOI.supports(m, MOI.ConstraintSet(), typeof(lb))
    MOI.set(m, MOI.ConstraintSet(), lb, MOI.LessThan(12.0))
    @test MOI.get(m, MOI.ConstraintSet(), lb) == MOI.LessThan(12.0)

    MOI.delete!(m, x[2])
    @test_throws MOI.InvalidIndex{typeof(x[2])} MOI.delete!(m, x[2])
    @test !MOI.isvalid(m, x[2])

    # TODO: test more constraint modifications


end

@testset "CachingOptimizer Automatic mode" begin
    m = MOIU.CachingOptimizer(ModelForCachingOptimizer{Float64}(), MOIU.Automatic)
    @test MOIU.state(m) == MOIU.NoOptimizer

    v = MOI.add_variable(m)
    @test MOI.supports(m, MOI.VariableName(), typeof(v))
    MOI.set(m, MOI.VariableName(), v, "v")
    @test MOI.get(m, MOI.VariableName(), v) == "v"

    s = MOIU.MockOptimizer(ModelForMock{Float64}())
    @test MOI.isempty(s)
    MOIU.resetoptimizer!(m, s)
    @test MOIU.state(m) == MOIU.EmptyOptimizer

    saf = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, v)], 0.0)
    @test MOI.supports(m, MOI.ObjectiveFunction{typeof(saf)}())
    MOI.set(m, MOI.ObjectiveFunction{typeof(saf)}(), saf)
    @test MOI.get(m, MOIU.AttributeFromModelCache(MOI.ObjectiveFunction{typeof(saf)}())) ≈ saf

    MOI.optimize!(m)
    @test MOIU.state(m) == MOIU.AttachedOptimizer
    @test MOI.get(m, MOIU.AttributeFromOptimizer(MOI.ResultCount())) == 0

    @test MOI.get(m, MOI.VariableName(), v) == "v"
    @test MOI.get(m, MOIU.AttributeFromModelCache(MOI.VariableName()), v) == "v"
    # The caching optimizer does not copy names
    @test MOI.get(m, MOIU.AttributeFromOptimizer(MOI.VariableName()), v) == ""

    @test MOI.supports(m, MOI.ObjectiveSense())
    MOI.set(m, MOI.ObjectiveSense(), MOI.MaxSense)
    @test MOI.get(m, MOIU.AttributeFromModelCache(MOI.ObjectiveSense())) == MOI.MaxSense
    @test MOI.get(m, MOIU.AttributeFromOptimizer(MOI.ObjectiveSense())) == MOI.MaxSense

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
        MOI.modify!(m, MOI.ObjectiveFunction{typeof(saf)}(),
                    MOI.ScalarConstantChange(1.0))
        s.modify_allowed = true
        @test MOIU.state(m) == MOIU.EmptyOptimizer
        MOIU.attachoptimizer!(m)
        @test MOIU.state(m) == MOIU.AttachedOptimizer
        ci = MOI.add_constraint(m, saf, MOI.EqualTo(0.0))
        s.modify_allowed = false
        MOI.modify!(m, ci, MOI.ScalarCoefficientChange(v, 1.0))
        s.modify_allowed = true
        @test MOIU.state(m) == MOIU.EmptyOptimizer
        MOIU.attachoptimizer!(m)
        @test MOIU.state(m) == MOIU.AttachedOptimizer
    end

    # Simulate that constraints cannot be added
    @testset "Add constraint not allowed" begin
        s.add_con_allowed = false
        MOI.add_constraint(m, MOI.VectorOfVariables([v]), MOI.SecondOrderCone(1))
        s.add_con_allowed = true
        @test MOIU.state(m) == MOIU.EmptyOptimizer
        MOI.empty!(m)
        @test MOIU.state(m) == MOIU.AttachedOptimizer
    end

    @testset "Add variable not allowed" begin
        s.add_var_allowed = false # Simulate optimizer that cannot add variables incrementally
        MOI.add_variable(m)
        @test MOIU.state(m) == MOIU.EmptyOptimizer
        @test_throws MOI.AddVariableNotAllowed MOIU.attachoptimizer!(m)
        @test MOIU.state(m) == MOIU.EmptyOptimizer

        s.add_var_allowed = true
        MOIU.attachoptimizer!(m)
        @test MOIU.state(m) == MOIU.AttachedOptimizer
        s.add_var_allowed = false
        MOI.add_variables(m, 2)
        s.add_var_allowed = true
        @test MOIU.state(m) == MOIU.EmptyOptimizer
    end

    @testset "Delete not allowed" begin
        vi = MOI.add_variable(m)
        s.delete_allowed = false # Simulate optimizer that cannot delete variable
        MOI.delete!(m, vi)
        s.delete_allowed = true
        @test MOIU.state(m) == MOIU.EmptyOptimizer
        MOIU.attachoptimizer!(m)
        @test MOIU.state(m) == MOIU.AttachedOptimizer

        vi = MOI.add_variable(m)
        ci = MOI.add_constraint(m, MOI.SingleVariable(vi), MOI.EqualTo(0.0))
        s.delete_allowed = false # Simulate optimizer that cannot delete constraint
        MOI.delete!(m, ci)
        s.delete_allowed = true
        @test MOIU.state(m) == MOIU.EmptyOptimizer
        MOIU.attachoptimizer!(m)
        @test MOIU.state(m) == MOIU.AttachedOptimizer
    end

end

@testset "CachingOptimizer constructor with optimizer" begin
    @testset "Empty model and optimizer" begin
        s = MOIU.MockOptimizer(ModelForMock{Float64}())
        model = ModelForCachingOptimizer{Float64}()
        m = MOIU.CachingOptimizer(model, s)
        @test m isa MOIU.CachingOptimizer{typeof(s), typeof(model)}
        @test MOI.isempty(m)
        @test MOIU.state(m) == MOIU.AttachedOptimizer
        @test MOIU.mode(m) == MOIU.Automatic
    end
    @testset "Non-empty optimizer" begin
        s = MOIU.MockOptimizer(ModelForMock{Float64}())
        MOI.add_variable(s)
        model = ModelForCachingOptimizer{Float64}()
        @test MOI.isempty(model)
        @test !MOI.isempty(s)
        @test_throws AssertionError MOIU.CachingOptimizer(model, s)
    end
    @testset "Non-empty model" begin
        s = MOIU.MockOptimizer(ModelForMock{Float64}())
        model = ModelForCachingOptimizer{Float64}()
        MOI.add_variable(model)
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

        @testset "Name test" begin
            MOIT.nametest(m)
        end

        @testset "Copy test" begin
            MOIT.failcopytestc(m)
            MOIT.failcopytestia(m)
            MOIT.failcopytestva(m)
            MOIT.failcopytestca(m)
            MOIT.copytest(m, Model{Float64}())
        end

        config = MOIT.TestConfig(solve=false)
        @testset "Unit" begin
            MOIT.unittest(m, config)
        end
        @testset "Continuous Linear" begin
            MOIT.contlineartest(m, config)
        end
    end
end
