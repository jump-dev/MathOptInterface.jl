using Test

using MathOptInterface
const MOI = MathOptInterface
const MOIT = MathOptInterface.Test
const MOIU = MathOptInterface.Utilities
const MOIB = MathOptInterface.Bridges

include("../utilities.jl")

mock = MOIU.MockOptimizer(MOIU.UniversalFallback(MOIU.Model{Float64}()))
config = MOIT.TestConfig()

@testset "RelativeEntropy" begin
    bridged_mock = MOIB.Constraint.RelativeEntropy{Float64}(mock)

    MOIT.basic_constraint_tests(bridged_mock, config,
        include = [(F, MOI.RelativeEntropyCone) for F in [
            MOI.VectorOfVariables, MOI.VectorAffineFunction{Float64}, MOI.VectorQuadraticFunction{Float64}
        ]])

    entr1 = 2 * log(2)
    entr2 = 3 * log(3 / 5)
    var_primal = [entr1 + entr2, entr1, entr2]
    exp_duals = [[0.5, 0, 0.5], [0.5, 0, 0.5]]
    mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, var_primal,
        (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) => [[1.0]],
        (MOI.VectorAffineFunction{Float64}, MOI.ExponentialCone) => exp_duals)

    MOIT.relentr1test(bridged_mock, config)

    @testset "Test mock model" begin
        var_names = ["u", "y1", "y2"]
        MOI.set(mock, MOI.VariableName(), MOI.get(mock, MOI.ListOfVariableIndices()), var_names)
        greater = MOI.get(mock, MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}}())
        @test length(greater) == 1
        MOI.set(mock, MOI.ConstraintName(), greater[1], "greater")
        exp = MOI.get(mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.ExponentialCone}())
        @test length(exp) == 2
        MOI.set(mock, MOI.ConstraintName(), exp[1], "exp1")
        MOI.set(mock, MOI.ConstraintName(), exp[2], "exp2")

        s = """
        variables: u, y1, y2
        greater: u + -1.0y1 + -1.0y2 >= 0.0
        exp1: [-1.0y1, 2.0, 1.0] in MathOptInterface.ExponentialCone()
        exp2: [-1.0y2, 3.0, 5.0] in MathOptInterface.ExponentialCone()
        minobjective: u
        """
        model = MOIU.Model{Float64}()
        MOIU.loadfromstring!(model, s)
        MOIU.test_models_equal(mock, model, var_names, ["greater", "exp1", "exp2"])
    end

    @testset "Test bridged model" begin
        var_names = ["u"]
        MOI.set(bridged_mock, MOI.VariableName(), MOI.get(bridged_mock, MOI.ListOfVariableIndices()), var_names)
        relentr = MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.RelativeEntropyCone}())
        @test length(relentr) == 1
        MOI.set(bridged_mock, MOI.ConstraintName(), relentr[1], "relentr")

        s = """
        variables: u
        relentr: [u, 1.0, 5.0, 2.0, 3.0] in MathOptInterface.RelativeEntropyCone(5)
        minobjective: u
        """
        model = MOIU.Model{Float64}()
        MOIU.loadfromstring!(model, s)
        MOIU.test_models_equal(bridged_mock, model, var_names, ["relentr"])
    end

    ci = first(MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.RelativeEntropyCone}()))
    test_delete_bridge(bridged_mock, ci, 1, (
        (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}, 0),
        (MOI.VectorAffineFunction{Float64}, MOI.ExponentialCone, 0)))
end
