using Test

using MathOptInterface
const MOI = MathOptInterface
const MOIT = MathOptInterface.Test
const MOIU = MathOptInterface.Utilities
const MOIB = MathOptInterface.Bridges

include("../utilities.jl")

mock = MOIU.MockOptimizer(MOIU.Model{Float64}())
config = MOIT.TestConfig()

@testset "GeoMean" begin
    bridged_mock = MOIB.Constraint.GeoMean{Float64}(mock)

    MOIT.basic_constraint_tests(
        bridged_mock, config,
        include = [(F, S)
                   for F in [MOI.VectorOfVariables, MOI.VectorAffineFunction{Float64},
                             MOI.VectorQuadraticFunction{Float64}]
                   for S in [MOI.GeometricMeanCone]])

    mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [ones(4); 2; √2; √2])
    MOIT.geomean1vtest(bridged_mock, config)
    MOIT.geomean1ftest(bridged_mock, config)

    @testset "Test mock model" begin
        var_names = ["t", "x", "y", "z", "x21", "x11", "x12"]
        MOI.set(mock, MOI.VariableName(), MOI.get(mock, MOI.ListOfVariableIndices()), var_names)
        lessthan = MOI.get(mock, MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}}())
        @test length(lessthan) == 2
        # TODO is it OK to assume this order?
        MOI.set.(mock, MOI.ConstraintName(), lessthan, ["lessthan1", "lessthan2"])
        rsoc = MOI.get(mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.RotatedSecondOrderCone}())
        @test length(rsoc) == 3
        MOI.set.(mock, MOI.ConstraintName(), rsoc, ["rsoc21", "rsoc11", "rsoc12"])

        # TODO do we need explicit nonnegativity on x11, x12, x21 ?
        s = """
        variables: t, x, y, z, x11, x12, x21
        lessthan1: t + -0.5 * x21 in MathOptInterface.LessThan(0.0)
        lessthan2: x + y + z in MathOptInterface.LessThan(3.0)
        rsoc11: [1.0x, y, x11] in MathOptInterface.RotatedSecondOrderCone(3)
        rsoc12: [z, 0.5 * x21, x12] in MathOptInterface.RotatedSecondOrderCone(3)
        rsoc21: [1.0x11, x12, x21] in MathOptInterface.RotatedSecondOrderCone(3)
        maxobjective: t
        """
        model = MOIU.Model{Float64}()
        MOIU.loadfromstring!(model, s)
        MOIU.test_models_equal(mock, model, var_names, ["lessthan1", "lessthan2", "rsoc11", "rsoc12", "rsoc21"])
    end

    @testset "Test bridged model" begin
        var_names = ["t", "x", "y", "z"]
        MOI.set(bridged_mock, MOI.VariableName(), MOI.get(bridged_mock, MOI.ListOfVariableIndices()), var_names)
        lessthan = MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}}())
        @test length(lessthan) == 1
        MOI.set(bridged_mock, MOI.ConstraintName(), lessthan[1], "lessthan")
        geomean = MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.GeometricMeanCone}())
        @test length(geomean) == 1
        MOI.set(bridged_mock, MOI.ConstraintName(), geomean[1], "geomean")

        s = """
        variables: t, x, y, z
        lessthan: x + y + z in MathOptInterface.LessThan(3.0)
        geomean: [1.0t, x, y, z] in MathOptInterface.GeometricMeanCone(4)
        maxobjective: t
        """
        model = MOIU.Model{Float64}()
        MOIU.loadfromstring!(model, s)
        MOIU.test_models_equal(bridged_mock, model, var_names, ["lessthan", "geomean"])
    end

    # Dual is not yet implemented for GeoMean bridge
    ci = first(MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.GeometricMeanCone}()))
    test_delete_bridge(bridged_mock, ci, 4, ((MOI.VectorAffineFunction{Float64}, MOI.RotatedSecondOrderCone, 0),
                                            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}, 1)))

end
