using Test

using MathOptInterface
const MOI = MathOptInterface
const MOIT = MathOptInterface.Test
const MOIU = MathOptInterface.Utilities
const MOIB = MathOptInterface.Bridges

include("../utilities.jl")

mock = MOIU.MockOptimizer(MOIU.UniversalFallback(MOIU.Model{Float64}()))
config = MOIT.TestConfig()

@testset "GeoMeantoRelEntr" begin
    bridged_mock = MOIB.Constraint.GeoMeantoRelEntr{Float64}(mock)

    MOIT.basic_constraint_tests(bridged_mock, config,
        include = [(F, MOI.GeometricMeanCone) for F in [
            MOI.VectorOfVariables, MOI.VectorAffineFunction{Float64}, MOI.VectorQuadraticFunction{Float64}
        ]])

    var_primal = [1, 1, 1, 1, 0]
    relentr_dual = [1, 1, 1, 1, -1, -1, -1] / 3
    mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, var_primal,
        (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) => [inv(3)],
        (MOI.VectorOfVariables, MOI.Nonnegatives) => [[1.0]],
        (MOI.VectorAffineFunction{Float64}, MOI.RelativeEntropyCone) => [relentr_dual])

    # MOIT.geomean1vtest(bridged_mock, config)
    MOIT.geomean1ftest(bridged_mock, config)

    var_names = ["t", "x", "y", "z"]
    MOI.set(bridged_mock, MOI.VariableName(), MOI.get(bridged_mock, MOI.ListOfVariableIndices()), var_names)

    nonneg = MOI.get(mock, MOI.ListOfConstraintIndices{MOI.VectorOfVariables, MOI.Nonnegatives}())
    relentr = MOI.get(mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.RelativeEntropyCone}())
    aux = MOI.get(mock, MOI.ListOfVariableIndices())[end]

    @testset "Test mock model" begin
        MOI.set(mock, MOI.VariableName(), aux, "aux")
        @test length(nonneg) == 1
        MOI.set(mock, MOI.ConstraintName(), nonneg[1], "nonneg")
        @test length(relentr) == 1
        MOI.set(mock, MOI.ConstraintName(), relentr[1], "relentr")
        less = MOI.get(mock, MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}}())
        @test length(less) == 1
        MOI.set(mock, MOI.ConstraintName(), less[1], "less")

        s = """
        variables: t, x, y, z, aux
        less: x + y + z in MathOptInterface.LessThan(3.0)
        nonneg: [aux] in MathOptInterface.Nonnegatives(1)
        relentr: [0.0, x, y, z, t + aux, t + aux, t + aux] in MathOptInterface.RelativeEntropyCone(7)
        maxobjective: t
        """
        model = MOIU.Model{Float64}()
        MOIU.loadfromstring!(model, s)
        MOIU.test_models_equal(mock, model, vcat(var_names, "aux"), ["less", "nonneg", "relentr"])
    end

    @testset "Test bridged model" begin
        geomean = MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.GeometricMeanCone}())
        @test length(geomean) == 1
        MOI.set(bridged_mock, MOI.ConstraintName(), geomean[1], "geomean")
        less = MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}}())
        @test length(less) == 1
        MOI.set(bridged_mock, MOI.ConstraintName(), less[1], "less")

        s = """
        variables: t, x, y, z
        less: x + y + z in MathOptInterface.LessThan(3.0)
        geomean: [1.0t, x, y, z] in MathOptInterface.GeometricMeanCone(4)
        maxobjective: t
        """
        model = MOIU.Model{Float64}()
        MOIU.loadfromstring!(model, s)
        MOIU.test_models_equal(bridged_mock, model, var_names, ["less", "geomean"])
    end

    ci = first(MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.GeometricMeanCone}()))

    @testset "$attr" for attr in [MOI.ConstraintPrimalStart(), MOI.ConstraintDualStart()]
        @test MOI.supports(bridged_mock, attr, typeof(ci))
        value = (attr isa MOI.ConstraintPrimalStart) ? ones(4) : vcat(-1, fill(inv(3), 3))
        MOI.set(bridged_mock, attr, ci, value)
        @test MOI.get(bridged_mock, attr, ci) ≈ value
        if attr isa MOI.ConstraintPrimalStart
            @test MOI.get(mock, MOI.VariablePrimalStart(), aux) == 0
            @test MOI.get(mock, attr, nonneg[1]) == [0]
            @test MOI.get(mock, attr, relentr[1]) == [0, 1, 1, 1, 1, 1, 1]
        else
            @test MOI.get(mock, attr, nonneg[1]) == [1]
            @test MOI.get(mock, attr, relentr[1]) == relentr_dual
        end
    end

    test_delete_bridge(bridged_mock, ci, 4, (
        (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}, 1),
        (MOI.VectorOfVariables, MOI.Nonnegatives, 0),
        (MOI.VectorAffineFunction{Float64}, MOI.RelativeEntropyCone, 0)))
end
