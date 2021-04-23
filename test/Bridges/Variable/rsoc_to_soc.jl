using Test

using MathOptInterface
const MOI = MathOptInterface
const MOIT = MathOptInterface.Test
const MOIU = MathOptInterface.Utilities
const MOIB = MathOptInterface.Bridges

include("../utilities.jl")

mock = MOIU.MockOptimizer(MOIU.Model{Float64}())
config = MOIT.TestConfig()

bridged_mock = MOIB.Variable.RSOCtoSOC{Float64}(mock)

@testset "rotatedsoc4" begin
    mock.optimize! =
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [√2, 0.0, 1.0, 1.0],
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) =>
                [-1.0],
        )
    MOIT.rotatedsoc4test(bridged_mock, config)

    ceqs = MOI.get(
        mock,
        MOI.ListOfConstraintIndices{
            MOI.ScalarAffineFunction{Float64},
            MOI.LessThan{Float64},
        }(),
    )
    @test length(ceqs) == 1
    MOI.set(bridged_mock, MOI.ConstraintName(), ceqs[1], "c")

    @testset "Test mock model" begin
        var_names = ["a", "b", "c", "d"]
        MOI.set(
            mock,
            MOI.VariableName(),
            MOI.get(mock, MOI.ListOfVariableIndices()),
            var_names,
        )
        socs = MOI.get(
            mock,
            MOI.ListOfConstraintIndices{
                MOI.VectorOfVariables,
                MOI.SecondOrderCone,
            }(),
        )
        @test length(socs) == 1
        MOI.set(mock, MOI.ConstraintName(), socs[1], "soc")

        s2 = √2
        s = """
        variables: a, b, c, d
        soc: [a, b, c, d] in MathOptInterface.SecondOrderCone(4)
        c: $s2*a <= 2.0
        maxobjective: c + d
        """
        model = MOIU.Model{Float64}()
        MOIU.loadfromstring!(model, s)
        MOIU.test_models_equal(mock, model, var_names, ["soc", "c"])
    end

    @testset "Test bridged model" begin
        var_names = ["t", "u", "x", "y"]
        MOI.set(
            bridged_mock,
            MOI.VariableName(),
            MOI.get(bridged_mock, MOI.ListOfVariableIndices()),
            var_names,
        )
        rsocs = MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{
                MOI.VectorOfVariables,
                MOI.RotatedSecondOrderCone,
            }(),
        )
        @test length(rsocs) == 1
        MOI.set(bridged_mock, MOI.ConstraintName(), rsocs[1], "rsoc")

        s = """
        variables: t, u, x, y
        rsoc: [t, u, x, y] in MathOptInterface.RotatedSecondOrderCone(4)
        c: t + u <= 2.0
        maxobjective: x + y
        """
        model = MOIU.Model{Float64}()
        MOIU.loadfromstring!(model, s)
        MOIU.test_models_equal(bridged_mock, model, var_names, ["rsoc", "c"])
    end

    @testset "Delete" begin
        tuxy = MOI.get(bridged_mock, MOI.ListOfVariableIndices())

        message = string(
            "Cannot delete variable as it is constrained with other",
            " variables in a `MOI.VectorOfVariables`.",
        )
        for i in eachindex(tuxy)
            err = MOI.DeleteNotAllowed(tuxy[i], message)
            @test_throws err MOI.delete(bridged_mock, tuxy[i])
        end

        test_delete_bridged_variables(
            bridged_mock,
            tuxy,
            MOI.RotatedSecondOrderCone,
            4,
            ((MOI.VectorOfVariables, MOI.SecondOrderCone, 0),),
        )
    end
end
