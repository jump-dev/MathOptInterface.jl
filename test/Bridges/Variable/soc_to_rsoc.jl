using Test

using MathOptInterface
const MOI = MathOptInterface
const MOIT = MathOptInterface.DeprecatedTest
const MOIU = MathOptInterface.Utilities
const MOIB = MathOptInterface.Bridges

include("../utilities.jl")

mock = MOIU.MockOptimizer(MOIU.Model{Float64}())
config = MOIT.Config()

bridged_mock = MOIB.Variable.SOCtoRSOC{Float64}(mock)

@testset "soc1v" begin
    mock.optimize! =
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [1 / √2 + 1 / 2, 1 / √2 - 1 / 2, 1 / √2],
            (MOI.VectorAffineFunction{Float64}, MOI.ZeroCone) => [[-√2]],
        )
    MOIT.soc1vtest(bridged_mock, config)

    ceqs = MOI.get(
        mock,
        MOI.ListOfConstraintIndices{
            MOI.VectorAffineFunction{Float64},
            MOI.ZeroCone,
        }(),
    )
    @test length(ceqs) == 1
    MOI.set(bridged_mock, MOI.ConstraintName(), ceqs[1], "ceq")

    @testset "Test mock model" begin
        var_names = ["a", "b", "c"]
        MOI.set(
            mock,
            MOI.VariableName(),
            MOI.get(mock, MOI.ListOfVariableIndices()),
            var_names,
        )
        rsocs = MOI.get(
            mock,
            MOI.ListOfConstraintIndices{
                MOI.VectorOfVariables,
                MOI.RotatedSecondOrderCone,
            }(),
        )
        @test length(rsocs) == 1
        MOI.set(mock, MOI.ConstraintName(), rsocs[1], "rsoc")

        invs2 = 1 / √2
        s = """
        variables: a, b, c
        rsoc: [a, b, c] in MathOptInterface.RotatedSecondOrderCone(3)
        ceq: [$invs2*a + $invs2*b + -1.0] in MathOptInterface.ZeroCone(1)
        maxobjective: $invs2*a + -$invs2*b + c
        """
        model = MOIU.Model{Float64}()
        MOIU.loadfromstring!(model, s)
        MOIU.test_models_equal(mock, model, var_names, ["rsoc", "ceq"])
    end

    @testset "Test bridged model" begin
        var_names = ["x", "y", "z"]
        MOI.set(
            bridged_mock,
            MOI.VariableName(),
            MOI.get(bridged_mock, MOI.ListOfVariableIndices()),
            var_names,
        )
        socs = MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{
                MOI.VectorOfVariables,
                MOI.SecondOrderCone,
            }(),
        )
        @test length(socs) == 1
        MOI.set(bridged_mock, MOI.ConstraintName(), socs[1], "soc")

        s = """
        variables: x, y, z
        soc: [x, y, z] in MathOptInterface.SecondOrderCone(3)
        ceq: [x + -1.0] in MathOptInterface.ZeroCone(1)
        maxobjective: y + z
        """
        model = MOIU.Model{Float64}()
        MOIU.loadfromstring!(model, s)
        MOIU.test_models_equal(bridged_mock, model, var_names, ["soc", "ceq"])
    end

    @testset "Delete" begin
        xyz = MOI.get(bridged_mock, MOI.ListOfVariableIndices())

        message = string(
            "Cannot delete variable as it is constrained with other",
            " variables in a `MOI.VectorOfVariables`.",
        )
        for i in eachindex(xyz)
            err = MOI.DeleteNotAllowed(xyz[i], message)
            @test_throws err MOI.delete(bridged_mock, xyz[i])
        end

        _test_delete_bridged_variables(
            bridged_mock,
            xyz,
            MOI.SecondOrderCone,
            3,
            ((MOI.VectorOfVariables, MOI.RotatedSecondOrderCone, 0),),
        )
    end
end
