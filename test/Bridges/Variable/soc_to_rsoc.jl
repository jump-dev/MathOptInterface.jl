module TestVariableSOCtoRSOC

using Test

using MathOptInterface
const MOI = MathOptInterface

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

include("../utilities.jl")

function test_soc1v()
    mock = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    bridged_mock = MOI.Bridges.Variable.SOCtoRSOC{Float64}(mock)
    mock.optimize! =
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            [1 / √2 + 1 / 2, 1 / √2 - 1 / 2, 1 / √2],
            (MOI.VectorAffineFunction{Float64}, MOI.Zeros) => [[-√2]],
        )
    MOI.Test.test_conic_SecondOrderCone_VectorOfVariables(
        bridged_mock,
        MOI.Test.Config(),
    )
    ceqs = MOI.get(
        mock,
        MOI.ListOfConstraintIndices{
            MOI.VectorAffineFunction{Float64},
            MOI.Zeros,
        }(),
    )
    @test length(ceqs) == 1
    MOI.set(bridged_mock, MOI.ConstraintName(), ceqs[1], "ceq")

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
    ceq: [$invs2*a + $invs2*b + -1.0] in MathOptInterface.Zeros(1)
    maxobjective: $invs2*a + -$invs2*b + c
    """
    model = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(model, s)
    MOI.Test.test_models_equal(mock, model, var_names, ["rsoc", "ceq"])
    var_names = ["x", "y", "z"]
    MOI.set(
        bridged_mock,
        MOI.VariableName(),
        MOI.get(bridged_mock, MOI.ListOfVariableIndices()),
        var_names,
    )
    socs = MOI.get(
        bridged_mock,
        MOI.ListOfConstraintIndices{MOI.VectorOfVariables,MOI.SecondOrderCone}(),
    )
    @test length(socs) == 1
    MOI.set(bridged_mock, MOI.ConstraintName(), socs[1], "soc")

    s = """
    variables: x, y, z
    soc: [x, y, z] in MathOptInterface.SecondOrderCone(3)
    ceq: [x + -1.0] in MathOptInterface.Zeros(1)
    maxobjective: y + z
    """
    model = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(model, s)
    MOI.Test.test_models_equal(bridged_mock, model, var_names, ["soc", "ceq"])
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
    return
end

end  # module

TestVariableSOCtoRSOC.runtests()
