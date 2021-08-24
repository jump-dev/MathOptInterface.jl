module TestConstraintRelativeEntropyToExponential

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

function test_RelativeEntropy()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    config = MOI.Test.Config()
    bridged_mock = MOI.Bridges.Constraint.RelativeEntropy{Float64}(mock)
    MOI.Test.test_basic_VectorOfVariables_RelativeEntropyCone(
        bridged_mock,
        config,
    )
    MOI.empty!(bridged_mock)
    MOI.Test.test_basic_VectorAffineFunction_RelativeEntropyCone(
        bridged_mock,
        config,
    )
    MOI.empty!(bridged_mock)
    MOI.Test.test_basic_VectorQuadraticFunction_RelativeEntropyCone(
        bridged_mock,
        config,
    )
    MOI.empty!(bridged_mock)
    entr1 = 2 * log(2)
    entr2 = 3 * log(3 / 5)
    var_primal = [entr1 + entr2, entr1, entr2]
    exps_duals = [[-1, log(0.5) - 1, 2], [-1, log(5 / 3) - 1, 0.6]]
    mock.optimize! =
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            var_primal,
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) =>
                [1.0],
            (MOI.VectorAffineFunction{Float64}, MOI.ExponentialCone) =>
                exps_duals,
        )

    MOI.Test.test_conic_RelativeEntropyCone(bridged_mock, config)
    var_names = ["u"]
    MOI.set(
        bridged_mock,
        MOI.VariableName(),
        MOI.get(bridged_mock, MOI.ListOfVariableIndices()),
        var_names,
    )
    greater = MOI.get(
        mock,
        MOI.ListOfConstraintIndices{
            MOI.ScalarAffineFunction{Float64},
            MOI.GreaterThan{Float64},
        }(),
    )
    exps = MOI.get(
        mock,
        MOI.ListOfConstraintIndices{
            MOI.VectorAffineFunction{Float64},
            MOI.ExponentialCone,
        }(),
    )
    (y1, y2) = MOI.get(mock, MOI.ListOfVariableIndices())[2:3]
    MOI.set(mock, MOI.VariableName(), y1, "y1")
    MOI.set(mock, MOI.VariableName(), y2, "y2")
    @test length(greater) == 1
    MOI.set(mock, MOI.ConstraintName(), greater[1], "greater")
    @test length(exps) == 2
    MOI.set(mock, MOI.ConstraintName(), exps[1], "exps1")
    MOI.set(mock, MOI.ConstraintName(), exps[2], "exps2")

    s = """
    variables: u, y1, y2
    greater: u + -1.0y1 + -1.0y2 >= 0.0
    exps1: [-1.0y1, 2.0, 1.0] in MathOptInterface.ExponentialCone()
    exps2: [-1.0y2, 3.0, 5.0] in MathOptInterface.ExponentialCone()
    minobjective: u
    """
    model = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(model, s)
    MOI.Test.test_models_equal(
        mock,
        model,
        vcat(var_names, "y1", "y2"),
        ["greater", "exps1", "exps2"],
    )
    relentr = MOI.get(
        bridged_mock,
        MOI.ListOfConstraintIndices{
            MOI.VectorAffineFunction{Float64},
            MOI.RelativeEntropyCone,
        }(),
    )
    @test length(relentr) == 1
    MOI.set(bridged_mock, MOI.ConstraintName(), relentr[1], "relentr")

    s = """
    variables: u
    relentr: [u, 1.0, 5.0, 2.0, 3.0] in MathOptInterface.RelativeEntropyCone(5)
    minobjective: u
    """
    model = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(model, s)
    MOI.Test.test_models_equal(bridged_mock, model, var_names, ["relentr"])
    ci = first(
        MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{
                MOI.VectorAffineFunction{Float64},
                MOI.RelativeEntropyCone,
            }(),
        ),
    )
    for attr in [MOI.ConstraintPrimalStart(), MOI.ConstraintDualStart()]
        @test MOI.supports(bridged_mock, attr, typeof(ci))
        value =
            (attr isa MOI.ConstraintPrimalStart) ?
            [entr1 + entr2 + 1, 1, 5, 2, 3] :
            [2, 2, 0.6, log(0.5) - 1, log(5 / 3) - 1]
        MOI.set(bridged_mock, attr, ci, value)
        @test MOI.get(bridged_mock, attr, ci) ≈ value
        if attr isa MOI.ConstraintPrimalStart
            @test MOI.get(mock, MOI.VariablePrimalStart(), y1) == entr1
            @test MOI.get(mock, MOI.VariablePrimalStart(), y2) == entr2
            @test MOI.get(mock, attr, greater[1]) == 1
            @test MOI.get(mock, attr, exps[1]) == [-entr1, 2, 1]
            @test MOI.get(mock, attr, exps[2]) == [-entr2, 3, 5]
        else
            @test MOI.get(mock, attr, greater[1]) == 2
            for i in 1:2
                @test MOI.get(mock, attr, exps[i]) ==
                      [value[1], value[3+i], value[1+i]]
            end
        end
    end
    _test_delete_bridge(
        bridged_mock,
        ci,
        1,
        (
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}, 0),
            (MOI.VectorAffineFunction{Float64}, MOI.ExponentialCone, 0),
        ),
    )
    return
end

end  # module

TestConstraintRelativeEntropyToExponential.runtests()
