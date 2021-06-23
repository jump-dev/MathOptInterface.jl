using Test

using MathOptInterface
const MOI = MathOptInterface
const MOIT = MathOptInterface.Test
const MOIU = MathOptInterface.Utilities
const MOIB = MathOptInterface.Bridges

include("../utilities.jl")

mock = MOIU.MockOptimizer(MOIU.UniversalFallback(MOIU.Model{Float64}()))
config = MOIT.Config()

@testset "RelativeEntropy" begin
    bridged_mock = MOIB.Constraint.RelativeEntropy{Float64}(mock)

    MOIT.basic_constraint_tests(
        bridged_mock,
        config,
        include = [
            (F, MOI.RelativeEntropyCone) for F in [
                MOI.VectorOfVariables,
                MOI.VectorAffineFunction{Float64},
                MOI.VectorQuadraticFunction{Float64},
            ]
        ],
    )

    entr1 = 2 * log(2)
    entr2 = 3 * log(3 / 5)
    var_primal = [entr1 + entr2, entr1, entr2]
    exps_duals = [[-1, log(0.5) - 1, 2], [-1, log(5 / 3) - 1, 0.6]]
    mock.optimize! =
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            var_primal,
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) =>
                [1.0],
            (MOI.VectorAffineFunction{Float64}, MOI.ExponentialCone) =>
                exps_duals,
        )

    MOIT.test_conic_RelativeEntropyCone(bridged_mock, config)

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

    @testset "Test mock model" begin
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
        model = MOIU.Model{Float64}()
        MOIU.loadfromstring!(model, s)
        MOIU.test_models_equal(
            mock,
            model,
            vcat(var_names, "y1", "y2"),
            ["greater", "exps1", "exps2"],
        )
    end

    @testset "Test bridged model" begin
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
        model = MOIU.Model{Float64}()
        MOIU.loadfromstring!(model, s)
        MOIU.test_models_equal(bridged_mock, model, var_names, ["relentr"])
    end

    ci = first(
        MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{
                MOI.VectorAffineFunction{Float64},
                MOI.RelativeEntropyCone,
            }(),
        ),
    )

    @testset "$attr" for attr in [
        MOI.ConstraintPrimalStart(),
        MOI.ConstraintDualStart(),
    ]
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

    test_delete_bridge(
        bridged_mock,
        ci,
        1,
        (
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}, 0),
            (MOI.VectorAffineFunction{Float64}, MOI.ExponentialCone, 0),
        ),
    )
end
