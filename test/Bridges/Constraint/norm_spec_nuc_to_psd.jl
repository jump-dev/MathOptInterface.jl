using Test

using MathOptInterface
const MOI = MathOptInterface
const MOIT = MathOptInterface.DeprecatedTest
const MOIU = MathOptInterface.Utilities
const MOIB = MathOptInterface.Bridges

include("../utilities.jl")

mock = MOIU.MockOptimizer(MOIU.UniversalFallback(MOIU.Model{Float64}()))
config = MOIT.Config()

@testset "NormSpectral" begin
    bridged_mock = MOIB.Constraint.NormSpectral{Float64}(mock)

    MOIT.basic_constraint_tests(
        bridged_mock,
        config,
        include = [
            (F, MOI.NormSpectralCone) for F in [
                MOI.VectorOfVariables,
                MOI.VectorAffineFunction{Float64},
                MOI.VectorQuadraticFunction{Float64},
            ]
        ],
    )

    inv6 = inv(6)
    rt3 = sqrt(3)
    invrt12 = inv(2 * rt3)
    psd_dual = [
        inv6,
        -invrt12,
        inv6,
        invrt12,
        -invrt12,
        inv6,
        0,
        0,
        0,
        0,
        -invrt12,
        invrt12,
        -invrt12,
        0,
        0.5,
    ]
    mock.optimize! =
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [rt3],
            (
                MOI.VectorAffineFunction{Float64},
                MOI.PositiveSemidefiniteConeTriangle,
            ) => [psd_dual],
        )

    MOIT.normspec1test(bridged_mock, config)

    var_names = ["t"]
    MOI.set(
        bridged_mock,
        MOI.VariableName(),
        MOI.get(bridged_mock, MOI.ListOfVariableIndices()),
        var_names,
    )

    psd = MOI.get(
        mock,
        MOI.ListOfConstraintIndices{
            MOI.VectorAffineFunction{Float64},
            MOI.PositiveSemidefiniteConeTriangle,
        }(),
    )

    @testset "Test mock model" begin
        @test length(psd) == 1
        MOI.set(mock, MOI.ConstraintName(), psd[1], "psd")

        s = """
        variables: t
        psd: [t, 0.0, t, 0.0, 0.0, t, 1.0, 1.0, 0.0, t, 1.0, -1.0, 1.0, 0.0, t] in MathOptInterface.PositiveSemidefiniteConeTriangle(5)
        minobjective: t
        """
        model = MOIU.Model{Float64}()
        MOIU.loadfromstring!(model, s)
        MOIU.test_models_equal(mock, model, var_names, ["psd"])
    end

    @testset "Test bridged model" begin
        spec = MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{
                MOI.VectorAffineFunction{Float64},
                MOI.NormSpectralCone,
            }(),
        )
        @test length(spec) == 1
        MOI.set(bridged_mock, MOI.ConstraintName(), spec[1], "spec")

        s = """
        variables: t
        spec: [t, 1.0, 1.0, 1.0, -1.0, 0.0, 1.0] in MathOptInterface.NormSpectralCone(2, 3)
        minobjective: t
        """
        model = MOIU.Model{Float64}()
        MOIU.loadfromstring!(model, s)
        MOIU.test_models_equal(bridged_mock, model, var_names, ["spec"])
    end

    ci = first(
        MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{
                MOI.VectorAffineFunction{Float64},
                MOI.NormSpectralCone,
            }(),
        ),
    )

    attr = MOI.ConstraintPrimalStart()
    @testset "$attr" begin
        @test MOI.supports(bridged_mock, attr, typeof(ci))
        value = Float64[4, 1, 1, 1, -1, 0, 1]
        MOI.set(bridged_mock, attr, ci, value)
        @test MOI.get(bridged_mock, attr, ci) ≈ value
        @test MOI.get(mock, attr, psd[1]) ==
              Float64[4, 0, 4, 0, 0, 4, 1, 1, 0, 4, 1, -1, 1, 0, 4]
    end

    _test_delete_bridge(
        bridged_mock,
        ci,
        1,
        ((
            MOI.VectorAffineFunction{Float64},
            MOI.PositiveSemidefiniteConeTriangle,
            0,
        ),),
    )
end

@testset "NormNuclear" begin
    bridged_mock = MOIB.Constraint.NormNuclear{Float64}(mock)

    MOIT.basic_constraint_tests(
        bridged_mock,
        config,
        include = [
            (F, MOI.NormNuclearCone) for F in [
                MOI.VectorOfVariables,
                MOI.VectorAffineFunction{Float64},
                MOI.VectorQuadraticFunction{Float64},
            ]
        ],
    )

    rt2 = sqrt(2)
    rt3 = sqrt(3)
    invrt2 = inv(rt2)
    invrt3 = inv(rt3)
    var_primal = [
        rt2 + rt3,
        invrt2 + invrt3,
        invrt2 - invrt3,
        invrt2 + invrt3,
        invrt3,
        -invrt3,
        invrt3,
        rt2,
        0,
        rt3,
    ]
    invrt8 = inv(sqrt(8))
    invrt12 = inv(sqrt(12))
    psd_dual = [
        0.5,
        0,
        0.5,
        0,
        0,
        0.5,
        -invrt8,
        -invrt8,
        0,
        invrt8,
        -invrt12,
        invrt12,
        -invrt12,
        0,
        0.5,
    ]
    mock.optimize! =
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            var_primal,
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) =>
                [[1.0]],
            (
                MOI.VectorAffineFunction{Float64},
                MOI.PositiveSemidefiniteConeTriangle,
            ) => [psd_dual],
        )

    MOIT.normnuc1test(bridged_mock, config)

    greater = MOI.get(
        mock,
        MOI.ListOfConstraintIndices{
            MOI.ScalarAffineFunction{Float64},
            MOI.GreaterThan{Float64},
        }(),
    )
    psd = MOI.get(
        mock,
        MOI.ListOfConstraintIndices{
            MOI.VectorAffineFunction{Float64},
            MOI.PositiveSemidefiniteConeTriangle,
        }(),
    )

    @testset "Test mock model" begin
        var_names =
            ["t", "U11", "U12", "U22", "U31", "U32", "U33", "V11", "V12", "V22"]
        MOI.set(
            mock,
            MOI.VariableName(),
            MOI.get(mock, MOI.ListOfVariableIndices()),
            var_names,
        )
        @test length(greater) == 1
        MOI.set(mock, MOI.ConstraintName(), greater[1], "greater")
        @test length(psd) == 1
        MOI.set(mock, MOI.ConstraintName(), psd[1], "psd")

        s = """
        variables: t, U11, U12, U22, U31, U32, U33, V11, V12, V22
        greater: t + -0.5U11 + -0.5U22 + -0.5U33 + -0.5V11 + -0.5V22 >= 0.0
        psd: [U11, U12, U22, U31, U32, U33, 1.0, 1.0, 0.0, V11, 1.0, -1.0, 1.0, V12, V22] in MathOptInterface.PositiveSemidefiniteConeTriangle(5)
        minobjective: t
        """
        model = MOIU.Model{Float64}()
        MOIU.loadfromstring!(model, s)
        MOIU.test_models_equal(mock, model, var_names, ["greater", "psd"])
    end

    @testset "Test bridged model" begin
        MOI.set(
            bridged_mock,
            MOI.VariableName(),
            MOI.get(bridged_mock, MOI.ListOfVariableIndices()),
            ["t"],
        )
        nuc = MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{
                MOI.VectorAffineFunction{Float64},
                MOI.NormNuclearCone,
            }(),
        )
        @test length(nuc) == 1
        MOI.set(bridged_mock, MOI.ConstraintName(), nuc[1], "nuc")

        s = """
        variables: t
        nuc: [t, 1.0, 1.0, 1.0, -1.0, 0.0, 1.0] in MathOptInterface.NormNuclearCone(2, 3)
        minobjective: t
        """
        model = MOIU.Model{Float64}()
        MOIU.loadfromstring!(model, s)
        MOIU.test_models_equal(bridged_mock, model, ["t"], ["nuc"])
    end

    ci = first(
        MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{
                MOI.VectorAffineFunction{Float64},
                MOI.NormNuclearCone,
            }(),
        ),
    )

    attr = MOI.ConstraintDualStart()
    @testset "$attr" begin
        @test MOI.supports(bridged_mock, attr, typeof(ci))
        value = Float64[4, 1, 1, 1, -1, 0, 1]
        MOI.set(bridged_mock, attr, ci, value)
        @test MOI.get(bridged_mock, attr, ci) ≈ value
        @test MOI.get(mock, attr, greater[1]) == 4
        @test MOI.get(mock, attr, psd[1]) ==
              Float64[4, 0, 4, 0, 0, 4, 0.5, 0.5, 0, 4, 0.5, -0.5, 0.5, 0, 4]
    end

    _test_delete_bridge(
        bridged_mock,
        ci,
        1,
        (
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}, 0),
            (
                MOI.VectorAffineFunction{Float64},
                MOI.PositiveSemidefiniteConeTriangle,
                0,
            ),
        ),
    )
end
