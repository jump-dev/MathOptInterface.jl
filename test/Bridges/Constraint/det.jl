module TestConstraintDet

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

function test_LogDet_basic()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    bridged_mock = MOI.Bridges.Constraint.LogDet{Float64}(mock)
    MOI.Test.runtests(
        bridged_mock,
        MOI.Test.Config(),
        include = [
            "test_basic_VectorOfVariables_LogDetCone",
            "test_basic_VectorAffineFunction_LogDetCone",
            "test_basic_VectorQuadraticFunction_LogDetCone",
        ],
    )
    return
end

function _setup_LogDetConeTriangle(
    mock::MOI.Utilities.MockOptimizer,
    ::Type{F},
) where {F}
    var_primal = [0, 1, 0, 1, 1, 0, 1, 0, 0, 1]
    exp_duals = [[-1, -1, 1], [-1, -1, 1]]
    psd_dual = [1, 0, 1, -1, 0, 1, 0, -1, 0, 1]
    mock.optimize! =
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            var_primal,
            (MOI.VariableIndex, MOI.EqualTo{Float64}) => [2],
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) =>
                [-1],
            (MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives) =>
                [[1, 1]],
            (F, MOI.ExponentialCone) => exp_duals,
            (
                MOI.VectorAffineFunction{Float64},
                MOI.PositiveSemidefiniteConeTriangle,
            ) => [psd_dual],
        )
    return
end

function test_conic_LogDetConeTriangle()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
        eval_variable_constraint_dual = false,
    )
    bridged_mock = MOI.Bridges.Constraint.LogDet{Float64}(mock)
    config = MOI.Test.Config()
    _setup_LogDetConeTriangle(mock, MOI.VectorOfVariables)
    MOI.Test.test_conic_LogDetConeTriangle_VectorOfVariables(
        bridged_mock,
        config,
    )
    MOI.empty!(bridged_mock)
    _setup_LogDetConeTriangle(mock, MOI.VectorAffineFunction{Float64})
    MOI.Test.test_conic_LogDetConeTriangle_VectorAffineFunction(
        bridged_mock,
        config,
    )

    # set primal/dual start is not yet implemented for LogDet bridge
    ci = first(
        MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{
                MOI.VectorAffineFunction{Float64},
                MOI.LogDetConeTriangle,
            }(),
        ),
    )
    _test_delete_bridge(
        bridged_mock,
        ci,
        5,
        (
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}, 0),
            (MOI.VectorAffineFunction{Float64}, MOI.ExponentialCone, 0),
            (
                MOI.VectorAffineFunction{Float64},
                MOI.PositiveSemidefiniteConeTriangle,
                0,
            ),
        ),
    )
    return
end

function test_conic_LogDetConeTriangle_2()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    bridged_mock = MOI.Bridges.Constraint.LogDet{Float64}(mock)
    var_primal =
        [log(5), 3, 2, 2 / 3, 1, 1 / 3, 2.5, log(3), log(2 / 3), log(2.5)]
    exp_duals = [
        [-1, log(3) - 1, 1 / 3],
        [-1, log(2 / 3) - 1, 3 / 2],
        [-1, log(2.5) - 1, 0.4],
    ]
    psd_dual = [
        1,
        -1,
        1.6,
        0,
        -0.2,
        0.4,
        -1 / 3,
        0,
        0,
        1 / 3,
        1,
        -1.5,
        0,
        0,
        1.5,
        0,
        0.2,
        -0.4,
        0,
        0,
        0.4,
    ]
    mock.optimize! =
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            var_primal,
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) =>
                [-1.0],
            (MOI.VectorAffineFunction{Float64}, MOI.ExponentialCone) =>
                exp_duals,
            (
                MOI.VectorAffineFunction{Float64},
                MOI.PositiveSemidefiniteConeTriangle,
            ) => [psd_dual],
        )
    config = MOI.Test.Config()
    MOI.Test.test_conic_LogDetConeTriangle(bridged_mock, config)

    # set primal/dual start is not yet implemented for LogDet bridge
    ci = first(
        MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{
                MOI.VectorAffineFunction{Float64},
                MOI.LogDetConeTriangle,
            }(),
        ),
    )
    _test_delete_bridge(
        bridged_mock,
        ci,
        1,
        (
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}, 0),
            (MOI.VectorAffineFunction{Float64}, MOI.ExponentialCone, 0),
            (
                MOI.VectorAffineFunction{Float64},
                MOI.PositiveSemidefiniteConeTriangle,
                0,
            ),
        ),
    )
    return
end

function test_conic_RootDetConeTriangle()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    bridged_mock = MOI.Bridges.Constraint.RootDet{Float64}(mock)
    var_primal = [1, 1, 0, 1, 1, 0, 1]
    geomean_dual = [-1, 0.5, 0.5]
    psd_dual = [0.5, 0, 0.5, -0.5, 0, 0.5, 0, -0.5, 0, 0.5]
    mock.optimize! =
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            var_primal,
            (MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives) =>
                [[0.5, 0.5]],
            (MOI.VectorAffineFunction{Float64}, MOI.GeometricMeanCone) =>
                [geomean_dual],
            (
                MOI.VectorAffineFunction{Float64},
                MOI.PositiveSemidefiniteConeTriangle,
            ) => [psd_dual],
        )
    config = MOI.Test.Config()
    MOI.Test.test_conic_RootDetConeTriangle_VectorOfVariables(
        bridged_mock,
        config,
    )
    MOI.empty!(bridged_mock)
    MOI.Test.test_conic_RootDetConeTriangle_VectorAffineFunction(
        bridged_mock,
        config,
    )

    # set primal/dual start is not yet implemented for RootDet bridge
    ci = first(
        MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{
                MOI.VectorAffineFunction{Float64},
                MOI.RootDetConeTriangle,
            }(),
        ),
    )
    _test_delete_bridge(
        bridged_mock,
        ci,
        4,
        (
            (MOI.VectorAffineFunction{Float64}, MOI.RotatedSecondOrderCone, 0),
            (MOI.VectorAffineFunction{Float64}, MOI.GeometricMeanCone, 0),
            (
                MOI.VectorAffineFunction{Float64},
                MOI.PositiveSemidefiniteConeTriangle,
                0,
            ),
        ),
    )
    return
end

function test_RootDet_basic()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    bridged_mock = MOI.Bridges.Constraint.RootDet{Float64}(mock)
    MOI.Test.runtests(
        bridged_mock,
        MOI.Test.Config(),
        include = [
            "test_basic_VectorOfVariables_RootDetCone",
            "test_basic_VectorAffineFunction_RootDetCone",
            "test_basic_VectorQuadraticFunction_RootDetCone",
        ],
    )
    return
end

function test_conic_RootDetConeTriangle_2()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    bridged_mock = MOI.Bridges.Constraint.RootDet{Float64}(mock)
    var_primal = [5^inv(3), 3, 2, 2 / 3, 1, 1 / 3, 2.5]
    multiplier = 5^inv(3) / 3
    geomean_dual = vcat(-1, multiplier * [inv(3), 1.5, 0.4])
    psd_dual =
        multiplier * [
            1,
            -1,
            1.6,
            0,
            -0.2,
            0.4,
            -1 / 3,
            0,
            0,
            1 / 3,
            1,
            -1.5,
            0,
            0,
            1.5,
            0,
            0.2,
            -0.4,
            0,
            0,
            0.4,
        ]
    mock.optimize! =
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            var_primal,
            (MOI.VectorAffineFunction{Float64}, MOI.GeometricMeanCone) =>
                [geomean_dual],
            (
                MOI.VectorAffineFunction{Float64},
                MOI.PositiveSemidefiniteConeTriangle,
            ) => [psd_dual],
        )
    MOI.Test.test_conic_RootDetConeTriangle(bridged_mock, MOI.Test.Config())

    # set primal/dual start is not yet implemented for RootDet bridge
    ci = first(
        MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{
                MOI.VectorAffineFunction{Float64},
                MOI.RootDetConeTriangle,
            }(),
        ),
    )
    _test_delete_bridge(
        bridged_mock,
        ci,
        1,
        (
            (MOI.VectorAffineFunction{Float64}, MOI.RotatedSecondOrderCone, 0),
            (MOI.VectorAffineFunction{Float64}, MOI.GeometricMeanCone, 0),
            (
                MOI.VectorAffineFunction{Float64},
                MOI.PositiveSemidefiniteConeTriangle,
                0,
            ),
        ),
    )
    return
end

end  # module

TestConstraintDet.runtests()
