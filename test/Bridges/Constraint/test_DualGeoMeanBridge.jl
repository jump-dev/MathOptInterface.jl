# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConstraintDualGeoMean

using Test

import MathOptInterface as MOI

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

function test_DualGeoMean()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    config = MOI.Test.Config()
    bridged_mock = MOI.Bridges.Constraint.DualGeoMean{Float64}(mock)
    MOI.Test.test_basic_VectorOfVariables_DualGeometricMeanCone(
        bridged_mock,
        config,
    )
    MOI.empty!(bridged_mock)
    MOI.Test.test_basic_VectorAffineFunction_DualGeometricMeanCone(
        bridged_mock,
        config,
    )
    MOI.empty!(bridged_mock)
    MOI.Test.test_basic_VectorQuadraticFunction_DualGeometricMeanCone(
        bridged_mock,
        config,
    )
    return
end

function test_conic_DualGeometricMeanCone_VectorAffineFunction()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    config = MOI.Test.Config()
    bridged_mock = MOI.Bridges.Constraint.DualGeoMean{Float64}(mock)
    var_primal = [-3, 1, 1, 1]
    geomean_dual = copy(var_primal)
    mock.optimize! =
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            var_primal,
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) =>
                [-1],
            (MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives) => [[0]],
            (MOI.VectorAffineFunction{Float64}, MOI.GeometricMeanCone) =>
                [geomean_dual],
        )

    MOI.Test.test_conic_DualGeometricMeanCone_VectorAffineFunction(
        bridged_mock,
        config,
    )
    var_names = ["t", "x", "y", "z"]
    MOI.set(
        bridged_mock,
        MOI.VariableName(),
        MOI.get(bridged_mock, MOI.ListOfVariableIndices()),
        var_names,
    )
    nonneg = MOI.get(
        mock,
        MOI.ListOfConstraintIndices{
            MOI.VectorAffineFunction{Float64},
            MOI.Nonnegatives,
        }(),
    )
    geomean = MOI.get(
        mock,
        MOI.ListOfConstraintIndices{
            MOI.VectorAffineFunction{Float64},
            MOI.GeometricMeanCone,
        }(),
    )
    @test length(nonneg) == 1
    MOI.set(mock, MOI.ConstraintName(), nonneg[1], "nonneg")
    @test length(geomean) == 1
    MOI.set(mock, MOI.ConstraintName(), geomean[1], "geomean")
    less = MOI.get(
        mock,
        MOI.ListOfConstraintIndices{
            MOI.ScalarAffineFunction{Float64},
            MOI.LessThan{Float64},
        }(),
    )
    @test length(less) == 1
    MOI.set(mock, MOI.ConstraintName(), less[1], "less")

    s = """
    variables: t, x, y, z
    less: x + y + z in LessThan(3.0)
    nonneg: [-1 * t] in Nonnegatives(1)
    geomean: [-1/3 * t, x, y, z] in GeometricMeanCone(4)
    minobjective: t
    """
    model = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(model, s)
    MOI.Test.util_test_models_equal(
        mock,
        model,
        var_names,
        ["less", "nonneg", "geomean"],
    )
    dualgeomean = MOI.get(
        bridged_mock,
        MOI.ListOfConstraintIndices{
            MOI.VectorAffineFunction{Float64},
            MOI.DualGeometricMeanCone,
        }(),
    )
    @test length(dualgeomean) == 1
    MOI.set(bridged_mock, MOI.ConstraintName(), dualgeomean[1], "dualgeomean")
    less = MOI.get(
        bridged_mock,
        MOI.ListOfConstraintIndices{
            MOI.ScalarAffineFunction{Float64},
            MOI.LessThan{Float64},
        }(),
    )
    @test length(less) == 1
    MOI.set(bridged_mock, MOI.ConstraintName(), less[1], "less")

    s = """
    variables: t, x, y, z
    less: x + y + z in LessThan(3.0)
    dualgeomean: [1.0t, x, y, z] in DualGeometricMeanCone(4)
    minobjective: t
    """
    model = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(model, s)
    MOI.Test.util_test_models_equal(
        bridged_mock,
        model,
        var_names,
        ["less", "dualgeomean"],
    )
    ci = first(
        MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{
                MOI.VectorAffineFunction{Float64},
                MOI.DualGeometricMeanCone,
            }(),
        ),
    )
    _test_delete_bridge(
        bridged_mock,
        ci,
        4,
        (
            (MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives, 0),
            (MOI.VectorAffineFunction{Float64}, MOI.GeometricMeanCone, 0),
        ),
    )
    return
end

function test_runtests()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.DualGeoMeanBridge,
        """
        variables: u, w1, w2
        [u, w1, w2] in DualGeometricMeanCone(3)
        """,
        """
        variables: u, w1, w2
        [-1/2 * u, w1, w2] in GeometricMeanCone(3)
        [-1 * u] in Nonnegatives(1)
        """,
    )
    return
end

end  # module

TestConstraintDualGeoMean.runtests()
