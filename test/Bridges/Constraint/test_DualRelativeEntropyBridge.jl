# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConstraintDualRelativeEntropy

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

function test_DualRelativeEntropy()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    config = MOI.Test.Config()
    bridged_mock = MOI.Bridges.Constraint.DualRelativeEntropy{Float64}(mock)
    MOI.Test.test_basic_VectorOfVariables_DualRelativeEntropyCone(
        bridged_mock,
        config,
    )
    MOI.empty!(bridged_mock)
    MOI.Test.test_basic_VectorAffineFunction_DualRelativeEntropyCone(
        bridged_mock,
        config,
    )
    MOI.empty!(bridged_mock)
    MOI.Test.test_basic_VectorQuadraticFunction_DualRelativeEntropyCone(
        bridged_mock,
        config,
    )
    MOI.empty!(bridged_mock)
    var_primal = [2.0, 3/5, log(1/2)-1, log(5/3)-1]
    exps_duals = [[2log(1/2), 2.0, 1.0], [3log(5/3), 3.0, 5.0]]
    mock.optimize! =
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            var_primal,
            (MOI.VectorAffineFunction{Float64}, MOI.DualExponentialCone) =>
                exps_duals,
        )

    MOI.Test.test_conic_DualRelativeEntropyCone(bridged_mock, config)
    var_names = ["v1", "v2", "w1", "w2"]
    MOI.set(
        bridged_mock,
        MOI.VariableName(),
        MOI.get(bridged_mock, MOI.ListOfVariableIndices()),
        var_names,
    )
    exps = MOI.get(
        mock,
        MOI.ListOfConstraintIndices{
            MOI.VectorAffineFunction{Float64},
            MOI.DualExponentialCone,
        }(),
    )
    @test length(exps) == 2
    MOI.set(mock, MOI.ConstraintName(), exps[1], "exps1")
    MOI.set(mock, MOI.ConstraintName(), exps[2], "exps2")

    s = """
    variables: v1, v2, w1, w2
    exps1: [-1.0, w1, v1] in DualExponentialCone()
    exps2: [-1.0, w2, v2] in DualExponentialCone()
    maxobjective: -1.0v1 + -5.0v2 + -2.0w1 + -3.0w2
    """
    model = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(model, s)
    MOI.Test.util_test_models_equal(mock, model, var_names, ["exps1", "exps2"])
    relentr = MOI.get(
        bridged_mock,
        MOI.ListOfConstraintIndices{
            MOI.VectorAffineFunction{Float64},
            MOI.DualRelativeEntropyCone,
        }(),
    )
    @test length(relentr) == 1
    MOI.set(bridged_mock, MOI.ConstraintName(), relentr[1], "relentr")

    s = """
    variables: v1, v2, w1, w2
    relentr: [1.0, v1, v2, w1, w2] in DualRelativeEntropyCone(5)
    maxobjective: -1.0v1 + -5.0v2 + -2.0w1 + -3.0w2
    """
    model = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(model, s)
    MOI.Test.util_test_models_equal(bridged_mock, model, var_names, ["relentr"])
    ci = first(
        MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{
                MOI.VectorAffineFunction{Float64},
                MOI.DualRelativeEntropyCone,
            }(),
        ),
    )
    _test_delete_bridge(
        bridged_mock,
        ci,
        4,
        ((MOI.VectorAffineFunction{Float64}, MOI.DualExponentialCone, 0),),
    )
    return
end

function test_runtests()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.DualRelativeEntropyBridge,
        """
        variables: u, v1, v2, w1, w2
        [u, v1, v2, w1, w2] in DualRelativeEntropyCone(5)
        """,
        """
        variables: u, v1, v2, w1, w2
        [-1.0 * u, w1, v1] in DualExponentialCone()
        [-1.0 * u, w2, v2] in DualExponentialCone()
        """,
    )
    return
end

end  # module

TestConstraintDualRelativeEntropy.runtests()
