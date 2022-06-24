# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConstraintSquare

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

function test_Square()
    mock = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    config = MOI.Test.Config()
    bridged_mock = MOI.Bridges.Constraint.Square{Float64}(mock)
    MOI.Test.runtests(
        bridged_mock,
        config,
        include = [
            "test_basic_VectorOfVariables_PositiveSemidefiniteConeSquare",
            "test_basic_VectorAffineFunction_PositiveSemidefiniteConeSquare",
            "test_basic_VectorQuadraticFunction_PositiveSemidefiniteConeSquare",
        ],
    )
    MOI.empty!(bridged_mock)
    mock.optimize! =
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            ones(4),
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                [2, 2],
        )
    MOI.Test.test_conic_PositiveSemidefiniteConeSquare_VectorOfVariables(
        bridged_mock,
        config,
    )
    MOI.empty!(bridged_mock)
    mock.optimize! =
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            ones(4),
            (
                MOI.VectorAffineFunction{Float64},
                MOI.PositiveSemidefiniteConeTriangle,
            ) => [[1, -1, 1]],
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                [2, 2],
        )
    MOI.Test.test_conic_PositiveSemidefiniteConeSquare_VectorAffineFunction(
        bridged_mock,
        config,
    )
    ci = first(
        MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{
                MOI.VectorAffineFunction{Float64},
                MOI.PositiveSemidefiniteConeSquare,
            }(),
        ),
    )
    _test_delete_bridge(
        bridged_mock,
        ci,
        4,
        (
            (
                MOI.VectorAffineFunction{Float64},
                MOI.PositiveSemidefiniteConeTriangle,
                0,
            ),
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}, 1),
        ),
    )
    return
end

function test_runtests()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.SquareBridge,
        """
        variables: x11, x21, x12, x22
        [x11, x21, x12, x22] in PositiveSemidefiniteConeSquare(2)
        """,
        """
        variables: x11, x21, x12, x22
        [x11, x12, x22] in PositiveSemidefiniteConeTriangle(2)
        x12 + -1.0 * x21 == 0.0
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.SquareBridge,
        """
        variables: x11, x12, x22
        [x11, x12, x12, x22] in PositiveSemidefiniteConeSquare(2)
        """,
        """
        variables: x11, x12, x22
        [x11, x12, x22] in PositiveSemidefiniteConeTriangle(2)
        """,
    )
    return
end

function test_symmetric_square()
    mock = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    mock.optimize! =
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            ones(5),
            (
                MOI.VectorAffineFunction{Float64},
                MOI.PositiveSemidefiniteConeTriangle,
            ) => [[0.0, 0.0, 1.0, 0.0, 1.0, 1.0]],
        )
    model = MOI.Bridges.Constraint.Square{Float64}(mock)
    x = MOI.add_variables(model, 2)
    MOI.add_constraint.(model, x, MOI.GreaterThan(0.0))
    X = MOI.add_variables(model, 3)
    y = [1.0, x[1], x[2], x[1], X[1], X[2], x[2], X[2], X[3]]
    g = MOI.Utilities.operate(vcat, Float64, 1.0 * y...)
    c = MOI.add_constraint(model, g, MOI.PositiveSemidefiniteConeSquare(3))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    f = sum(1.0 * x) + sum(1.0 * X) + X[2]
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    MOI.optimize!(model)
    dual = MOI.get(model, MOI.ConstraintDual(), c)
    @test reshape(dual, 3, 3) == Float64[0 0 0; 0 1 1; 0 1 1]
    return
end

function test_runtests_symmetric_logdet()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.SquareBridge,
        """
        variables: t, u, x11, x21, x22
        [t, u, x11, x21, x21, x22] in LogDetConeSquare(2)
        """,
        """
        variables: t, u, x11, x21, x22
        [t, u, x11, x21, x22] in LogDetConeTriangle(2)
        """,
    )
    return
end

function test_runtests_non_symmetric_logdet()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.SquareBridge,
        """
        variables: t, u, x11, x12, x21, x22
        [t, u, x11, x21, x12, x22] in LogDetConeSquare(2)
        """,
        """
        variables: t, u, x11, x12, x21, x22
        [t, u, x11, x12, x22] in LogDetConeTriangle(2)
        1.0 * x12 + -1.0 * x21 == 0.0
        """,
    )
    return
end

function test_runtests_symmetric_rootdet()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.SquareBridge,
        """
        variables: t, x11, x21, x22
        [t, x11, x21, x21, x22] in RootDetConeSquare(2)
        """,
        """
        variables: t, x11, x21, x22
        [t, x11, x21, x22] in RootDetConeTriangle(2)
        """,
    )
    return
end

function test_runtests_non_symmetric_rootdet()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.SquareBridge,
        """
        variables: t, x11, x12, x21, x22
        [t, x11, x21, x12, x22] in RootDetConeSquare(2)
        """,
        """
        variables: t, x11, x12, x21, x22
        [t, x11, x12, x22] in RootDetConeTriangle(2)
        x12 + -1.0 * x21 == 0.0
        """,
    )
    return
end

function test_conic_LogDetConeSquare_VectorOfVariables()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.Model{Float64}();
        eval_variable_constraint_dual = false,
    )
    model = MOI.Bridges.Constraint.Square{Float64}(mock)
    mock.optimize! =
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            [0.0, 1.0, 0.0, 0.0, 1.0, 1.0],
            (MOI.VariableIndex, MOI.EqualTo{Float64}) => [2.0],
            (MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives) =>
                [[1.0, 1.0]],
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                [0.0],
            (MOI.VectorOfVariables, MOI.LogDetConeTriangle) =>
                [Float64[-1, -2, 1, 0, 1]],
        )
    config = MOI.Test.Config()
    MOI.Test.test_conic_LogDetConeSquare_VectorOfVariables(model, config)
    return
end

function test_square_warning()
    inner = MOI.Utilities.Model{Float64}()
    model = MOI.Bridges.Constraint.Square{Float64}(inner)
    x = 1.0 * MOI.add_variable(model)
    f = MOI.Utilities.operate(vcat, Float64, x, x + 1e-9, x, x)
    @test_logs(
        (:warn,),
        MOI.add_constraint(model, f, MOI.PositiveSemidefiniteConeSquare(2)),
    )
    return
end

end  # module

TestConstraintSquare.runtests()
