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

end  # module

TestConstraintSquare.runtests()
