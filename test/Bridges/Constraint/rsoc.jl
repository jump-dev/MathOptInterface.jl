module TestConstraintRSOC

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

function test_RSOC()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    config = MOI.Test.Config()
    bridged_mock = MOI.Bridges.Constraint.RSOC{Float64}(mock)
    MOI.Test.runtests(
        bridged_mock,
        config,
        include = [
            "test_basic_VectorOfVariables_RotatedSecondOrderCone",
            "test_basic_VectorAffineFunction_RotatedSecondOrderCone",
            "test_basic_VectorQuadraticFunction_RotatedSecondOrderCone",
        ],
    )
    MOI.empty!(bridged_mock)
    mock.optimize! =
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            [0.5, 1.0, 1 / √2, 1 / √2],
            (MOI.VariableIndex, MOI.EqualTo{Float64}) => [-√2, -1 / √2],
            (MOI.VectorAffineFunction{Float64}, MOI.SecondOrderCone) =>
                [[3 / 2, 1 / 2, -1.0, -1.0]],
        )
    MOI.Test.test_conic_RotatedSecondOrderCone_VectorOfVariables(
        bridged_mock,
        config,
    )
    MOI.empty!(bridged_mock)
    mock.optimize! =
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            [1 / √2, 1 / √2],
            (MOI.VectorAffineFunction{Float64}, MOI.SecondOrderCone) =>
                [[3 / 2, 1 / 2, -1.0, -1.0]],
        )
    MOI.Test.test_conic_RotatedSecondOrderCone_VectorAffineFunction(
        bridged_mock,
        config,
    )
    ci = first(
        MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{
                MOI.VectorAffineFunction{Float64},
                MOI.RotatedSecondOrderCone,
            }(),
        ),
    )
    for attr in [MOI.ConstraintPrimalStart(), MOI.ConstraintDualStart()]
        @test MOI.supports(bridged_mock, attr, typeof(ci))
        value = [√2, 1 / √2, -1.0, -1.0]
        MOI.set(bridged_mock, attr, ci, value)
        @test MOI.get(bridged_mock, attr, ci) ≈ value
    end
    _test_delete_bridge(
        bridged_mock,
        ci,
        2,
        ((MOI.VectorAffineFunction{Float64}, MOI.SecondOrderCone, 0),),
    )
    return
end

function test_SOCR()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    config = MOI.Test.Config()
    bridged_mock = MOI.Bridges.Constraint.SOCR{Float64}(mock)
    MOI.Test.runtests(
        bridged_mock,
        config,
        include = [
            "test_basic_VectorOfVariables_SecondOrderCone",
            "test_basic_VectorAffineFunction_SecondOrderCone",
            "test_basic_VectorQuadraticFunction_SecondOrderCone",
        ],
    )
    MOI.empty!(bridged_mock)
    mock.optimize! =
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            [1.0, 1 / √2, 1 / √2],
            (MOI.VectorAffineFunction{Float64}, MOI.RotatedSecondOrderCone) => [[1 - 1 / √2, 1 + 1 / √2, -1]],
            (MOI.VectorAffineFunction{Float64}, MOI.Zeros) => [[-√2]],
        )
    MOI.Test.test_conic_SecondOrderCone_VectorOfVariables(bridged_mock, config)
    MOI.empty!(bridged_mock)
    mock.optimize! =
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            [1.0, 1 / √2, 1 / √2],
            (MOI.VectorAffineFunction{Float64}, MOI.RotatedSecondOrderCone) => [[1 - 1 / √2, 1 + 1 / √2, -1]],
            (MOI.VectorAffineFunction{Float64}, MOI.Zeros) => [[-√2]],
        )
    MOI.Test.test_conic_SecondOrderCone_VectorAffineFunction(
        bridged_mock,
        config,
    )
    ci = first(
        MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{
                MOI.VectorAffineFunction{Float64},
                MOI.SecondOrderCone,
            }(),
        ),
    )
    @test !MOI.Utilities.is_canonical(
        MOI.get(bridged_mock, MOI.ConstraintFunction(), ci),
    )
    @test MOI.Utilities.is_canonical(
        MOI.get(bridged_mock, MOI.CanonicalConstraintFunction(), ci),
    )
    for attr in [MOI.ConstraintPrimalStart(), MOI.ConstraintDualStart()]
        @test MOI.supports(bridged_mock, attr, typeof(ci))
        value = [√2, 1 / √2, -1.0, -1.0]
        MOI.set(bridged_mock, attr, ci, value)
        @test MOI.get(bridged_mock, attr, ci) ≈ value
    end
    _test_delete_bridge(
        bridged_mock,
        ci,
        3,
        ((MOI.VectorAffineFunction{Float64}, MOI.RotatedSecondOrderCone, 0),),
    )
    return
end

end  # module

TestConstraintRSOC.runtests()
