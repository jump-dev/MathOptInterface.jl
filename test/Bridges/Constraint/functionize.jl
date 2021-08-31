module TestConstraintFunctionize

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

function test_scalar_functionize()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    bridged_mock = MOI.Bridges.Constraint.ScalarFunctionize{Float64}(mock)
    x = MOI.add_variable(bridged_mock)
    y = MOI.add_variable(bridged_mock)
    ci = MOI.add_constraint(bridged_mock, x, MOI.GreaterThan(0.0))
    @test MOI.get(bridged_mock, MOI.ConstraintFunction(), ci) ≈ x
    MOI.set(bridged_mock, MOI.ConstraintFunction(), ci, y)
    @test MOI.get(bridged_mock, MOI.ConstraintFunction(), ci) ≈ y
    @test MOI.get(bridged_mock, MOI.ConstraintSet(), ci) == MOI.GreaterThan(0.0)
    MOI.set(bridged_mock, MOI.ConstraintSet(), ci, MOI.GreaterThan(1.0))
    @test MOI.get(bridged_mock, MOI.ConstraintSet(), ci) == MOI.GreaterThan(1.0)
    _test_delete_bridge(
        bridged_mock,
        ci,
        2,
        ((MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}, 0),),
    )
    MOI.Test.runtests(
        bridged_mock,
        MOI.Test.Config(),
        include = [
            "test_basic_VariableIndex_GreaterThan",
            "test_basic_VariableIndex_LessThan",
        ],
    )
    for T in [Int, Float64], S in [MOI.GreaterThan{T}, MOI.LessThan{T}]
        @test MOI.Bridges.added_constraint_types(
            MOI.Bridges.Constraint.ScalarFunctionizeBridge{T,S},
        ) == [(MOI.ScalarAffineFunction{T}, S)]
    end
    return
end

function test_scalar_functionize_linear2()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    bridged_mock = MOI.Bridges.Constraint.ScalarFunctionize{Float64}(mock)
    MOI.Utilities.set_mock_optimize!(
        mock,
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            [1, 0],
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) =>
                [-1],
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) =>
                [0, 1],
            constraint_basis_status = [
                (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) => [MOI.NONBASIC],
                (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) => [MOI.BASIC, MOI.NONBASIC],
            ],
            variable_basis_status = [MOI.BASIC, MOI.NONBASIC_AT_LOWER],
        ),
    )
    MOI.Test.test_linear_integration_2(
        bridged_mock,
        MOI.Test.Config(
            exclude = Any[MOI.VariableBasisStatus, MOI.ConstraintBasisStatus],
        ),
    )
    cis = MOI.get(
        bridged_mock,
        MOI.ListOfConstraintIndices{
            MOI.VariableIndex,
            MOI.GreaterThan{Float64},
        }(),
    )
    for attr in [MOI.ConstraintPrimalStart(), MOI.ConstraintDualStart()]
        for ci in cis
            @test MOI.supports(bridged_mock, attr, typeof(ci))
            MOI.set(bridged_mock, attr, ci, 2.0)
            @test MOI.get(bridged_mock, attr, ci) == 2.0
        end
    end
    ci = cis[1]
    attr = MOI.Test.UnknownConstraintAttribute()
    BT = MOI.Bridges.Constraint.ScalarFunctionizeBridge{
        Float64,
        MOI.GreaterThan{Float64},
    }
    err = ArgumentError(
        "Bridge of type `$(BT)` does not support setting the attribute " *
        "`$attr` because `MOIB.Constraint.invariant_under_function_conversion($attr)` returns `false`.",
    )
    @test_throws err MOI.set(bridged_mock, attr, ci, 1.0)
    err = ArgumentError(
        "Bridge of type `$(BT)` does not support accessing the attribute " *
        "`$attr` because `MOIB.Constraint.invariant_under_function_conversion($attr)` returns `false`.",
    )
    @test_throws err MOI.get(bridged_mock, attr, ci)
    for (i, ci) in enumerate(cis)
        _test_delete_bridge(
            bridged_mock,
            ci,
            2,
            ((MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}, 0),),
            num_bridged = 3 - i,
        )
    end
    return
end

function test_vector_functionize()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    bridged_mock = MOI.Bridges.Constraint.VectorFunctionize{Float64}(mock)
    x = MOI.add_variable(bridged_mock)
    y = MOI.add_variable(bridged_mock)
    z = MOI.add_variable(bridged_mock)
    v1 = MOI.VectorOfVariables([x, y, z])
    v2 = MOI.VectorOfVariables([x, y, y])
    ci = MOI.add_constraint(bridged_mock, v1, MOI.PowerCone(0.1))
    @test MOI.get(bridged_mock, MOI.ConstraintFunction(), ci) ≈ v1
    MOI.set(bridged_mock, MOI.ConstraintFunction(), ci, v2)
    @test MOI.get(bridged_mock, MOI.ConstraintFunction(), ci) ≈ v2
    @test MOI.get(bridged_mock, MOI.ConstraintSet(), ci) == MOI.PowerCone(0.1)
    MOI.set(bridged_mock, MOI.ConstraintSet(), ci, MOI.PowerCone(0.2))
    @test MOI.get(bridged_mock, MOI.ConstraintSet(), ci) == MOI.PowerCone(0.2)
    _test_delete_bridge(
        bridged_mock,
        ci,
        3,
        ((MOI.VectorAffineFunction{Float64}, MOI.PowerCone{Float64}, 0),),
    )

    MOI.Test.runtests(
        bridged_mock,
        MOI.Test.Config(),
        include = [
            "test_basic_VectorOfVariables_Nonnegatives",
            "test_basic_VectorOfVariables_Nonpositives",
        ],
    )
    for T in [Int, Float64], S in [MOI.Nonnegatives, MOI.Nonpositives]
        @test MOI.Bridges.added_constraint_types(
            MOI.Bridges.Constraint.VectorFunctionizeBridge{T,S},
        ) == [(MOI.VectorAffineFunction{T}, S)]
    end
    return
end

function test_vector_functionize_lin1v()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    bridged_mock = MOI.Bridges.Constraint.VectorFunctionize{Float64}(mock)
    mock.optimize! =
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            [1.0, 0.0, 2.0],
            (MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives) =>
                [[0, 2, 0]],
            (MOI.VectorAffineFunction{Float64}, MOI.Zeros) => [[-3, -1]],
        )
    MOI.Test.test_conic_linear_VectorOfVariables(
        bridged_mock,
        MOI.Test.Config(),
    )
    ci = first(
        MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{
                MOI.VectorOfVariables,
                MOI.Nonnegatives,
            }(),
        ),
    )
    func = MOI.get(bridged_mock, MOI.ConstraintFunction(), ci)
    MOI.delete(bridged_mock, func.variables[2])
    new_func = MOI.VectorOfVariables(func.variables[[1, 3]])
    @test MOI.get(bridged_mock, MOI.ConstraintFunction(), ci) == new_func
    @test MOI.get(bridged_mock, MOI.ConstraintSet(), ci) == MOI.Nonnegatives(2)
    for attr in [MOI.ConstraintPrimalStart(), MOI.ConstraintDualStart()]
        @test MOI.supports(bridged_mock, attr, typeof(ci))
        MOI.set(bridged_mock, attr, ci, [1.0, 2.0])
        @test MOI.get(bridged_mock, attr, ci) == [1.0, 2.0]
    end
    _test_delete_bridge(
        bridged_mock,
        ci,
        2,
        ((MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives, 0),),
    )
    return
end

end  # module

TestConstraintFunctionize.runtests()
