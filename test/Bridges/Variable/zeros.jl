module TestVariableZeros

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

function test_zeros()
    mock = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    bridged_mock = MOI.Bridges.Variable.Zeros{Float64}(mock)

    x, cx = MOI.add_constrained_variable(bridged_mock, MOI.GreaterThan(0.0))
    MOI.set(bridged_mock, MOI.VariableName(), x, "x")
    yz, cyz = MOI.add_constrained_variables(bridged_mock, MOI.Zeros(2))
    MOI.set(bridged_mock, MOI.VariableName(), yz, ["y", "z"])
    MOI.set(bridged_mock, MOI.ConstraintName(), cyz, "cyz")
    y, z = yz
    fx = MOI.SingleVariable(x)
    fy = MOI.SingleVariable(y)
    fz = MOI.SingleVariable(z)

    MOI.set(bridged_mock, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(bridged_mock, MOI.ObjectiveFunction{typeof(fx)}(), fx)
    @test MOI.get(bridged_mock, MOI.ObjectiveFunction{typeof(fx)}()) == fx

    # Test before adding affine constraints are affine expressions cannot be
    # unbridged when `Variable.ZerosBridge` is used.
    s = """
    variables: x, y, z
    x >= 0.0
    cyz: [y, z] in MathOptInterface.Zeros(2)
    minobjective: x
    """
    model = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(model, s)
    MOI.Utilities.test_models_equal(
        bridged_mock,
        model,
        ["x", "y", "z"],
        ["cyz"],
        [("x", MOI.GreaterThan{Float64}(0.0))],
    )

    c1, c2 = MOI.add_constraints(
        bridged_mock,
        [1.0fy + 1.0fz, 1.0fx + 1.0fy + 1.0fz],
        [MOI.EqualTo(0.0), MOI.GreaterThan(1.0)],
    )
    MOI.set(bridged_mock, MOI.ConstraintName(), c1, "con1")
    MOI.set(bridged_mock, MOI.ConstraintName(), c2, "con2")
    MOI.set(bridged_mock, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    obj = 1.0fx - 1.0fy - 1.0fz
    MOI.set(bridged_mock, MOI.ObjectiveFunction{typeof(obj)}(), obj)

    @test MOI.Bridges.Variable.unbridged_map(
        MOI.Bridges.bridge(bridged_mock, y),
        y,
        MOI.Bridges.IndexInVector(1),
    ) === nothing
    @test MOI.Bridges.Variable.unbridged_map(
        MOI.Bridges.bridge(bridged_mock, z),
        z,
        MOI.Bridges.IndexInVector(2),
    ) === nothing

    err = ErrorException(
        "Cannot delete constraint index of bridged constrained variables. Delete" *
        " the scalar variable or the vector of variables instead.",
    )
    @test_throws err MOI.delete(bridged_mock, cyz)

    err = ErrorException(
        "Cannot unbridge function because some variables are bridged by" *
        " variable bridges that do not support reverse mapping, e.g.," *
        " `ZerosBridge`.",
    )
    @test_throws err MOI.get(bridged_mock, MOI.ObjectiveFunction{typeof(obj)}())
    # With `c1`, the function does not contain any variable so it tests that it
    # also throws an error even if it never calls `variable_unbridged_function`.
    @test_throws err MOI.get(bridged_mock, MOI.ConstraintFunction(), c1)
    @test_throws err MOI.get(bridged_mock, MOI.ConstraintFunction(), c2)

    err = ArgumentError(
        "Variable bridge of type `MathOptInterface.Bridges.Variable.ZerosBridge{Float64}`" *
        " does not support accessing the attribute `MathOptInterface.Test.UnknownVariableAttribute()`.",
    )
    @test_throws err MOI.get(
        bridged_mock,
        MOI.Test.UnknownVariableAttribute(),
        y,
    )

    MOI.Utilities.set_mock_optimize!(
        mock,
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            [1.0],
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                0.0,
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) =>
                1.0,
        ),
    )
    MOI.optimize!(bridged_mock)
    @test MOI.get(bridged_mock, MOI.VariablePrimal(), x) == 1.0
    @test MOI.get(bridged_mock, MOI.VariablePrimal(), y) == 0.0
    @test MOI.get(bridged_mock, MOI.VariablePrimal(), z) == 0.0

    @test MOI.get(bridged_mock, MOI.ConstraintPrimal(), cyz) == zeros(2)

    @test MOI.get(bridged_mock, MOI.ConstraintDual(), cx) == 0.0
    @test MOI.get(bridged_mock, MOI.ConstraintDual(), c1) == 0.0
    @test MOI.get(bridged_mock, MOI.ConstraintDual(), c2) == 1.0
    bridge = MathOptInterface.Bridges.Variable.ZerosBridge{Float64}
    attr = MOI.ConstraintDual()
    err = ArgumentError(
        "Bridge of type `$(bridge)` does not support accessing " *
        "the attribute `$attr`. If you encountered this error " *
        "unexpectedly, it probably means your model has been " *
        "reformulated using the bridge, and you are attempting to query " *
        "an attribute that we haven't implemented yet for this bridge. " *
        "Please open an issue at https://github.com/jump-dev/MathOptInterface.jl/issues/new " *
        "and provide a reproducible example explaining what you were " *
        "trying to do.",
    )
    @test_throws err MOI.get(bridged_mock, attr, cyz)

    @test MOI.get(bridged_mock, MOI.ConstraintFunction(), cyz).variables == yz
    @test MOI.get(mock, MOI.NumberOfVariables()) == 1
    @test MOI.get(mock, MOI.ListOfVariableIndices()) == [x]
    @test MOI.get(bridged_mock, MOI.NumberOfVariables()) == 3
    @test MOI.get(bridged_mock, MOI.ListOfVariableIndices()) == [x, y, z]
    @test MOI.get(
        mock,
        MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.Zeros}(),
    ) == 0
    @test MOI.get(
        bridged_mock,
        MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.Zeros}(),
    ) == 1
    @test MOI.get(
        bridged_mock,
        MOI.ListOfConstraintIndices{MOI.VectorOfVariables,MOI.Zeros}(),
    ) == [cyz]

    s = """
    variables: x
    x >= 0.0
    con1: x + 0.0 == 0.0
    con2: x + 0.0 >= 1.0
    minobjective: x
    """
    model = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(model, s)
    MOI.Utilities.test_models_equal(
        mock,
        model,
        ["x"],
        ["con1", "con2"],
        [("x", MOI.GreaterThan{Float64}(0.0))],
    )

    _test_delete_bridged_variables(
        bridged_mock,
        yz,
        MOI.Zeros,
        3,
        ((MOI.SingleVariable, MOI.GreaterThan{Float64}, 1),),
    )
    @test MOI.is_valid(bridged_mock, x)
    @test !MOI.is_valid(bridged_mock, y)
    @test !MOI.is_valid(bridged_mock, z)
    return
end

end  # module

TestVariableZeros.runtests()
