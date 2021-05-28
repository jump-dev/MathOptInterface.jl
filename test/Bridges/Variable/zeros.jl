using Test

using MathOptInterface
const MOI = MathOptInterface
const MOIT = MathOptInterface.Test
const MOIU = MathOptInterface.Utilities
const MOIB = MathOptInterface.Bridges

include("../utilities.jl")

mock = MOIU.MockOptimizer(MOIU.Model{Float64}())
config = MOIT.Config()

bridged_mock = MOIB.Variable.Zeros{Float64}(mock)

x, cx = MOI.add_constrained_variable(bridged_mock, MOI.GreaterThan(0.0))
MOI.set(bridged_mock, MOI.VariableName(), x, "x")
yz, cyz = MOI.add_constrained_variables(bridged_mock, MOI.Zeros(2))
MOI.set(bridged_mock, MOI.VariableName(), yz, ["y", "z"])
MOI.set(bridged_mock, MOI.ConstraintName(), cyz, "cyz")
y, z = yz
fx = MOI.SingleVariable(x)
fy = MOI.SingleVariable(y)
fz = MOI.SingleVariable(z)

@testset "SingleVariable objective" begin
    MOI.set(bridged_mock, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(bridged_mock, MOI.ObjectiveFunction{typeof(fx)}(), fx)
    @test MOI.get(bridged_mock, MOI.ObjectiveFunction{typeof(fx)}()) == fx
end

# Test before adding affine constraints are affine expressions cannot be
# unbridged when `Variable.ZerosBridge` is used.
@testset "Test bridged model" begin
    s = """
    variables: x, y, z
    x >= 0.0
    cyz: [y, z] in MathOptInterface.Zeros(2)
    minobjective: x
    """
    model = MOIU.Model{Float64}()
    MOIU.loadfromstring!(model, s)
    MOIU.test_models_equal(
        bridged_mock,
        model,
        ["x", "y", "z"],
        ["cyz"],
        [("x", MOI.GreaterThan{Float64}(0.0))],
    )
end

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

@test MOIB.Variable.unbridged_map(
    MOIB.bridge(bridged_mock, y),
    y,
    MOIB.IndexInVector(1),
) === nothing
@test MOIB.Variable.unbridged_map(
    MOIB.bridge(bridged_mock, z),
    z,
    MOIB.IndexInVector(2),
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
@test_throws err MOI.get(bridged_mock, MOIT.UnknownVariableAttribute(), y)

@testset "Results" begin
    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
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

    err = ArgumentError(
        "Bridge of type `MathOptInterface.Bridges.Variable.ZerosBridge{Float64}`" *
        " does not support accessing the attribute" *
        " `MathOptInterface.ConstraintDual(1)`.",
    )
    @test_throws err MOI.get(bridged_mock, MOI.ConstraintDual(), cyz)
end

@testset "Query" begin
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
end

@testset "Test mock model" begin
    s = """
    variables: x
    x >= 0.0
    con1: x + 0.0 == 0.0
    con2: x + 0.0 >= 1.0
    minobjective: x
    """
    model = MOIU.Model{Float64}()
    MOIU.loadfromstring!(model, s)
    MOIU.test_models_equal(
        mock,
        model,
        ["x"],
        ["con1", "con2"],
        [("x", MOI.GreaterThan{Float64}(0.0))],
    )
end

@testset "Delete" begin
    test_delete_bridged_variables(
        bridged_mock,
        yz,
        MOI.Zeros,
        3,
        ((MOI.SingleVariable, MOI.GreaterThan{Float64}, 1),),
    )
    @test MOI.is_valid(bridged_mock, x)
    @test !MOI.is_valid(bridged_mock, y)
    @test !MOI.is_valid(bridged_mock, z)
end
