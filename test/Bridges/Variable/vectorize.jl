using Test

using MathOptInterface
const MOI = MathOptInterface
const MOIT = MathOptInterface.DeprecatedTest
const MOIU = MathOptInterface.Utilities
const MOIB = MathOptInterface.Bridges

include("../utilities.jl")

mock = MOIU.MockOptimizer(MOIU.UniversalFallback(MOIU.Model{Float64}()))
config = MOIT.Config()

bridged_mock = MOIB.Variable.Vectorize{Float64}(mock)

@testset "get scalar constraint" begin
    x, cx = MOI.add_constrained_variable(bridged_mock, MOI.GreaterThan(1.0))
    fx = MOI.SingleVariable(x)
    func = 2.0 * fx
    set = MOI.GreaterThan(5.0)
    err =
        MOI.ScalarFunctionConstantNotZero{Float64,typeof(func),typeof(set)}(1.0)
    @test_throws err MOI.add_constraint(bridged_mock, func + 1.0, set)

    c = MOI.add_constraint(bridged_mock, func, set)
    @test MOI.get(bridged_mock, MOI.ConstraintFunction(), c) ≈ func
    @test MOI.get(bridged_mock, MOI.ConstraintSet(), c) == set
    MOI.set(bridged_mock, MOI.ConstraintName(), c, "c")

    @testset "Mock model" begin
        MOI.set(
            mock,
            MOI.VariableName(),
            MOI.get(mock, MOI.ListOfVariableIndices()),
            ["y"],
        )
        MOI.set(
            mock,
            MOI.ConstraintName(),
            MOI.get(
                mock,
                MOI.ListOfConstraintIndices{
                    MOI.VectorOfVariables,
                    MOI.Nonnegatives,
                }(),
            ),
            ["cy"],
        )
        s = """
        variables: y
        cy: [y] in MathOptInterface.Nonnegatives(1)
        c: 2.0y >= 3.0
        """
        model = MOIU.Model{Float64}()
        MOIU.loadfromstring!(model, s)
        MOIU.test_models_equal(mock, model, ["y"], ["cy", "c"])
    end
    @testset "Bridged model" begin
        MOI.set(bridged_mock, MOI.VariableName(), x, "x")
        s = """
        variables: x
        x >= 1.0
        c: 2.0x >= 5.0
        """
        model = MOIU.Model{Float64}()
        MOIU.loadfromstring!(model, s)
        MOIU.test_models_equal(
            bridged_mock,
            model,
            ["x"],
            ["c"],
            [("x", MOI.GreaterThan{Float64}(1.0))],
        )
    end
end

@testset "exp3 with add_constrained_variable for `y`" begin
    mock.optimize! =
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [log(5), 0.0],
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) =>
                [0.0],
            (MOI.VectorAffineFunction{Float64}, MOI.ExponentialCone) =>
                [[-1.0, log(5) - 1, 1 / 5]],
        )

    MOI.empty!(bridged_mock)
    x = MOI.add_variable(bridged_mock)
    @test MOI.get(bridged_mock, MOI.NumberOfVariables()) == 1
    fx = MOI.SingleVariable(x)
    xc = MOI.add_constraint(bridged_mock, 2.0fx, MOI.LessThan(4.0))
    y, yc = MOI.add_constrained_variable(bridged_mock, MOI.LessThan(5.0))
    @test yc.value == y.value == -1
    @test MOI.get(bridged_mock, MOI.NumberOfVariables()) == 2
    @test length(MOI.get(bridged_mock, MOI.ListOfVariableIndices())) == 2
    @test Set(MOI.get(bridged_mock, MOI.ListOfVariableIndices())) == Set([x, y])
    fy = MOI.SingleVariable(y)
    ec = MOI.add_constraint(
        bridged_mock,
        MOIU.operate(vcat, Float64, fx, 1.0, fy),
        MOI.ExponentialCone(),
    )

    MOI.optimize!(bridged_mock)
    @test MOI.get(bridged_mock, MOI.VariablePrimal(), x) ≈ log(5)
    @test MOI.get(bridged_mock, MOI.VariablePrimal(), y) ≈ 5.0
    @test MOI.get(bridged_mock, MOI.ConstraintPrimal(), xc) ≈ 2log(5)
    @test MOI.get(bridged_mock, MOI.ConstraintPrimal(), yc) ≈ 5
    @test MOI.get(bridged_mock, MOI.ConstraintPrimal(), ec) ≈ [log(5), 1.0, 5.0]
    @test MOI.get(bridged_mock, MOI.ConstraintDual(), xc) ≈ 0.0
    @test MOI.get(bridged_mock, MOI.ConstraintDual(), yc) ≈ -1 / 5
    @test MOI.get(bridged_mock, MOI.ConstraintDual(), ec) ≈
          [-1.0, log(5) - 1, 1 / 5]

    err = ErrorException(
        "Cannot add two `SingleVariable`-in-`MathOptInterface.LessThan{Float64}`" *
        " on the same variable MathOptInterface.VariableIndex(-1).",
    )
    @test_throws err MOI.add_constraint(
        bridged_mock,
        MOI.SingleVariable(y),
        MOI.LessThan(4.0),
    )

    cis = MOI.get(
        bridged_mock,
        MOI.ListOfConstraintIndices{
            MOI.VectorAffineFunction{Float64},
            MOI.ExponentialCone,
        }(),
    )
    @test length(cis) == 1

    @testset "get `UnknownVariableAttribute``" begin
        err = ArgumentError(
            "Variable bridge of type `$(MathOptInterface.Bridges.Variable.VectorizeBridge{Float64,MathOptInterface.Nonpositives})`" *
            " does not support accessing the attribute `MathOptInterface.DeprecatedTest.UnknownVariableAttribute()`.",
        )
        @test_throws err MOI.get(
            bridged_mock,
            MOIT.UnknownVariableAttribute(),
            y,
        )
    end

    @testset "set `ConstraintSet`" begin
        ci = MOI.ConstraintIndex{MOI.SingleVariable,MOI.LessThan{Float64}}(
            y.value,
        )
        attr = MOI.ConstraintSet()
        err = MOI.SetAttributeNotAllowed(
            attr,
            "The variable `MathOptInterface.VariableIndex(12345676)` is bridged by the `VectorizeBridge`.",
        )
        @test_throws err MOI.set(bridged_mock, attr, ci, MOI.LessThan(4.0))
    end

    @testset "MultirowChange" begin
        change = MOI.MultirowChange(y, [(3, 0.0)])
        message =
            "The change $change" *
            " contains variables bridged into a function with nonzero constant."
        err = MOI.ModifyConstraintNotAllowed(cis[1], change, message)
        @test_throws err MOI.modify(bridged_mock, cis[1], change)
    end

    @testset "ScalarCoefficientChange" begin
        change = MOI.ScalarCoefficientChange(y, 0.0)
        attr = MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}()
        message =
            "The change MathOptInterface.ScalarCoefficientChange{Float64}(MathOptInterface.VariableIndex(-1), 0.0)" *
            " contains variables bridged into a function with nonzero constant."
        err = MOI.ModifyObjectiveNotAllowed(change, message)
        @test_throws err MOI.modify(bridged_mock, attr, change)
    end

    MOI.set(bridged_mock, MOI.VariableName(), x, "x")
    MOI.set(bridged_mock, MOI.ConstraintName(), xc, "xc")
    MOI.set(bridged_mock, MOI.ConstraintName(), ec, "ec")
    z = MOI.get(mock, MOI.ListOfVariableIndices())[2]
    @testset "Mock model" begin
        MOI.set(mock, MOI.VariableName(), z, "z")
        MOI.set(
            mock,
            MOI.ConstraintName(),
            MOI.get(
                mock,
                MOI.ListOfConstraintIndices{
                    MOI.VectorOfVariables,
                    MOI.Nonpositives,
                }(),
            ),
            ["zc"],
        )
        s = """
        variables: x, z
        zc: [z] in MathOptInterface.Nonpositives(1)
        xc: 2.0x <= 4.0
        ec: [x, 1.0, z + 5.0] in MathOptInterface.ExponentialCone()
        """
        model = MOIU.Model{Float64}()
        MOIU.loadfromstring!(model, s)
        MOIU.test_models_equal(mock, model, ["x", "z"], ["zc", "xc", "ec"])
    end
    @testset "Bridged model" begin
        MOI.set(bridged_mock, MOI.VariableName(), y, "y")
        s = """
        variables: x, y
        y <= 5.0
        xc: 2.0x <= 4.0
        ec: [x, 1.0, y] in MathOptInterface.ExponentialCone()
        """
        model = MOIU.Model{Float64}()
        MOIU.loadfromstring!(model, s)
        MOIU.test_models_equal(
            bridged_mock,
            model,
            ["x", "y"],
            ["xc", "ec"],
            [("y", MOI.LessThan{Float64}(5.0))],
        )
    end

    @test MOI.supports(
        bridged_mock,
        MOI.VariablePrimalStart(),
        MOI.VariableIndex,
    )
    @test MOI.supports(
        bridged_mock,
        MOI.VariablePrimalStart(),
        typeof(MOIB.bridge(bridged_mock, y)),
    )
    MOI.set(bridged_mock, MOI.VariablePrimalStart(), y, 1.0)
    @test MOI.get(mock, MOI.VariablePrimalStart(), z) == -4
    @test MOI.get(bridged_mock, MOI.VariablePrimalStart(), y) == 1

    test_delete_bridged_variable(
        bridged_mock,
        y,
        MOI.LessThan{Float64},
        2,
        ((MOI.VectorOfVariables, MOI.Nonpositives, 0),),
    )
end
