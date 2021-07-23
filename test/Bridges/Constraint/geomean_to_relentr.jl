module TestConstraintGeomeanToRelentr

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

function test_GeoMeantoRelEntr()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    config = MOI.Test.Config()
    bridged_mock = MOI.Bridges.Constraint.GeoMeantoRelEntr{Float64}(mock)
    MOI.Test.test_basic_VectorOfVariables_GeometricMeanCone(
        bridged_mock,
        config,
    )
    MOI.empty!(bridged_mock)
    MOI.Test.test_basic_VectorAffineFunction_GeometricMeanCone(
        bridged_mock,
        config,
    )
    MOI.empty!(bridged_mock)
    return MOI.Test.test_basic_VectorQuadraticFunction_GeometricMeanCone(
        bridged_mock,
        config,
    )
end

function test_conic_GeometricMeanCone_VectorAffineFunction()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    config = MOI.Test.Config()
    bridged_mock = MOI.Bridges.Constraint.GeoMeantoRelEntr{Float64}(mock)
    var_primal = [1, 1, 1, 1, 0]
    relentr_dual = [1, 1, 1, 1, -1, -1, -1] / 3
    mock.optimize! =
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            var_primal,
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) =>
                [-inv(3)],
            (MOI.VectorOfVariables, MOI.Nonnegatives) => [[1]],
            (MOI.VectorAffineFunction{Float64}, MOI.RelativeEntropyCone) =>
                [relentr_dual],
        )

    MOI.Test.test_conic_GeometricMeanCone_VectorAffineFunction(
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
        MOI.ListOfConstraintIndices{MOI.VectorOfVariables,MOI.Nonnegatives}(),
    )
    relentr = MOI.get(
        mock,
        MOI.ListOfConstraintIndices{
            MOI.VectorAffineFunction{Float64},
            MOI.RelativeEntropyCone,
        }(),
    )
    aux = MOI.get(mock, MOI.ListOfVariableIndices())[end]
    MOI.set(mock, MOI.VariableName(), aux, "aux")
    @test length(nonneg) == 1
    MOI.set(mock, MOI.ConstraintName(), nonneg[1], "nonneg")
    @test length(relentr) == 1
    MOI.set(mock, MOI.ConstraintName(), relentr[1], "relentr")
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
    variables: t, x, y, z, aux
    less: x + y + z in MathOptInterface.LessThan(3.0)
    nonneg: [aux] in MathOptInterface.Nonnegatives(1)
    relentr: [0.0, x, y, z, t + aux, t + aux, t + aux] in MathOptInterface.RelativeEntropyCone(7)
    maxobjective: t
    """
    model = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(model, s)
    MOI.Utilities.test_models_equal(
        mock,
        model,
        vcat(var_names, "aux"),
        ["less", "nonneg", "relentr"],
    )
    geomean = MOI.get(
        bridged_mock,
        MOI.ListOfConstraintIndices{
            MOI.VectorAffineFunction{Float64},
            MOI.GeometricMeanCone,
        }(),
    )
    @test length(geomean) == 1
    MOI.set(bridged_mock, MOI.ConstraintName(), geomean[1], "geomean")
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
    less: x + y + z in MathOptInterface.LessThan(3.0)
    geomean: [1.0t, x, y, z] in MathOptInterface.GeometricMeanCone(4)
    maxobjective: t
    """
    model = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(model, s)
    MOI.Utilities.test_models_equal(
        bridged_mock,
        model,
        var_names,
        ["less", "geomean"],
    )
    ci = first(
        MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{
                MOI.VectorAffineFunction{Float64},
                MOI.GeometricMeanCone,
            }(),
        ),
    )
    for attr in [MOI.ConstraintPrimalStart(), MOI.ConstraintDualStart()]
        @test MOI.supports(bridged_mock, attr, typeof(ci))
        value =
            (attr isa MOI.ConstraintPrimalStart) ? ones(4) :
            vcat(-1, fill(inv(3), 3))
        MOI.set(bridged_mock, attr, ci, value)
        @test MOI.get(bridged_mock, attr, ci) ≈ value
        if attr isa MOI.ConstraintPrimalStart
            @test MOI.get(mock, MOI.VariablePrimalStart(), aux) == 0
            @test MOI.get(mock, attr, nonneg[1]) == [0]
            @test MOI.get(mock, attr, relentr[1]) == [0, 1, 1, 1, 1, 1, 1]
        else
            @test MOI.get(mock, attr, nonneg[1]) == [1]
            @test MOI.get(mock, attr, relentr[1]) ≈ relentr_dual
        end
    end
    _test_delete_bridge(
        bridged_mock,
        ci,
        4,
        (
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}, 1),
            (MOI.VectorOfVariables, MOI.Nonnegatives, 0),
            (MOI.VectorAffineFunction{Float64}, MOI.RelativeEntropyCone, 0),
        ),
    )
    return
end

function test_conic_GeometricMeanCone_VectorAffineFunction_2()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    config = MOI.Test.Config()
    bridged_mock = MOI.Bridges.Constraint.GeoMeantoRelEntr{Float64}(mock)
    var_primal = vcat(ones(10), 0)
    relentr_dual = vcat(fill(inv(9), 10), fill(-inv(9), 9))
    mock.optimize! =
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            var_primal,
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
                fill(-inv(9), 9),
            (MOI.VectorOfVariables, MOI.Nonnegatives) => [[1]],
            (MOI.VectorAffineFunction{Float64}, MOI.RelativeEntropyCone) =>
                [relentr_dual],
        )

    MOI.Test.test_conic_GeometricMeanCone_VectorAffineFunction_2(
        bridged_mock,
        config,
    )
    var_names = vcat("t", ["x$(i)" for i in 1:9])
    MOI.set(
        bridged_mock,
        MOI.VariableName(),
        MOI.get(bridged_mock, MOI.ListOfVariableIndices()),
        var_names,
    )
    nonneg = MOI.get(
        mock,
        MOI.ListOfConstraintIndices{MOI.VectorOfVariables,MOI.Nonnegatives}(),
    )
    relentr = MOI.get(
        mock,
        MOI.ListOfConstraintIndices{
            MOI.VectorAffineFunction{Float64},
            MOI.RelativeEntropyCone,
        }(),
    )
    aux = MOI.get(mock, MOI.ListOfVariableIndices())[end]
    MOI.set(mock, MOI.VariableName(), aux, "aux")
    @test length(nonneg) == 1
    MOI.set(mock, MOI.ConstraintName(), nonneg[1], "nonneg")
    @test length(relentr) == 1
    MOI.set(mock, MOI.ConstraintName(), relentr[1], "relentr")
    equalto = MOI.get(
        bridged_mock,
        MOI.ListOfConstraintIndices{
            MOI.ScalarAffineFunction{Float64},
            MOI.EqualTo{Float64},
        }(),
    )
    @test length(equalto) == 9
    equalto_names = ["equalto$(i)" for i in 1:9]
    MOI.set.(bridged_mock, MOI.ConstraintName(), equalto, equalto_names)

    s = """
    variables: t, x1, x2, x3, x4, x5, x6, x7, x8, x9, aux
    equalto1: 1.0x1 in MathOptInterface.EqualTo(1.0)
    equalto2: 1.0x2 in MathOptInterface.EqualTo(1.0)
    equalto3: 1.0x3 in MathOptInterface.EqualTo(1.0)
    equalto4: 1.0x4 in MathOptInterface.EqualTo(1.0)
    equalto5: 1.0x5 in MathOptInterface.EqualTo(1.0)
    equalto6: 1.0x6 in MathOptInterface.EqualTo(1.0)
    equalto7: 1.0x7 in MathOptInterface.EqualTo(1.0)
    equalto8: 1.0x8 in MathOptInterface.EqualTo(1.0)
    equalto9: 1.0x9 in MathOptInterface.EqualTo(1.0)
    nonneg: [aux] in MathOptInterface.Nonnegatives(1)
    relentr: [0.0, x1, x2, x3, x4, x5, x6, x7, x8, x9, t + aux, t + aux, t + aux, t + aux, t + aux, t + aux, t + aux, t + aux, t + aux] in MathOptInterface.RelativeEntropyCone(19)
    maxobjective: t
    """
    model = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(model, s)
    MOI.Utilities.test_models_equal(
        mock,
        model,
        vcat(var_names, "aux"),
        vcat(equalto_names, "nonneg", "relentr"),
    )
    geomean = MOI.get(
        bridged_mock,
        MOI.ListOfConstraintIndices{
            MOI.VectorAffineFunction{Float64},
            MOI.GeometricMeanCone,
        }(),
    )
    @test length(geomean) == 1
    MOI.set(bridged_mock, MOI.ConstraintName(), geomean[1], "geomean")
    equalto = MOI.get(
        bridged_mock,
        MOI.ListOfConstraintIndices{
            MOI.ScalarAffineFunction{Float64},
            MOI.EqualTo{Float64},
        }(),
    )
    @test length(equalto) == 9
    equalto_names = ["equalto$(i)" for i in 1:9]
    MOI.set.(bridged_mock, MOI.ConstraintName(), equalto, equalto_names)

    s = """
    variables: t, x1, x2, x3, x4, x5, x6, x7, x8, x9
    equalto1: 1.0x1 in MathOptInterface.EqualTo(1.0)
    equalto2: 1.0x2 in MathOptInterface.EqualTo(1.0)
    equalto3: 1.0x3 in MathOptInterface.EqualTo(1.0)
    equalto4: 1.0x4 in MathOptInterface.EqualTo(1.0)
    equalto5: 1.0x5 in MathOptInterface.EqualTo(1.0)
    equalto6: 1.0x6 in MathOptInterface.EqualTo(1.0)
    equalto7: 1.0x7 in MathOptInterface.EqualTo(1.0)
    equalto8: 1.0x8 in MathOptInterface.EqualTo(1.0)
    equalto9: 1.0x9 in MathOptInterface.EqualTo(1.0)
    geomean: [1.0t, x1, x2, x3, x4, x5, x6, x7, x8, x9] in MathOptInterface.GeometricMeanCone(10)
    maxobjective: t
    """
    model = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(model, s)
    MOI.Utilities.test_models_equal(
        bridged_mock,
        model,
        var_names,
        vcat(equalto_names, "geomean"),
    )
    ci = first(
        MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{
                MOI.VectorAffineFunction{Float64},
                MOI.GeometricMeanCone,
            }(),
        ),
    )
    for attr in [MOI.ConstraintPrimalStart(), MOI.ConstraintDualStart()]
        @test MOI.supports(bridged_mock, attr, typeof(ci))
        value =
            (attr isa MOI.ConstraintPrimalStart) ? ones(10) :
            vcat(-1, fill(inv(9), 9))
        MOI.set(bridged_mock, attr, ci, value)
        @test MOI.get(bridged_mock, attr, ci) ≈ value
        if attr isa MOI.ConstraintPrimalStart
            @test MOI.get(mock, MOI.VariablePrimalStart(), aux) == 0
            @test MOI.get(mock, attr, nonneg[1]) == [0]
            @test MOI.get(mock, attr, relentr[1]) == vcat(0, ones(18))
        else
            @test MOI.get(mock, attr, nonneg[1]) == [1]
            @test MOI.get(mock, attr, relentr[1]) ≈ relentr_dual
        end
    end
    _test_delete_bridge(
        bridged_mock,
        ci,
        10,
        (
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}, 9),
            (MOI.VectorOfVariables, MOI.Nonnegatives, 0),
            (MOI.VectorAffineFunction{Float64}, MOI.RelativeEntropyCone, 0),
        ),
    )
    return
end

function test_conic_GeometricMeanCone_VectorAffineFunction_3()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    config = MOI.Test.Config()
    bridged_mock = MOI.Bridges.Constraint.GeoMeantoRelEntr{Float64}(mock)
    var_primal = [2, 2, 0]
    relentr_dual = [2, 2, -2]
    mock.optimize! =
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            var_primal,
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) =>
                [-2],
            (MOI.VectorOfVariables, MOI.Nonnegatives) => [[2]],
            (MOI.VectorAffineFunction{Float64}, MOI.RelativeEntropyCone) =>
                [relentr_dual],
        )

    MOI.Test.test_conic_GeometricMeanCone_VectorAffineFunction_3(
        bridged_mock,
        config,
    )
    var_names = ["t", "x"]
    MOI.set(
        bridged_mock,
        MOI.VariableName(),
        MOI.get(bridged_mock, MOI.ListOfVariableIndices()),
        var_names,
    )
    nonneg = MOI.get(
        mock,
        MOI.ListOfConstraintIndices{MOI.VectorOfVariables,MOI.Nonnegatives}(),
    )
    relentr = MOI.get(
        mock,
        MOI.ListOfConstraintIndices{
            MOI.VectorAffineFunction{Float64},
            MOI.RelativeEntropyCone,
        }(),
    )
    aux = MOI.get(mock, MOI.ListOfVariableIndices())[end]
    MOI.set(mock, MOI.VariableName(), aux, "aux")
    @test length(nonneg) == 1
    MOI.set(mock, MOI.ConstraintName(), nonneg[1], "nonneg")
    @test length(relentr) == 1
    MOI.set(mock, MOI.ConstraintName(), relentr[1], "relentr")
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
    variables: t, x, aux
    less: 1.0x in MathOptInterface.LessThan(2.0)
    nonneg: [aux] in MathOptInterface.Nonnegatives(1)
    relentr: [0.0, x, t + aux] in MathOptInterface.RelativeEntropyCone(3)
    maxobjective: 2.0t
    """
    model = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(model, s)
    MOI.Utilities.test_models_equal(
        mock,
        model,
        vcat(var_names, "aux"),
        ["less", "nonneg", "relentr"],
    )
    geomean = MOI.get(
        bridged_mock,
        MOI.ListOfConstraintIndices{
            MOI.VectorAffineFunction{Float64},
            MOI.GeometricMeanCone,
        }(),
    )
    @test length(geomean) == 1
    MOI.set(bridged_mock, MOI.ConstraintName(), geomean[1], "geomean")
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
    variables: t, x
    less: 1.0x in MathOptInterface.LessThan(2.0)
    geomean: [1.0t, x] in MathOptInterface.GeometricMeanCone(2)
    maxobjective: 2.0t
    """
    model = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(model, s)
    MOI.Utilities.test_models_equal(
        bridged_mock,
        model,
        var_names,
        ["less", "geomean"],
    )
    ci = first(
        MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{
                MOI.VectorAffineFunction{Float64},
                MOI.GeometricMeanCone,
            }(),
        ),
    )
    for attr in [MOI.ConstraintPrimalStart(), MOI.ConstraintDualStart()]
        @test MOI.supports(bridged_mock, attr, typeof(ci))
        value = (attr isa MOI.ConstraintPrimalStart) ? [2, 2] : [-2, 2]
        MOI.set(bridged_mock, attr, ci, value)
        @test MOI.get(bridged_mock, attr, ci) ≈ value
        if attr isa MOI.ConstraintPrimalStart
            @test MOI.get(mock, MOI.VariablePrimalStart(), aux) == 0
            @test MOI.get(mock, attr, nonneg[1]) == [0]
            @test MOI.get(mock, attr, relentr[1]) == [0, 2, 2]
        else
            @test MOI.get(mock, attr, nonneg[1]) == [2]
            @test MOI.get(mock, attr, relentr[1]) ≈ relentr_dual
        end
    end
    _test_delete_bridge(
        bridged_mock,
        ci,
        2,
        (
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}, 1),
            (MOI.VectorOfVariables, MOI.Nonnegatives, 0),
            (MOI.VectorAffineFunction{Float64}, MOI.RelativeEntropyCone, 0),
        ),
    )
    return
end

end  # module

TestConstraintGeomeanToRelentr.runtests()
