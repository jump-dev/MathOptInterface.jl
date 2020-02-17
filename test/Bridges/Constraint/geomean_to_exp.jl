
using Test

using MathOptInterface
const MOI = MathOptInterface
const MOIT = MathOptInterface.Test
const MOIU = MathOptInterface.Utilities
const MOIB = MathOptInterface.Bridges

include("../utilities.jl")

mock = MOIU.MockOptimizer(MOIU.UniversalFallback(MOIU.Model{Float64}()))
config = MOIT.TestConfig()

@testset "GeoMeantoExp" begin
    bridged_mock = MOIB.Constraint.GeoMeantoExp{Float64}(mock)

    MOIT.basic_constraint_tests(bridged_mock, config,
        include = [(F, MOI.GeometricMeanCone) for F in [
            MOI.VectorOfVariables, MOI.VectorAffineFunction{Float64}, MOI.VectorQuadraticFunction{Float64}
        ]])

    @testset "geomean1test" begin
        var_primal = [1, 1, 1, 1, 1, 0, 0, 0]
        exp_dual = [-inv(3), -inv(3), inv(3)]
        mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, var_primal,
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) => [-1, -inv(3)],
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) => [inv(3)],
            (MOI.VectorAffineFunction{Float64}, MOI.ExponentialCone) => fill(exp_dual, 3))

        MOIT.geomean1ftest(bridged_mock, config)

        var_names = ["u", "w1", "w2", "w3"]
        MOI.set(bridged_mock, MOI.VariableName(), MOI.get(bridged_mock, MOI.ListOfVariableIndices()), var_names)

        greater = MOI.get(mock, MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}}())
        @test length(greater) == 1
        less = MOI.get(mock, MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}}())
        @test length(less) == 2
        exps = MOI.get(mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.ExponentialCone}())
        y = MOI.get(mock, MOI.ListOfVariableIndices())[5]
        z = MOI.get(mock, MOI.ListOfVariableIndices())[6:end]
        @test length(z) == 3

        @testset "Test mock model" begin
            MOI.set(mock, MOI.VariableName(), y, "y")
            MOI.set(mock, MOI.VariableName(), z, ["z1", "z2", "z3"])
            MOI.set(mock, MOI.ConstraintName(), greater[1], "greater")
            MOI.set(mock, MOI.ConstraintName(), less[1], "less")
            @test length(exps) == 3
            MOI.set(mock, MOI.ConstraintName(), exps[1], "exp1")
            MOI.set(mock, MOI.ConstraintName(), exps[2], "exp2")
            MOI.set(mock, MOI.ConstraintName(), exps[3], "exp3")
            MOI.set(mock, MOI.ConstraintName(), less[2], "less_orig")

            s = """
            variables: u, w1, w2, w3, y, z1, z2, z3
            less_orig: w1 + w2 + w3 in MathOptInterface.LessThan(3.0)
            less: u + -1.0y in MathOptInterface.LessThan(0.0)
            greater: z1 + z2 + z3 in MathOptInterface.GreaterThan(0.0)
            exp1: [z1, y, 1.0w1] in MathOptInterface.ExponentialCone()
            exp2: [z2, y, 1.0w2] in MathOptInterface.ExponentialCone()
            exp3: [z3, y, 1.0w3] in MathOptInterface.ExponentialCone()
            maxobjective: u
            """
            model = MOIU.Model{Float64}()
            MOIU.loadfromstring!(model, s)
            MOIU.test_models_equal(mock, model, vcat(var_names, ["y", "z1", "z2", "z3"]), ["less_orig", "less", "greater", "exp1", "exp2", "exp3"])
        end

        @testset "Test bridged model" begin
            geomean = MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.GeometricMeanCone}())
            @test length(geomean) == 1
            MOI.set(bridged_mock, MOI.ConstraintName(), geomean[1], "geomean")
            less_orig = MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}}())
            @test length(less_orig) == 1
            MOI.set(bridged_mock, MOI.ConstraintName(), less_orig[1], "less_orig")

            s = """
            variables: u, w1, w2, w3
            less_orig: w1 + w2 + w3 in MathOptInterface.LessThan(3.0)
            geomean: [1.0u, w1, w2, w3] in MathOptInterface.GeometricMeanCone(4)
            maxobjective: u
            """
            model = MOIU.Model{Float64}()
            MOIU.loadfromstring!(model, s)
            MOIU.test_models_equal(bridged_mock, model, var_names, ["less_orig", "geomean"])
        end

        ci = first(MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.GeometricMeanCone}()))

        @testset "$attr" for attr in [MOI.ConstraintPrimalStart(), MOI.ConstraintDualStart()]
            @test MOI.supports(bridged_mock, attr, typeof(ci))
            value = (attr isa MOI.ConstraintPrimalStart) ? ones(4) : vcat(-1, fill(inv(3), 3))
            MOI.set(bridged_mock, attr, ci, value)
            @test MOI.get(bridged_mock, attr, ci) ≈ value
            if attr isa MOI.ConstraintPrimalStart
                @test MOI.get(mock, MOI.VariablePrimalStart(), y) == 1
                @test MOI.get(mock, MOI.VariablePrimalStart(), z) == zeros(3)
                @test MOI.get(mock, attr, less[1]) == 0
                @test MOI.get(mock, attr, greater[1]) == 0
                for i in 1:3
                    @test MOI.get(mock, attr, exps[i]) == [0, 1, 1]
                end
            else
                @test MOI.get(mock, attr, less[1]) == -1
                @test MOI.get(mock, attr, greater[1]) == inv(3)
                for i in 1:3
                    @test MOI.get(mock, attr, exps[i]) ≈ exp_dual
                end
            end
        end

        test_delete_bridge(bridged_mock, ci, 4, (
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}, 1),
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}, 0),
            (MOI.VectorAffineFunction{Float64}, MOI.ExponentialCone, 0)))
    end

    @testset "geomean2test" begin
        var_primal = vcat(ones(11), zeros(9))
        exp_dual = [-inv(9), -inv(9), inv(9)]
        mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, var_primal,
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) => fill(-inv(9), 9),
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) => [-1],
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) => [inv(9)],
            (MOI.VectorAffineFunction{Float64}, MOI.ExponentialCone) => fill(exp_dual, 9))

        MOIT.geomean2ftest(bridged_mock, config)

        var_names = vcat("t", ["x$(i)" for i in 1:9])
        MOI.set(bridged_mock, MOI.VariableName(), MOI.get(bridged_mock, MOI.ListOfVariableIndices()), var_names)

        greater = MOI.get(mock, MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}}())
        @test length(greater) == 1
        less = MOI.get(mock, MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}}())
        @test length(less) == 1
        exps = MOI.get(mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.ExponentialCone}())
        y = MOI.get(mock, MOI.ListOfVariableIndices())[11]
        z = MOI.get(mock, MOI.ListOfVariableIndices())[12:end]
        @test length(z) == 9

        @testset "Test mock model" begin
            MOI.set(mock, MOI.VariableName(), y, "y")
            z_names = ["z$(i)" for i in 1:9]
            MOI.set(mock, MOI.VariableName(), z, z_names)
            MOI.set(mock, MOI.ConstraintName(), greater[1], "greater")
            MOI.set(mock, MOI.ConstraintName(), less[1], "less")
            @test length(exps) == 9
            exp_names = ["exp$i" for i in 1:9]
            MOI.set(mock, MOI.ConstraintName(), exps, exp_names)
            equalto = MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}}())
            @test length(equalto) == 9
            equalto_names = ["equalto$(i)" for i in 1:9]
            MOI.set.(bridged_mock, MOI.ConstraintName(), equalto, equalto_names)

            s = """
            variables: t, x1, x2, x3, x4, x5, x6, x7, x8, x9, y, z1, z2, z3, z4, z5, z6, z7, z8, z9
            equalto1: 1.0x1 in MathOptInterface.EqualTo(1.0)
            equalto2: 1.0x2 in MathOptInterface.EqualTo(1.0)
            equalto3: 1.0x3 in MathOptInterface.EqualTo(1.0)
            equalto4: 1.0x4 in MathOptInterface.EqualTo(1.0)
            equalto5: 1.0x5 in MathOptInterface.EqualTo(1.0)
            equalto6: 1.0x6 in MathOptInterface.EqualTo(1.0)
            equalto7: 1.0x7 in MathOptInterface.EqualTo(1.0)
            equalto8: 1.0x8 in MathOptInterface.EqualTo(1.0)
            equalto9: 1.0x9 in MathOptInterface.EqualTo(1.0)
            less: t + -1.0y in MathOptInterface.LessThan(0.0)
            greater: z1 + z2 + z3 + z4 + z5 + z6 + z7 + z8 + z9 in MathOptInterface.GreaterThan(0.0)
            exp1: [z1, y, 1.0x1] in MathOptInterface.ExponentialCone()
            exp2: [z2, y, 1.0x2] in MathOptInterface.ExponentialCone()
            exp3: [z3, y, 1.0x3] in MathOptInterface.ExponentialCone()
            exp4: [z4, y, 1.0x4] in MathOptInterface.ExponentialCone()
            exp5: [z5, y, 1.0x5] in MathOptInterface.ExponentialCone()
            exp6: [z6, y, 1.0x6] in MathOptInterface.ExponentialCone()
            exp7: [z7, y, 1.0x7] in MathOptInterface.ExponentialCone()
            exp8: [z8, y, 1.0x8] in MathOptInterface.ExponentialCone()
            exp9: [z9, y, 1.0x9] in MathOptInterface.ExponentialCone()
            maxobjective: t
            """
            model = MOIU.Model{Float64}()
            MOIU.loadfromstring!(model, s)
            MOIU.test_models_equal(mock, model, vcat(var_names, "y", z_names), vcat(equalto_names, "less", "greater", exp_names))
        end

        @testset "Test bridged model" begin
            geomean = MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.GeometricMeanCone}())
            @test length(geomean) == 1
            MOI.set(bridged_mock, MOI.ConstraintName(), geomean[1], "geomean")
            equalto = MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}}())
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
            model = MOIU.Model{Float64}()
            MOIU.loadfromstring!(model, s)
            MOIU.test_models_equal(bridged_mock, model, var_names, vcat(equalto_names, "geomean"))
        end

        ci = first(MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.GeometricMeanCone}()))

        @testset "$attr" for attr in [MOI.ConstraintPrimalStart(), MOI.ConstraintDualStart()]
            @test MOI.supports(bridged_mock, attr, typeof(ci))
            value = (attr isa MOI.ConstraintPrimalStart) ? ones(10) : vcat(-1, fill(inv(9), 9))
            MOI.set(bridged_mock, attr, ci, value)
            @test MOI.get(bridged_mock, attr, ci) ≈ value
            if attr isa MOI.ConstraintPrimalStart
                @test MOI.get(mock, MOI.VariablePrimalStart(), y) == 1
                @test MOI.get(mock, MOI.VariablePrimalStart(), z) == zeros(9)
                @test MOI.get(mock, attr, less[1]) == 0
                @test MOI.get(mock, attr, greater[1]) == 0
                for i in 1:9
                    @test MOI.get(mock, attr, exps[i]) == [0, 1, 1]
                end
            else
                @test MOI.get(mock, attr, less[1]) == -1
                @test MOI.get(mock, attr, greater[1]) == inv(9)
                for i in 1:9
                    @test MOI.get(mock, attr, exps[i]) ≈ exp_dual
                end
            end
        end

        test_delete_bridge(bridged_mock, ci, 10, (
            (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}, 9),
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}, 0),
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}, 0),
            (MOI.VectorAffineFunction{Float64}, MOI.ExponentialCone, 0)))
    end

    @testset "geomean3test" begin
        var_primal = [2, 2, 2, 0]
        exp_dual = [-2, -2, 2]
        mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, var_primal,
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) => [-2, -2],
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) => [2],
            (MOI.VectorAffineFunction{Float64}, MOI.ExponentialCone) => [exp_dual])

        MOIT.geomean3ftest(bridged_mock, config)

        var_names = ["t", "x"]
        MOI.set(bridged_mock, MOI.VariableName(), MOI.get(bridged_mock, MOI.ListOfVariableIndices()), var_names)

        greater = MOI.get(mock, MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}}())
        @test length(greater) == 1
        less = MOI.get(mock, MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}}())
        @test length(less) == 2
        exps = MOI.get(mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.ExponentialCone}())
        y = MOI.get(mock, MOI.ListOfVariableIndices())[3]
        z = MOI.get(mock, MOI.ListOfVariableIndices())[4]

        @testset "Test mock model" begin
            MOI.set(mock, MOI.VariableName(), y, "y")
            MOI.set(mock, MOI.VariableName(), z, "z")
            MOI.set(mock, MOI.ConstraintName(), greater[1], "greater")
            MOI.set(mock, MOI.ConstraintName(), less[1], "less")
            @test length(exps) == 1
            MOI.set(mock, MOI.ConstraintName(), exps[1], "exp")
            MOI.set(mock, MOI.ConstraintName(), less[2], "less_orig")

            s = """
            variables: t, x, y, z
            less_orig: 1.0x in MathOptInterface.LessThan(2.0)
            less: t + -1.0y in MathOptInterface.LessThan(0.0)
            greater: 1.0z in MathOptInterface.GreaterThan(0.0)
            exp: [z, y, 1.0x] in MathOptInterface.ExponentialCone()
            maxobjective: 2.0t
            """
            model = MOIU.Model{Float64}()
            MOIU.loadfromstring!(model, s)
            MOIU.test_models_equal(mock, model, vcat(var_names, "y", "z"), ["less_orig", "less", "greater", "exp"])
        end

        @testset "Test bridged model" begin
            geomean = MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.GeometricMeanCone}())
            @test length(geomean) == 1
            MOI.set(bridged_mock, MOI.ConstraintName(), geomean[1], "geomean")
            less_orig = MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}}())
            @test length(less_orig) == 1
            MOI.set(bridged_mock, MOI.ConstraintName(), less_orig[1], "less_orig")

            s = """
            variables: t, x
            less_orig: 1.0x in MathOptInterface.LessThan(2.0)
            geomean: [1.0t, x] in MathOptInterface.GeometricMeanCone(2)
            maxobjective: 2.0t
            """
            model = MOIU.Model{Float64}()
            MOIU.loadfromstring!(model, s)
            MOIU.test_models_equal(bridged_mock, model, var_names, ["less_orig", "geomean"])
        end

        ci = first(MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.GeometricMeanCone}()))

        @testset "$attr" for attr in [MOI.ConstraintPrimalStart(), MOI.ConstraintDualStart()]
            @test MOI.supports(bridged_mock, attr, typeof(ci))
            value = (attr isa MOI.ConstraintPrimalStart) ? [2, 2] : [-2, 2]
            MOI.set(bridged_mock, attr, ci, value)
            @test MOI.get(bridged_mock, attr, ci) ≈ value
            if attr isa MOI.ConstraintPrimalStart
                @test MOI.get(mock, MOI.VariablePrimalStart(), y) == 2
                @test MOI.get(mock, MOI.VariablePrimalStart(), z) == 0
                @test MOI.get(mock, attr, less[1]) == 0
                @test MOI.get(mock, attr, greater[1]) == 0
                @test MOI.get(mock, attr, exps[1]) == [0, 2, 2]
            else
                @test MOI.get(mock, attr, less[1]) == -2
                @test MOI.get(mock, attr, greater[1]) == 2
                @test MOI.get(mock, attr, exps[1]) ≈ exp_dual
            end
        end

        test_delete_bridge(bridged_mock, ci, 2, (
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}, 1),
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}, 0),
            (MOI.VectorAffineFunction{Float64}, MOI.ExponentialCone, 0)))
    end
end
