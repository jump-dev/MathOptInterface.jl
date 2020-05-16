using Test

using MathOptInterface
const MOI = MathOptInterface
const MOIT = MathOptInterface.Test
const MOIU = MathOptInterface.Utilities
const MOIB = MathOptInterface.Bridges

include("../utilities.jl")

mock = MOIU.MockOptimizer(MOIU.UniversalFallback(MOIU.Model{Float64}()))
config = MOIT.TestConfig()

@testset "NormInfinity" begin
    bridged_mock = MOIB.Constraint.NormInfinity{Float64}(mock)

    MOIT.basic_constraint_tests(bridged_mock, config,
        include = [(F, MOI.NormInfinityCone) for F in [
            MOI.VectorOfVariables, MOI.VectorAffineFunction{Float64}, MOI.VectorQuadraticFunction{Float64}
        ]])

    @testset "norminf1test" begin
        mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock,
            [1.0, 0.5, 1.0],
            (MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives) => [[0.0, 1.0, 0.0, 0.0]],
            (MOI.VectorAffineFunction{Float64}, MOI.Zeros) => [[-1], [-1]])

        MOIT.norminf1vtest(bridged_mock, config)
        MOIT.norminf1ftest(bridged_mock, config)

        var_names = ["x", "y", "z"]

        @testset "Test mock model" begin
            MOI.set(mock, MOI.VariableName(), MOI.get(mock, MOI.ListOfVariableIndices()), var_names)
            nonneg = MOI.get(mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives}())
            @test length(nonneg) == 1
            MOI.set(mock, MOI.ConstraintName(), nonneg[1], "nonneg")
            zeros = MOI.get(mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.Zeros}())
            @test length(zeros) == 2
            MOI.set(mock, MOI.ConstraintName(), zeros[1], "x_eq")
            MOI.set(mock, MOI.ConstraintName(), zeros[2], "y_eq")

            s = """
            variables: x, y, z
            nonneg: [x + -1.0y, x + -1.0z, x + y, x + z] in MathOptInterface.Nonnegatives(4)
            x_eq: [-1.0 + x] in MathOptInterface.Zeros(1)
            y_eq: [-0.5 + y] in MathOptInterface.Zeros(1)
            maxobjective: y + z
            """
            model = MOIU.Model{Float64}()
            MOIU.loadfromstring!(model, s)
            MOIU.test_models_equal(mock, model, var_names, ["nonneg", "x_eq", "y_eq"])
        end

        @testset "Test bridged model" begin
            MOI.set(bridged_mock, MOI.VariableName(), MOI.get(bridged_mock, MOI.ListOfVariableIndices()), var_names)
            norminf = MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.NormInfinityCone}())
            @test length(norminf) == 1
            MOI.set(bridged_mock, MOI.ConstraintName(), norminf[1], "norminf")
            zeros = MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.Zeros}())
            @test length(zeros) == 2
            MOI.set(bridged_mock, MOI.ConstraintName(), zeros[1], "x_eq")
            MOI.set(bridged_mock, MOI.ConstraintName(), zeros[2], "y_eq")

            s = """
            variables: x, y, z
            norminf: [1.0x, y, z] in MathOptInterface.NormInfinityCone(3)
            x_eq: [-1.0 + x] in MathOptInterface.Zeros(1)
            y_eq: [-0.5 + y] in MathOptInterface.Zeros(1)
            maxobjective: y + z
            """
            model = MOIU.Model{Float64}()
            MOIU.loadfromstring!(model, s)
            MOIU.test_models_equal(bridged_mock, model, var_names, ["norminf", "x_eq", "y_eq"])
        end

        ci = first(MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.NormInfinityCone}()))
        nonneg = MOI.get(mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives}())

        @testset "$attr" for attr in [MOI.ConstraintPrimalStart(), MOI.ConstraintDualStart()]
            @test MOI.supports(bridged_mock, attr, typeof(ci))
            value = [4.0, 1.0, -2.0]
            MOI.set(bridged_mock, attr, ci, value)
            @test MOI.get(bridged_mock, attr, ci) ≈ value
            if attr isa MOI.ConstraintPrimalStart
                nonneg_value = Float64[3, 6, 5, 2]
            else
                nonneg_value = [0.25, 2.25, 1.25, 0.25]
            end
            @test MOI.get(mock, attr, nonneg[1]) ≈ nonneg_value
        end

        test_delete_bridge(bridged_mock, ci, 3, (
            (MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives, 0),))
    end

    @testset "norminf3test" begin
        mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock,
            [2, -1, -1, -1],
            (MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives) => [vcat(fill(inv(3), 3), zeros(3)), fill(inv(3), 3)])
        MOIT.norminf3test(bridged_mock, config)

        var_names = ["x", "y1", "y2", "y3"]

        @testset "Test mock model" begin
            MOI.set(mock, MOI.VariableName(), MOI.get(mock, MOI.ListOfVariableIndices()), var_names)
            nonneg = MOI.get(mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives}())
            @test length(nonneg) == 2
            MOI.set(mock, MOI.ConstraintName(), nonneg[1], "nonneg1")
            MOI.set(mock, MOI.ConstraintName(), nonneg[2], "nonneg2")

            s = """
            variables: x, y1, y2, y3
            nonneg1: [x + -1.0y1 + -3.0, x + -1.0y2 + -3.0, x + -1.0y3 + -3.0, x + y1 + 1.0, x + y2 + 1.0, x + y3 + 1.0] in MathOptInterface.Nonnegatives(6)
            nonneg2: [y1 + 1.0, y2 + 1.0, y3 + 1.0] in MathOptInterface.Nonnegatives(3)
            minobjective: x
            """
            model = MOIU.Model{Float64}()
            MOIU.loadfromstring!(model, s)
            MOIU.test_models_equal(mock, model, var_names, ["nonneg1", "nonneg2"])
        end

        @testset "Test bridged model" begin
            MOI.set(bridged_mock, MOI.VariableName(), MOI.get(bridged_mock, MOI.ListOfVariableIndices()), var_names)
            norminf = MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.NormInfinityCone}())
            @test length(norminf) == 1
            MOI.set(bridged_mock, MOI.ConstraintName(), norminf[1], "norminf")
            nonneg = MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives}())
            @test length(nonneg) == 1
            MOI.set(bridged_mock, MOI.ConstraintName(), nonneg[1], "nonneg")

            s = """
            variables: x, y1, y2, y3
            norminf: [x + -1.0, y1 + 2.0, y2 + 2.0, y3 + 2.0] in MathOptInterface.NormInfinityCone(4)
            nonneg: [y1 + 1.0, y2 + 1.0, y3 + 1.0] in MathOptInterface.Nonnegatives(3)
            minobjective: x
            """
            model = MOIU.Model{Float64}()
            MOIU.loadfromstring!(model, s)
            MOIU.test_models_equal(bridged_mock, model, var_names, ["norminf", "nonneg"])
        end

        ci = first(MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.NormInfinityCone}()))
        nonneg = MOI.get(mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives}())

        @testset "$attr" for attr in [MOI.ConstraintPrimalStart(), MOI.ConstraintDualStart()]
            @test MOI.supports(bridged_mock, attr, typeof(ci))
            value = Float64[5, 1, -2, 3]
            MOI.set(bridged_mock, attr, ci, value)
            @test MOI.get(bridged_mock, attr, ci) ≈ value
            if attr isa MOI.ConstraintPrimalStart
                nonneg_value = Float64[4, 7, 2, 6, 3, 8]
            else
                nonneg_value = [-1, 11, -1, 5, -1, 17] / 6
            end
            @test MOI.get(mock, attr, nonneg[1]) ≈ nonneg_value
        end

        test_delete_bridge(bridged_mock, ci, 4, (
            (MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives, 1),))
    end
end

@testset "NormOne" begin
    bridged_mock = MOIB.Constraint.NormOne{Float64}(mock)

    MOIT.basic_constraint_tests(bridged_mock, config,
        include = [(F, MOI.NormOneCone) for F in [
            MOI.VectorOfVariables, MOI.VectorAffineFunction{Float64}, MOI.VectorQuadraticFunction{Float64}
        ]])

    @testset "norminf1test" begin
        mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock,
            [1.0, 0.5, 0.5, 0.5, 0.5],
            (MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives) => [[1.0, 1.0, 1.0, 0.0, 0.0]],
            (MOI.VectorAffineFunction{Float64}, MOI.Zeros) => [[-1], [0]])

        MOIT.normone1vtest(bridged_mock, config)
        MOIT.normone1ftest(bridged_mock, config)

        var_names = ["x", "y", "z"]
        MOI.set(bridged_mock, MOI.VariableName(), MOI.get(bridged_mock, MOI.ListOfVariableIndices()), var_names)

        nonneg = MOI.get(mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives}())
        u, v = MOI.get(mock, MOI.ListOfVariableIndices())[4:5]
        @testset "Test mock model" begin
            MOI.set(mock, MOI.VariableName(), u, "u")
            MOI.set(mock, MOI.VariableName(), v, "v")
            @test length(nonneg) == 1
            MOI.set(mock, MOI.ConstraintName(), nonneg[1], "nonneg")
            zeros = MOI.get(mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.Zeros}())
            @test length(zeros) == 2
            MOI.set(mock, MOI.ConstraintName(), zeros[1], "x_eq")
            MOI.set(mock, MOI.ConstraintName(), zeros[2], "y_eq")

            s = """
            variables: x, y, z, u, v
            nonneg: [x + -1.0u + -1.0v, u + -1.0y, v + -1.0z, u + y, v + z] in MathOptInterface.Nonnegatives(5)
            x_eq: [-1.0 + x] in MathOptInterface.Zeros(1)
            y_eq: [-0.5 + y] in MathOptInterface.Zeros(1)
            maxobjective: y + z
            """
            model = MOIU.Model{Float64}()
            MOIU.loadfromstring!(model, s)
            MOIU.test_models_equal(mock, model, [var_names; "u"; "v"], ["nonneg", "x_eq", "y_eq"])
        end

        @testset "Test bridged model" begin
            normone = MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.NormOneCone}())
            @test length(normone) == 1
            MOI.set(bridged_mock, MOI.ConstraintName(), normone[1], "normone")
            zeros = MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.Zeros}())
            @test length(zeros) == 2
            MOI.set(bridged_mock, MOI.ConstraintName(), zeros[1], "x_eq")
            MOI.set(bridged_mock, MOI.ConstraintName(), zeros[2], "y_eq")

            s = """
            variables: x, y, z
            normone: [1.0x, y, z] in MathOptInterface.NormOneCone(3)
            x_eq: [-1.0 + x] in MathOptInterface.Zeros(1)
            y_eq: [-0.5 + y] in MathOptInterface.Zeros(1)
            maxobjective: y + z
            """
            model = MOIU.Model{Float64}()
            MOIU.loadfromstring!(model, s)
            MOIU.test_models_equal(bridged_mock, model, var_names, ["normone", "x_eq", "y_eq"])
        end

        ci = first(MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.NormOneCone}()))

        @testset "$attr" for attr in [MOI.ConstraintPrimalStart(), MOI.ConstraintDualStart()]
            @test MOI.supports(bridged_mock, attr, typeof(ci))
            value = [4.0, 1.0, -2.0]
            MOI.set(bridged_mock, attr, ci, value)
            @test MOI.get(bridged_mock, attr, ci) ≈ value
            if attr isa MOI.ConstraintPrimalStart
                @test MOI.get(mock, MOI.VariablePrimalStart(), u) == 1
                @test MOI.get(mock, MOI.VariablePrimalStart(), v) == 2
                @test MOI.get(mock, attr, nonneg[1]) == Float64[1, 0, 4, 2, 0]
            else
                @test MOI.get(mock, attr, nonneg[1]) == Float64[4, 0, 2, 1, 0]
            end
        end

        test_delete_bridge(bridged_mock, ci, 3, (
            (MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives, 0),))
    end

    @testset "normone3test" begin
        mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock,
            [4, -1, -1, -1, 1, 1, 1],
            (MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives) => [vcat(ones(4), zeros(3)), ones(3)])
        MOIT.normone3test(bridged_mock, config)

        var_names = ["x", "y1", "y2", "y3"]

        @testset "Test mock model" begin
            var_names_all = vcat(var_names, "z1", "z2", "z3")
            MOI.set(mock, MOI.VariableName(), MOI.get(mock, MOI.ListOfVariableIndices()), var_names_all)
            nonneg = MOI.get(mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives}())
            @test length(nonneg) == 2
            MOI.set(mock, MOI.ConstraintName(), nonneg[1], "nonneg1")
            MOI.set(mock, MOI.ConstraintName(), nonneg[2], "nonneg2")

            s = """
            variables: x, y1, y2, y3, z1, z2, z3
            nonneg1: [x + -1.0 + -1.0z1 + -1.0z2 + -1.0z3, z1 + -1.0y1 + -2.0, z2 + -1.0y2 + -2.0, z3 + -1.0y3 + -2.0, z1 + y1 + 2.0, z2 + y2 + 2.0, z3 + y3 + 2.0] in MathOptInterface.Nonnegatives(7)
            nonneg2: [y1 + 1.0, y2 + 1.0, y3 + 1.0] in MathOptInterface.Nonnegatives(3)
            minobjective: x
            """
            model = MOIU.Model{Float64}()
            MOIU.loadfromstring!(model, s)
            MOIU.test_models_equal(mock, model, var_names_all, ["nonneg1", "nonneg2"])
        end

        @testset "Test bridged model" begin
            MOI.set(bridged_mock, MOI.VariableName(), MOI.get(bridged_mock, MOI.ListOfVariableIndices()), var_names)
            normone = MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.NormOneCone}())
            @test length(normone) == 1
            MOI.set(bridged_mock, MOI.ConstraintName(), normone[1], "normone")
            nonneg = MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives}())
            @test length(nonneg) == 1
            MOI.set(bridged_mock, MOI.ConstraintName(), nonneg[1], "nonneg")

            s = """
            variables: x, y1, y2, y3
            normone: [x + -1.0, y1 + 2.0, y2 + 2.0, y3 + 2.0] in MathOptInterface.NormOneCone(4)
            nonneg: [y1 + 1.0, y2 + 1.0, y3 + 1.0] in MathOptInterface.Nonnegatives(3)
            minobjective: x
            """
            model = MOIU.Model{Float64}()
            MOIU.loadfromstring!(model, s)
            MOIU.test_models_equal(bridged_mock, model, var_names, ["normone", "nonneg"])
        end

        ci = first(MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.NormOneCone}()))
        nonneg = MOI.get(mock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives}())

        @testset "$attr" for attr in [MOI.ConstraintPrimalStart(), MOI.ConstraintDualStart()]
            @test MOI.supports(bridged_mock, attr, typeof(ci))
            value = (attr isa MOI.ConstraintPrimalStart ? vcat(3, ones(3)) : vcat(1, fill(-1, 3)))
            MOI.set(bridged_mock, attr, ci, value)
            @test MOI.get(bridged_mock, attr, ci) ≈ value
            nonneg_value = (attr isa MOI.ConstraintPrimalStart ? vcat(zeros(4), fill(2.0, 3)) : vcat(ones(4), zeros(3)))
            @test MOI.get(mock, attr, nonneg[1]) ≈ nonneg_value
        end

        test_delete_bridge(bridged_mock, ci, 4, (
            (MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives, 1),))
    end
end
