using Test

using MathOptInterface
const MOI = MathOptInterface
const MOIT = MathOptInterface.Test
const MOIU = MathOptInterface.Utilities
const MOIB = MathOptInterface.Bridges

include("../utilities.jl")

mock = MOIU.MockOptimizer(MOIU.UniversalFallback(MOIU.Model{Float64}()))
config = MOIT.TestConfig()

@testset "NonposToNonneg" begin
    bridged_mock = MOIB.Variable.NonposToNonneg{Float64}(mock)

    @testset "lin2v" begin
        mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [-4, 3, 16, 0],
            (MOI.VectorAffineFunction{Float64}, MOI.Zeros) => [[7, 2, -4]])
        MOIT.lin2vtest(bridged_mock, config)

        @test MOI.get(mock, MOI.NumberOfVariables()) == 4
        @test MOI.get(bridged_mock, MOI.NumberOfVariables()) == 4
        vis = MOI.get(bridged_mock, MOI.ListOfVariableIndices())
        y = vis[4]
        @test y.value == -1

        @test MOI.supports(bridged_mock, MOI.VariablePrimalStart(), MOI.VariableIndex)
        @test MOI.supports(bridged_mock, MOI.VariablePrimalStart(), typeof(MOIB.bridge(bridged_mock, y)))
        MOI.set(bridged_mock, MOI.VariablePrimalStart(), y, 1.0)
        x, y_flipped, z, s = MOI.get(mock, MOI.ListOfVariableIndices())
        @test MOI.get(mock, MOI.VariablePrimalStart(), y_flipped) == -1
        @test MOI.get(bridged_mock, MOI.VariablePrimalStart(), y) == 1

        var_names = ["x", "y", "z", "w"]
        MOI.set(mock, MOI.VariableName(), MOI.get(mock, MOI.ListOfVariableIndices()), var_names)
        con_w = MOI.get(mock, MOI.ListOfConstraintIndices{MathOptInterface.VectorOfVariables, MathOptInterface.Zeros}())[1]
        con_yz = MOI.get(mock, MOI.ListOfConstraintIndices{MathOptInterface.VectorOfVariables, MathOptInterface.Nonnegatives}())
        con_ex = MOI.get(mock, MOI.ListOfConstraintIndices{MathOptInterface.VectorAffineFunction{Float64}, MathOptInterface.Zeros}())[1]

        MOI.set(mock, MOI.ConstraintName(), con_w, "cw")
        MOI.set(mock, MOI.ConstraintName(), con_yz[1], "cy")
        MOI.set(mock, MOI.ConstraintName(), con_yz[2], "cz")
        MOI.set(mock, MOI.ConstraintName(), con_ex, "cex")

        MOI.set(bridged_mock, MOI.VariableName(), MOI.get(bridged_mock, MOI.ListOfVariableIndices())[4], "v")
        con_v = MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MathOptInterface.VectorOfVariables, MathOptInterface.Nonpositives}())[1]
        MOI.set(bridged_mock, MOI.ConstraintName(), con_v, "cv")

        @testset "Test Mock model" begin
            s = """
            variables: x, y, z, w
            cw: [w] in MathOptInterface.Zeros(1)
            cy: [y] in MathOptInterface.Nonnegatives(1)
            cz: [z] in MathOptInterface.Nonnegatives(1)
            cex: [1*x + -1*w + 4.0, -1*y + 3.0, 1*x + 1*z + -12.0] in MathOptInterface.Zeros(3)
            minobjective: 3*x + -2*y + -4*z
            """
            model = MOIU.Model{Float64}()
            MOIU.loadfromstring!(model, s)
            MOIU.test_models_equal(mock, model, var_names, ["cw", "cy", "cz", "cex"])
        end

        @testset "Test Bridged model" begin
            s = """
            variables: x, z, w, v
            cv: [v] in MathOptInterface.Nonpositives(1)
            cw: [w] in MathOptInterface.Zeros(1)
            cz: [z] in MathOptInterface.Nonnegatives(1)
            cex: [1*x + -1*w + 4.0, 1*v + 3.0, 1*x + 1*z + -12.0] in MathOptInterface.Zeros(3)
            minobjective: 3*x + 2*v + -4*z
            """
            model = MOIU.Model{Float64}()
            MOIU.loadfromstring!(model, s)
            MOIU.test_models_equal(bridged_mock, model, ["x", "z", "w", "v"], ["cv", "cw", "cz", "cex"])
        end
    end

    @testset "lin4" begin
        MOIU.set_mock_optimize!(mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
                mock, MOI.INFEASIBLE, MOI.INFEASIBLE_POINT,
                MOI.INFEASIBILITY_CERTIFICATE)
        )
        MOIT.lin4test(bridged_mock, config)

        @test MOI.get(mock, MOI.NumberOfVariables()) == 1
        @test length(MOI.get(mock, MOI.ListOfVariableIndices())) == 1
        @test first(MOI.get(mock, MOI.ListOfVariableIndices())).value ≥ 0
        @test MOI.get(bridged_mock, MOI.NumberOfVariables()) == 1
        vis = MOI.get(bridged_mock, MOI.ListOfVariableIndices())
        @test vis == [MOI.VariableIndex(-1)]
        test_delete_bridged_variable(bridged_mock, vis[1], MOI.Nonpositives, 1, (
            (MOI.VectorOfVariables, MOI.Nonnegatives, 0),
        ))
    end

    @testset "Delete in vector" begin
        MOI.empty!(bridged_mock)
        vis, ci = MOI.add_constrained_variables(bridged_mock, MOI.Nonpositives(4))
        test_delete_bridged_variable(bridged_mock, vis[2], MOI.Nonpositives, 4, (
            (MOI.VectorOfVariables, MOI.Nonnegatives, 0),
        ), used_bridges = 0, used_constraints = 0)
    end
end
