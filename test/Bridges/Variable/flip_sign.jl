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
    end

    @testset "Test Mock model" begin
        var_names = ["a", "b", "c", "d"]
        MOI.set(mock, MOI.VariableName(), MOI.get(mock, MOI.ListOfVariableIndices()), var_names)

        con_d = MOI.get(mock, MOI.ListOfConstraintIndices{MathOptInterface.VectorOfVariables, MathOptInterface.Zeros}())[1]
        con_bc = MOI.get(mock, MOI.ListOfConstraintIndices{MathOptInterface.VectorOfVariables, MathOptInterface.Nonnegatives}())
        con_ex = MOI.get(mock, MOI.ListOfConstraintIndices{MathOptInterface.VectorAffineFunction{Float64}, MathOptInterface.Zeros}())[1]

        MOI.set(mock, MOI.ConstraintName(), con_d, "cd")
        MOI.set(mock, MOI.ConstraintName(), con_bc[1], "cb")
        MOI.set(mock, MOI.ConstraintName(), con_bc[2], "cc")
        MOI.set(mock, MOI.ConstraintName(), con_ex, "cex")

        s = """
        variables: a, b, c, d
        cd: [d] in MathOptInterface.Zeros(1)
        cb: [b] in MathOptInterface.Nonnegatives(1)
        cc: [c] in MathOptInterface.Nonnegatives(1)
        cex: [1*a + -1*d + 4.0, -1*b + 3.0, 1*a + 1*c + -12.0] in MathOptInterface.Zeros(3)
        minobjective: 3*a + -2*b + -4*c
        """
        model = MOIU.Model{Float64}()
        MOIU.loadfromstring!(model, s)
        MOIU.test_models_equal(mock, model, var_names, ["cd", "cb", "cc", "cex"])
    end

    @testset "Test Bridged model" begin
        var_names = ["x", "y", "z", "w"]
        MOI.set(bridged_mock, MOI.VariableName(), MOI.get(bridged_mock, MOI.ListOfVariableIndices()), var_names)

        con_w = MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MathOptInterface.VectorOfVariables, MathOptInterface.Nonpositives}())[1]
        con_z = MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MathOptInterface.VectorOfVariables, MathOptInterface.Zeros}())[1]
        con_y = MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MathOptInterface.VectorOfVariables, MathOptInterface.Nonnegatives}())[1]
        con_ex = MOI.get(bridged_mock, MOI.ListOfConstraintIndices{MathOptInterface.VectorAffineFunction{Float64}, MathOptInterface.Zeros}())[1]

        MOI.set(bridged_mock, MOI.ConstraintName(), con_w, "cw")
        MOI.set(bridged_mock, MOI.ConstraintName(), con_z, "cz")
        MOI.set(bridged_mock, MOI.ConstraintName(), con_y, "cy")
        MOI.set(bridged_mock, MOI.ConstraintName(), con_ex, "cex")

        s = """
        variables: x, y, z, w
        cw: [w] in MathOptInterface.Nonpositives(1)
        cz: [z] in MathOptInterface.Zeros(1)
        cy: [y] in MathOptInterface.Nonnegatives(1)
        cex: [1*x + -1*z + 4.0, 1*w + 3.0, 1*x + 1*y + -12.0] in MathOptInterface.Zeros(3)
        minobjective: 3*x + 2*w + -4*y
        """
        model = MOIU.Model{Float64}()
        MOIU.loadfromstring!(model, s)
        MOIU.test_models_equal(bridged_mock, model, var_names, ["cw", "cz", "cy", "cex"])
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
